package izumi.distage.roles

import cats.effect.LiftIO
import distage._
import izumi.distage.config.ConfigInjectionOptions
import izumi.distage.config.model.AppConfig
import izumi.distage.model.definition.Axis.AxisValue
import izumi.distage.model.definition.AxisBase
import izumi.distage.model.definition.StandardAxis.{Env, ExternalApi, Repo}
import izumi.distage.model.monadic.DIEffect
import izumi.distage.model.reflection.universe.MirrorProvider
import izumi.distage.plugins.merge.{PluginMergeStrategy, SimplePluginMergeStrategy}
import izumi.distage.roles.RoleAppLauncher.Options
import izumi.distage.roles.config.ContextOptions
import izumi.distage.roles.model.meta.{LibraryReference, RolesInfo}
import izumi.distage.roles.model.{AppActivation, DiAppBootstrapException}
import izumi.distage.roles.services.PluginSource.AllLoadedPlugins
import izumi.distage.roles.services.ResourceRewriter.RewriteRules
import izumi.distage.roles.services._
import izumi.functional.bio.{BIOAsync, BIOPrimitives}
import izumi.fundamentals.platform.cli.model.raw.RawAppArgs
import izumi.fundamentals.platform.cli.model.schema.ParserDef
import izumi.fundamentals.platform.functional.Identity
import izumi.fundamentals.platform.language.Quirks
import izumi.fundamentals.platform.resources.IzManifest
import izumi.fundamentals.platform.strings.IzString._
import izumi.logstage.api.IzLogger

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global
import scala.reflect.ClassTag

/**
  * Application flow:
  * 1. Parse commandline parameters
  * 2. Create "early logger" (console sink & configurable log level)
  * 3. Show startup banner
  * 4. Load raw config
  * 5. Create "late logger" using config
  * 6. Enumerate app plugins and bootstrap plugins
  * 7. Enumerate available roles, show role info and and apply merge strategy/conflict resolution
  * 8. Validate loaded roles (for non-emptyness and conflicts between bootstrap and app plugins)
  * 9. Build plan for DIEffect runner
  * 10. Build plan for integration checks
  * 11. Build plan for application
  * 12. Run roles
  * 13. Run services
  * 14. Await application termination
  * 15. Close autocloseables
  * 16. Shutdown executors
  */
abstract class RoleAppLauncher[F[_] : TagK : DIEffect] {

  private val loggers = new EarlyLoggers()

  protected def bootstrapConfig: BootstrapConfig

  protected val hook: AppShutdownStrategy[F]

  protected def referenceLibraryInfo: Seq[LibraryReference] = Vector.empty

  final def launch(parameters: RawAppArgs): Unit = {
    val earlyLogger = loggers.makeEarlyLogger(parameters)
    showBanner(earlyLogger, referenceLibraryInfo)

    val plugins = makePluginLoader(bootstrapConfig).load()
    val roles = loadRoles(parameters, earlyLogger, plugins)

    // default PlanMergingPolicy will be applied to bootstrap module, so any non-trivial conflict in bootstrap bindings will fail the app
    val defBs = makeBootstrapMergeStrategy(earlyLogger, parameters).merge(plugins.bootstrap)

    earlyLogger.info(s"Loaded ${defBs.bindings.size -> "bootstrap bindings"}...")

    val config = makeConfigLoader(earlyLogger, parameters).buildConfig()
    val lateLogger = loggers.makeLateLogger(parameters, earlyLogger, config)

    val mergeStrategy = makeMergeStrategy(lateLogger, parameters, roles)
    val defApp = mergeStrategy.merge(plugins.app)
    lateLogger.info(s"Loaded ${defApp.bindings.size -> "app bindings"}...")

    validate(defBs, defApp)

    val roots = gcRoots(roles)

    val activation = new ActivationParser().parseActivation(earlyLogger, parameters, defApp, defaultActivations, requiredActivations)

    val options = contextOptions(parameters)
    val moduleProvider = makeModuleProvider(options, parameters, activation, roles, config, lateLogger)
    val bsModule = moduleProvider.bootstrapModules().merge overridenBy defBs overridenBy bsOverride

    val appModule = moduleProvider.appModules().merge overridenBy defApp overridenBy appOverride
    val planner = makePlanner(options, bsModule, activation, lateLogger)
    val appPlan = planner.makePlan(roots, appModule)
    lateLogger.info(s"Planning finished. ${appPlan.app.keys.size -> "main ops"}, ${appPlan.integration.keys.size -> "integration ops"}, ${appPlan.runtime.keys.size -> "runtime ops"}")

    val r = makeExecutor(parameters, roles, lateLogger, appPlan.injector)
    r.runPlan(appPlan)
  }

  protected def appOverride: ModuleBase = ModuleBase.empty

  protected def bsOverride: BootstrapModule = BootstrapModule.empty

  protected def defaultActivations: Map[AxisBase, AxisValue] = {
    Map(
      Env -> Env.Prod,
      Repo -> Repo.Prod,
      ExternalApi -> ExternalApi.Prod,
    )
  }
  protected def requiredActivations: Map[AxisBase, AxisValue] = Map.empty

  protected def gcRoots(rolesInfo: RolesInfo): Set[DIKey] = {
    rolesInfo.requiredComponents
  }

  protected def makeBootstrapMergeStrategy(lateLogger: IzLogger, parameters: RawAppArgs): PluginMergeStrategy = {
    Quirks.discard(lateLogger, parameters)
    SimplePluginMergeStrategy
  }

  protected def makeMergeStrategy(lateLogger: IzLogger, parameters: RawAppArgs, roles: RolesInfo): PluginMergeStrategy = {
    Quirks.discard(lateLogger, parameters, roles)
    SimplePluginMergeStrategy
  }

  protected def makePlanner(options: ContextOptions, bsModule: BootstrapModule, activation: AppActivation, lateLogger: IzLogger): RoleAppPlanner[F] = {
    new RoleAppPlanner.Impl[F](options, bsModule, activation, lateLogger)
  }

  protected def makeExecutor(parameters: RawAppArgs, roles: RolesInfo, lateLogger: IzLogger, injector: Injector): RoleAppExecutor[F] = {
    new RoleAppExecutor.Impl[F](hook, roles, injector, lateLogger, parameters)
  }

  protected def makeModuleProvider(options: ContextOptions, parameters: RawAppArgs, activation: AppActivation, roles: RolesInfo, config: AppConfig, lateLogger: IzLogger): ModuleProvider[F] = {
    new ModuleProvider.Impl[F](
      lateLogger,
      config,
      roles,
      options,
      parameters,
      activation,
    )
  }

  protected def contextOptions(parameters: RawAppArgs): ContextOptions = {
    val dumpContext = RoleAppLauncher.Options.dumpContext.hasFlag(parameters.globalParameters)
    ContextOptions(
      dumpContext,
      warnOnCircularDeps = true,
      RewriteRules(),
      ConfigInjectionOptions(),
    )
  }

  protected def loadRoles(parameters: RawAppArgs, logger: IzLogger, plugins: AllLoadedPlugins): RolesInfo = {
    val activeRoleNames = parameters.roles.map(_.role).toSet
    val mp = MirrorProvider.Impl
    val roleProvider: RoleProvider[F] = new RoleProvider.Impl(logger, activeRoleNames, mp)
    val bindings = plugins.app.flatMap(_.bindings)
    val bsBindings = plugins.app.flatMap(_.bindings)
    logger.info(s"Available ${plugins.app.size -> "app plugins"} with ${bindings.size -> "app bindings"} and ${plugins.bootstrap.size -> "bootstrap plugins"} with ${bsBindings.size -> "bootstrap bindings"} ...")
    val roles = roleProvider.getInfo(bindings)

    printRoleInfo(logger, roles)
    val missing = parameters.roles.map(_.role).toSet.diff(roles.availableRoleBindings.map(_.descriptor.id).toSet)
    if (missing.nonEmpty) {
      logger.crit(s"Missing ${missing.niceList() -> "roles"}")
      throw new DiAppBootstrapException(s"Unknown roles: $missing")
    }

    roles
  }

  protected def printRoleInfo(logger: IzLogger, roles: RolesInfo): Unit = {
    val requestedNames = roles.requiredRoleBindings.map(_.descriptor.id)

    val availableRoleInfo = roles.availableRoleBindings.map {
      r =>
        val active = if (requestedNames.contains(r.descriptor.id)) {
          s"[+]"
        } else {
          s"[ ]"
        }
        s"$active ${r.descriptor.id}, ${r.binding.key}, source=${r.source.getOrElse("N/A")}"
    }.sorted

    logger.info(s"Available ${availableRoleInfo.niceList() -> "roles"}")
  }

  protected def showBanner(logger: IzLogger, referenceLibraries: Seq[LibraryReference]): this.type = {
    val withIzumi = referenceLibraries :+ LibraryReference("izumi-r2", classOf[ConfigLoader])
    showDepData(logger, "Application is about to start", this.getClass)
    withIzumi.foreach {
      u => showDepData(logger, s"... using ${u.libraryName}", u.clazz)
    }
    this
  }

  private def showDepData(logger: IzLogger, msg: String, clazz: Class[_]): Unit = {
    val mf = IzManifest.manifest()(ClassTag(clazz)).map(IzManifest.read)
    val details = mf.getOrElse("{No version data}")
    logger.info(s"$msg : $details")
  }

  protected def validate(bootstrapAutoDef: ModuleBase, appDef: ModuleBase): Unit = {
    val conflicts = bootstrapAutoDef.keys.intersect(appDef.keys)
    if (conflicts.nonEmpty) {
      throw new DiAppBootstrapException(s"Same keys defined by bootstrap and app plugins: $conflicts. Most likely your bootstrap configs are contradictive, terminating...")
    }

    if (appDef.bindings.isEmpty) {
      throw new DiAppBootstrapException("Empty app object graph. Most likely you have no plugins defined or your app plugin config is wrong, terminating...")
    }
  }

  protected def makePluginLoader(bootstrapConfig: BootstrapConfig): PluginSource = {
    new PluginSource.Impl(bootstrapConfig)
  }

  protected def makeConfigLoader(logger: IzLogger, parameters: RawAppArgs): ConfigLoader = {
    val maybeGlobalConfig = Options.configParam.findValue(parameters.globalParameters).asFile

    val roleConfigs = parameters.roles.map {
      r =>
        r.role -> Options.configParam.findValue(r.roleParameters).asFile
    }
    new ConfigLoader.LocalFSImpl(logger, maybeGlobalConfig, roleConfigs.toMap)
  }

}

object RoleAppLauncher {

  object Options extends ParserDef {
    final val logLevelRootParam = arg("log-level-root", "ll", "root log level", "{trace|debug|info|warn|error|critical}")
    final val logFormatParam = arg("log-format", "lf", "log format", "{hocon|json}")
    final val configParam = arg("config", "c", "path to config file", "<path>")
    final val dumpContext = flag("debug-dump-graph", "dump DI graph for debugging")
    final val use = arg("use", "u", "activate a choice on functionality axis", "<axis>:<choice>")
  }

  abstract class LauncherF[F[_] : TagK : DIEffect : LiftIO](executionContext: ExecutionContext = global) extends RoleAppLauncher[F] {
    override protected val hook: AppShutdownStrategy[F] = new CatsEffectIOShutdownStrategy(executionContext)
  }

  abstract class LauncherBIO[F[+_, +_]: BIOAsync: BIOPrimitives](implicit tagK: TagK[F[Throwable, ?]]) extends RoleAppLauncher[F[Throwable, ?]] {
    override protected val hook: AppShutdownStrategy[F[Throwable, ?]] = new BIOShutdownStrategy[F]
  }

  abstract class LauncherIdentity extends RoleAppLauncher[Identity] {
    override protected val hook: AppShutdownStrategy[Identity] = new JvmExitHookLatchShutdownStrategy
  }

}

