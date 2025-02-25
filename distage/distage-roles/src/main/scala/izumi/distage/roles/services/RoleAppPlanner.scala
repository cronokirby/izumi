package izumi.distage.roles.services

import distage.{BootstrapModule, DIKey, Injector, TagK, _}
import izumi.distage.model.definition.{ModuleBase, ModuleDef}
import izumi.distage.model.monadic.{DIEffect, DIEffectRunner}
import izumi.distage.model.plan.OrderedPlan
import izumi.distage.roles.config.ContextOptions
import izumi.distage.roles.model.{AppActivation, IntegrationCheck}
import izumi.distage.roles.services.RoleAppPlanner.AppStartupPlans
import izumi.logstage.api.IzLogger

trait RoleAppPlanner[F[_]] {
  def reboot(bsModule: BootstrapModule): RoleAppPlanner[F]
  def makePlan(appMainRoots: Set[DIKey], appModule: ModuleBase): AppStartupPlans
}

object RoleAppPlanner {
  case class AppStartupPlans(
                              runtime: OrderedPlan,
                              integration: OrderedPlan,
                              integrationKeys: Set[DIKey],
                              app: OrderedPlan,
                              injector: Injector,
                            )

  class Impl[F[_] : TagK](
                           options: ContextOptions,
                           bsModule: BootstrapModule,
                           activation: AppActivation,
                           logger: IzLogger,
                         ) extends RoleAppPlanner[F] { self =>

    private val injector = Injector.Standard(bsModule)

    override def reboot(bsOverride: BootstrapModule): RoleAppPlanner[F] = {
      new RoleAppPlanner.Impl[F](options, bsModule overridenBy bsOverride, activation, logger)
    }

    override def makePlan(appMainRoots: Set[DIKey], appModule: ModuleBase): AppStartupPlans = {
      val fullAppModule = appModule
        .overridenBy(new ModuleDef {
          make[RoleAppPlanner[F]].from(self)
          make[ContextOptions].from(options)
          make[ModuleBase].named("application.module").from(appModule)
        })

      val runtimeGcRoots: Set[DIKey] = Set(
        DIKey.get[DIEffectRunner[F]],
        DIKey.get[DIEffect[F]],
      )
      val runtimePlan = injector.plan(PlannerInput(fullAppModule, runtimeGcRoots))

      val appPlan = injector.splitPlan(fullAppModule.drop(runtimeGcRoots), appMainRoots) {
        _.collectChildren[IntegrationCheck].map(_.target).toSet
      }

      val check = new PlanCircularDependencyCheck(options, logger)
      check.verify(runtimePlan)
      check.verify(appPlan.subplan)
      check.verify(appPlan.primary)

      AppStartupPlans(
        runtimePlan,
        appPlan.subplan,
        appPlan.subRoots,
        appPlan.primary,
        injector
      )
    }
  }

}
