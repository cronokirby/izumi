package org.scalatest

import distage.{ModuleBase, TagK}
import izumi.distage.model.definition.BootstrapModule
import izumi.distage.roles.config.ContextOptions
import izumi.distage.roles.services.IntegrationChecker
import izumi.distage.testkit.services.dstest.DistageTestRunner._
import izumi.distage.testkit.services.dstest.{AbstractDistageSpec, DistageTestRunner, SpecEnvironment, SpecEnvironmentImpl}
import izumi.distage.testkit.services.st.dtest.DistageTestsRegistrySingleton
import izumi.fundamentals.platform.language.Quirks
import izumi.logstage.api.{IzLogger, Log}
import org.scalatest.events._

import scala.collection.immutable.TreeSet

final case class SpecConfig(
                             contextOptions: ContextOptions = ContextOptions(),
                             bootstrapOverrides: BootstrapModule = BootstrapModule.empty,
                             moduleOverrides: ModuleBase = ModuleBase.empty,
                             bootstrapLogLevel: Log.Level = Log.Level.Info,
                           )

trait DistageScalatestTestSuiteRunner[F[_]] extends Suite with AbstractDistageSpec[F] {
  implicit def tagMonoIO: TagK[F]

  private[this] lazy val specEnv: SpecEnvironment[F] = makeSpecEnvironment()

  protected def makeSpecConfig(): SpecConfig = SpecConfig()

  protected def makeSpecEnvironment(): SpecEnvironment[F] = {
    val c = makeSpecConfig()

    new SpecEnvironmentImpl[F](
      this.getClass,
      c.contextOptions,
      c.bootstrapOverrides,
      c.moduleOverrides,
      c.bootstrapLogLevel,
    )
  }


  override final protected def runNestedSuites(args: Args): Status =
    throw new UnsupportedOperationException
  override protected final def runTests(testName: Option[String], args: Args): Status =
    throw new UnsupportedOperationException
  override protected final def runTest(testName: String, args: Args): Status =
    throw new UnsupportedOperationException

  override def testNames: Set[String] = {
    TreeSet[String](thisTests.map(_.meta.id.name): _*)
  }
  override def tags: Map[String, Set[String]] = Map.empty

  override def expectedTestCount(filter: Filter): Int = {
    if (filter.tagsToInclude.isDefined) {
      0
    } else {
      testNames.size - tags.size
    }
  }

  private def thisTests: Seq[DistageTest[F]] = {
    monadTests.filter(_.meta.id.suiteId == suiteId)
  }

  private def monadTests: Seq[DistageTest[F]] = {
    DistageTestsRegistrySingleton.list[F]
  }

  override def run(testName: Option[String], args: Args): Status = {
    val status = new StatefulStatus
    val tracker = args.tracker

    try {
      if (DistageTestsRegistrySingleton.ticketToProceed[F]()) {
        doRun(tracker, testName, args)
      } else {
        addStub(args, tracker, None)
      }
    } catch {
      case t: Throwable =>
        // IDEA reporter is insane
        //status.setFailedWith(t)
        addStub(args, tracker, Some(t))

    } finally {
      status.setCompleted()
    }

    status
  }

  override def testDataFor(testName: String, theConfigMap: ConfigMap): TestData = {
    val suiteTags = for {
      a <- this.getClass.getAnnotations
      annotationClass = a.annotationType
      if annotationClass.isAnnotationPresent(classOf[TagAnnotation])
    } yield {
      annotationClass.getName
    }

    val testTags: Set[String] = Set.empty

    new TestData {
      val configMap: ConfigMap = theConfigMap
      val name: String = testName
      val scopes: Vector[Nothing] = Vector.empty
      val text: String = testName
      val tags: Set[String] = Set.empty ++ suiteTags ++ testTags
      val pos: None.type = None
    }
  }

  private def doRun(tracker: Tracker, testName: Option[String], args: Args): Unit = {

    val dreporter = mkTestReporter(args, tracker)

    val toRun = testName match {
      case None =>
        val enabled = args.filter.dynaTags.testTags.toSeq
          .flatMap {
            case (suiteId, tests) =>

              tests.filter(_._2.contains(Suite.SELECTED_TAG)).keys
                .map {
                  testname =>
                    (suiteId, testname)
                }
          }
          .toSet

        if (enabled.isEmpty) {
          monadTests
        } else {
          monadTests.filter(t => enabled.contains((t.meta.id.suiteId, t.meta.id.name)))
        }
      case Some(tn) =>
        if (!testNames.contains(tn)) {
          throw new IllegalArgumentException(Resources.testNotFound(testName))
        } else {
          monadTests.filter(_.meta.id.name == tn)
        }
    }

    val runner = {
      val logger = IzLogger(Log.Level.Debug)("phase" -> "test")
      val checker = new IntegrationChecker.Impl(logger)
      new DistageTestRunner[F](dreporter, checker, specEnv, toRun)
    }

    runner.run()
  }

  private def mkTestReporter(args: Args, tracker: Tracker): TestReporter = {
    def ord(testId: TestMeta) = {
      Quirks.discard(testId)
      tracker.nextOrdinal()
    }

    def recordStart(test: TestMeta): Unit = {
      args.reporter.apply(TestStarting(
        ord(test),
        suiteName, suiteId, Some(suiteId),
        test.id.name,
        test.id.name,
        location = Some(LineInFile(test.pos.position.line, test.pos.position.file, None)),
      ))
    }

    new TestReporter {
      override def beginSuite(id: SuiteData): Unit = {
        args.reporter.apply(TestStarting(
          tracker.nextOrdinal(),
          suiteName, suiteId, Some(suiteId),
          id.suiteName,
          id.suiteName,
        ))
      }

      override def endSuite(id: SuiteData): Unit = {
        args.reporter.apply(TestSucceeded(
          tracker.nextOrdinal(),
          suiteName, suiteId, Some(suiteId),
          id.suiteName,
          id.suiteName,
          scala.collection.immutable.IndexedSeq.empty[RecordableEvent],
        ))
      }

      override def testStatus(test: TestMeta, testStatus: TestStatus): Unit = {
        testStatus match {
          case TestStatus.Scheduled =>

          case TestStatus.Running =>
            recordStart(test)

          case TestStatus.Succeed(duration) =>
            args.reporter.apply(TestSucceeded(
              ord(test),
              suiteName, suiteId, Some(suiteId),
              test.id.name,
              test.id.name,
              scala.collection.immutable.IndexedSeq.empty[RecordableEvent],
              location = Some(LineInFile(test.pos.position.line, test.pos.position.file, None)),
              duration = Some(duration.toMillis),
              rerunner = Some(test.id.suiteClassName),
            ))
          case TestStatus.Failed(t, duration) =>
            args.reporter.apply(TestFailed(
              ord(test),
              "Test failed",
              suiteName, suiteId, Some(suiteId),
              test.id.name,
              test.id.name,
              scala.collection.immutable.IndexedSeq.empty[RecordableEvent],
              location = Some(LineInFile(test.pos.position.line, test.pos.position.file, None)),
              throwable = Some(t),
              duration = Some(duration.toMillis),
              rerunner = Some(test.id.suiteClassName),
            ))
          case TestStatus.Cancelled(checks) =>
            recordStart(test)
            import izumi.fundamentals.platform.strings.IzString._
            args.reporter.apply(TestCanceled(
              ord(test),
              s"ignored: ${checks.niceList()}",
              suiteName, suiteId, Some(suiteId),
              test.id.name,
              test.id.name,
              scala.collection.immutable.IndexedSeq.empty[RecordableEvent],
              location = Some(LineInFile(test.pos.position.line, test.pos.position.file, None)),
              rerunner = Some(test.id.suiteClassName),
            ))
        }
      }
    }
  }

  private def addStub(args: Args, tracker: Tracker, failure: Option[Throwable]): Unit = {
    val FUCK_SCALATEST = "Scalatest is not good for your mental health"
    val SUITE_FAILED = "Whole suite failed :/"

    failure match {
      case Some(_) =>
        args.reporter(TestStarting(
          tracker.nextOrdinal(),
          suiteName, suiteId, Some(suiteId),
          SUITE_FAILED,
          SUITE_FAILED,
        ))
        args.reporter(TestFailed(
          tracker.nextOrdinal(),
          s"suite failed",
          suiteName, suiteId, Some(suiteId),
          SUITE_FAILED,
          SUITE_FAILED,
          scala.collection.immutable.IndexedSeq.empty[RecordableEvent],
          throwable = failure
        ))

      case None =>
        args.reporter(TestStarting(
          tracker.nextOrdinal(),
          suiteName, suiteId, Some(suiteId),
          FUCK_SCALATEST,
          FUCK_SCALATEST,
        ))
        args.reporter(TestCanceled(
          tracker.nextOrdinal(),
          s"ignored",
          suiteName, suiteId, Some(suiteId),
          FUCK_SCALATEST,
          FUCK_SCALATEST,
          scala.collection.immutable.IndexedSeq.empty[RecordableEvent],
        ))

    }

  }

  final override val styleName: String = "DistageSuite"
}
