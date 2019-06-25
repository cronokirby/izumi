package com.github.pshirshov.izumi.distage.roles.services

import com.github.pshirshov.izumi.distage.model.monadic.DIEffect
import com.github.pshirshov.izumi.distage.model.provisioning.PlanInterpreter.FinalizersFilter
import com.github.pshirshov.izumi.distage.roles.services.IntegrationChecker.IntegrationCheckException
import com.github.pshirshov.izumi.distage.roles.services.RoleAppPlanner.AppStartupPlans
import com.github.pshirshov.izumi.fundamentals.platform.functional.Identity
import com.github.pshirshov.izumi.logstage.api.IzLogger
import distage.{Injector, Locator, TagK}

trait StartupPlanExecutor {
  def execute[F[_]: TagK, A](appPlan: AppStartupPlans, filters: StartupPlanExecutor.Filters[F])(doRun: (Locator, Option[IntegrationCheckException], DIEffect[F]) => F[A]): A
}

object StartupPlanExecutor {
  def default(logger: IzLogger, injector: Injector): StartupPlanExecutor = {
    val checker = new IntegrationCheckerImpl(logger)
    new StartupPlanExecutorImpl(injector, checker)
  }


  case class Filters[F[_]](
                            filterF: FinalizersFilter[F],
                            filterId: FinalizersFilter[Identity],
                          )

  object Filters {
    def all[F[_]]: Filters[F] = Filters[F](FinalizersFilter.all, FinalizersFilter.all)
  }
}
