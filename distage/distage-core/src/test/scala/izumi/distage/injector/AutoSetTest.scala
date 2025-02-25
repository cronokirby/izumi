package izumi.distage.injector

import org.scalatest.WordSpec
import izumi.distage.fixtures.SetCases._
import izumi.distage.model.PlannerInput
import izumi.distage.model.planning.PlanningHook
import izumi.distage.planning.AssignableFromAutoSetHook
import distage.{BootstrapModuleDef, Injector, ModuleDef}
import izumi.distage.fixtures.SetCases.SetCase3.{ServiceA, ServiceB, ServiceC, ServiceD}

class AutoSetTest extends WordSpec with MkInjector {

  "AutoSets preserve dependency order" in {
    import SetCase3._

    val definition = new ModuleDef {
      make[ServiceA]
      make[ServiceB]
      make[ServiceC]
      make[ServiceD]
    }

    val injector = Injector.Standard(new BootstrapModuleDef {
      many[AutoCloseable]
      many[PlanningHook]
        .add(new AssignableFromAutoSetHook[Ordered, Ordered](identity))
    })

    val autoset = injector.produceUnsafe(PlannerInput.noGc(definition)).get[Set[Ordered]]

    assert(autoset.toSeq == autoset.toSeq.sortBy(_.order))
  }

  "AutoSets collect instances with the same type but different implementations" in {
    val definition = new ModuleDef {
      make[Int].fromValue(1)
      make[Int].named("x").fromValue(2)
      many[Int].named("nonauto")
        .addValue(3)
        .addValue(4)
        .addValue(5)
    }

    val injector = Injector.Standard(new BootstrapModuleDef {
      many[AutoCloseable]
      many[PlanningHook]
        .add(new AssignableFromAutoSetHook[Int, Int](identity))
    })

    val autoset = injector.produceUnsafe(PlannerInput.noGc(definition)).get[Set[Int]]

    assert(autoset == Set(1, 2, 3, 4, 5))
  }

}
