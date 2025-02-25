package izumi.distage.injector

import distage._
import izumi.distage.fixtures.TraitCases._
import izumi.distage.fixtures.TypesCases._
import izumi.distage.model.PlannerInput
import izumi.distage.model.exceptions.UnsupportedWiringException
import org.scalatest.WordSpec

import scala.language.reflectiveCalls

class AdvancedTypesTest extends WordSpec with MkInjector {

  "support generics" in {
    import TypesCase1._

    val definition = PlannerInput.noGc(new ModuleDef {
      make[List[Dep]].named("As").from(List(DepA()))
      make[List[Dep]].named("Bs").from(List(DepB()))
      make[List[DepA]].from(List(DepA(), DepA(), DepA()))
      make[TestClass[DepA]]
    })

    val injector = mkInjector()
    val plan = injector.plan(definition)
    val context = injector.produceUnsafe(plan)

    assert(context.get[List[Dep]]("As").forall(_.isInstanceOf[DepA]))
    assert(context.get[List[DepA]].forall(_.isInstanceOf[DepA]))
    assert(context.get[List[Dep]]("Bs").forall(_.isInstanceOf[DepB]))
    assert(context.get[TestClass[DepA]].inner == context.get[List[DepA]])
  }

  "support classes with typealiases" in {
    import TypesCase1._

    val definition = PlannerInput.noGc(new ModuleDef {
      make[DepA]
      make[TestClass2[TypeAliasDepA]]
    })

    val injector = mkInjector()
    val plan = injector.plan(definition)
    val context = injector.produceUnsafe(plan)

    assert(context.get[TestClass2[TypeAliasDepA]].inner.isInstanceOf[TypeAliasDepA])
  }

  "support traits with typealiases" in {
    import TypesCase1._

    val definition = PlannerInput.noGc(new ModuleDef {
      make[DepA]
      make[TestTrait]
    })

    val injector = mkInjector()
    val plan = injector.plan(definition)
    val context = injector.produceUnsafe(plan)

    assert(context.get[TestTrait].dep.isInstanceOf[TypeAliasDepA])
  }

  "type annotations in di keys do not result in different keys" in {
    import TraitCase2._

    val definition = PlannerInput.noGc(new ModuleDef {
      make[Dependency1@Id("special")]
      make[Trait1]
    })

    val injector = mkInjector()
    val plan = injector.plan(definition)
    val context = injector.produceUnsafe(plan)

    val instantiated = context.get[Dependency1]
    val instantiated1 = context.get[Dependency1@Id("special")]

    assert(instantiated eq instantiated1)
  }

  "handle `with` types" in {
    import TypesCase3._

    val definition = PlannerInput.noGc(new ModuleDef {
      make[Dep]
      make[Dep2]
      make[Trait2 with Trait1].from[Trait6]
    })

    val injector = mkInjector()
    val plan = injector.plan(definition)
    val context = injector.produceUnsafe(plan)

    val instantiated = context.get[Trait2 with Trait1]

    assert(instantiated.dep == context.get[Dep])
  }

  "light type tags can handle refinement & structural types" in {
    import TypesCase3._

    val definition = PlannerInput.noGc(new ModuleDef {
      make[Dep]
      make[Dep2]
      make[Trait1 {def dep: Dep2}].from[Trait3[Dep2]]
      make[Trait1 {def dep: Dep}].from[Trait3[Dep]]
      make[ {def dep: Dep}].from[Trait6]
    })

    val injector = mkInjector()
    val plan = injector.plan(definition)
    val context = injector.produceUnsafe(plan)

    val instantiated1 = context.get[Trait1 {def dep: Dep2}]
    val instantiated2 = context.get[ {def dep: Dep}]
    val instantiated3 = context.get[Trait1 {def dep: Dep}]

    assert(instantiated1.dep == context.get[Dep2])
    assert(instantiated2.dep == context.get[Dep])
    assert(instantiated3.dep == context.get[Dep])
  }

  "handle function local type aliases" in {
    import TypesCase4._

    class Definition[T: Tag] extends ModuleDef {
      make[Dep]
      make[Dep2]
      val _ = {
        type X[A] = Trait1[Dep, A]
        make[X[T]]
      }
    }

    val injector = mkInjector()
    val plan = injector.plan(PlannerInput.noGc(new Definition[Dep2]))
    val context = injector.produceUnsafe(plan)

    val instantiated = context.get[Trait1[Dep, Dep2]]

    assert(instantiated.a == context.get[Dep])
    assert(instantiated.b == context.get[Dep2])
  }

  "light type tags can handle abstract structural refinement types" in {
    import TypesCase3._

    class Definition[T >: Null : Tag, G <: T {def dep : Dep} : Tag] extends ModuleDef {
      make[Dep]
      make[T {def dep2: Dep}].from(() => null: T {def dep2: Dep})
      make[T {def dep: Dep}].from[G]
    }

    val definition = PlannerInput.noGc(new Definition[Trait1, Trait1])

    val injector = mkInjector()
    val plan = injector.plan(definition)
    val context = injector.produceUnsafe(plan)

    val instantiated = context.get[Trait1 {def dep: Dep}]

    assert(instantiated.dep == context.get[Dep])

  }

  "handle abstract `with` types" in {
    import TypesCase3._

    class Definition[T: Tag, G <: T with Trait1 : Tag, C <: T with Trait4 : Tag] extends ModuleDef {
      make[Dep]
      make[T with Trait4].from[C]
      make[T with Trait1].from[G]
    }

    val definition = PlannerInput.noGc(new Definition[Trait3[Dep], Trait3[Dep], Trait5[Dep]])

    val injector = mkInjector()
    val plan = injector.plan(definition)
    val context = injector.produceUnsafe(plan)

    val instantiated = context.get[Trait3[Dep] with Trait1]
    val instantiated2 = context.get[Trait3[Dep] with Trait4]

    assert(instantiated.dep == context.get[Dep])
    assert(instantiated.isInstanceOf[Trait1])
    assert(!instantiated.isInstanceOf[Trait4])

    assert(instantiated2.dep == context.get[Dep])
    assert(instantiated2.isInstanceOf[Trait1])
    assert(instantiated2.isInstanceOf[Trait4])
  }

  "handle generic parameters in abstract `with` types" in {
    import TypesCase3._

    class Definition[T <: Dep : Tag, K >: Trait5[T] : Tag] extends ModuleDef {
      make[T]
      make[Trait3[T] with K].from[Trait5[T]]
    }

    val definition = PlannerInput.noGc(new Definition[Dep, Trait4])

    val injector = mkInjector()
    val plan = injector.plan(definition)
    val context = injector.produceUnsafe(plan)

    val instantiated = context.get[Trait3[Dep] with Trait4]

    assert(instantiated.dep == context.get[Dep])
  }

  "support newtypes" in {
    import TypesCase5._

    val definition = PlannerInput.noGc(new ModuleDef {
      make[WidgetId].from(WidgetId(1))
      make[Dep]
    })

    val injector = mkInjector()
    val context = injector.produceUnsafe(definition)

    val instantiated1 = context.get[Dep]
    val instantiated2 = context.get[WidgetId]
    assert(instantiated1.widgetId == instantiated2)
  }

  "structural types are unsupported in class strategy" in {
    intercept[UnsupportedWiringException] {
      val definition = PlannerInput.noGc(new ModuleDef {
        make[ {def a: Int}]
        make[Int].from(5)
      })

      val injector = mkInjector()
      val context = injector.produceUnsafe(definition)

      val instantiated = context.get[ {def a: Int}]
      assert(instantiated.a == context.get[Int])
    }
  }

  "refinements are unsupported in class strategy" in {
    intercept[UnsupportedWiringException] {
      import TypesCase4._

      val definition = PlannerInput.noGc(new ModuleDef {
        make[Dep {}]
      })

      val injector = mkInjector()
      val context = injector.produceUnsafe(definition)

      assert(context.get[Dep {}] != null)
    }
  }

}
