package izumi.distage.testkit

import izumi.distage.model.reflection.universe.RuntimeDIUniverse.TagK
import izumi.distage.testkit.services.st.adapter.DistageTestSupport
import org.scalatest.{ScalatestSuite, WordSpecLike}

@deprecated("Use dstest", "2019/Jul/18")
abstract class DistageSpec[F[_] : TagK] extends DistageTestSupport[F] with WordSpecLike {
  override def toString: String = ScalatestSuite.suiteToString(None, this)
}



