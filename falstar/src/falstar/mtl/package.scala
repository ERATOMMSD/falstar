package falstar

import scala.language.implicitConversions

package object mtl {
  import falstar.hybrid.Time

  def ○(phi: Formula) = {
    Eventually(0.001, 0.1, phi)
  }

  def ◇(t0: Time, T: Time, phi: Formula) = {
    Eventually(t0, T, phi)
  }

  def □(t0: Time, T: Time, phi: Formula) = {
    Always(t0, T, phi)
  }

  implicit def toConst(const: Double): Term = Const(const)
  implicit def toConst(const: Int): Term = Const(const)

  implicit class TransformOps(f: Double => Double) {
    def apply(tm: Term) = Transform(tm, f)
  }
}
