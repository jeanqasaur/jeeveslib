package cap.scalasmt

/** 
 * Zero values for types.
 * @author kuat
 */
trait Zero[Z] {
  val zero: Z
}
object Zeros {
  private def z[Z](z: Z): Zero[Z] = new Zero[Z] {val zero = z}
  def zero[Z](implicit z: Zero[Z]): Z = z.zero

  implicit def BigIntZero: Zero[BigInt] = z(0)
  implicit def SetZero[T]: Zero[Set[T]] = z(Set())
  implicit def BooleanZero: Zero[Boolean] = z(false)
  implicit def AtomZero: Zero[Atom] = z(null)
}


