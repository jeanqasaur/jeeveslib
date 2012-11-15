package cap.scalasmt

/** 
 * VarEnv.
 */
sealed trait VarEnv {
  type Defaults = List[Formula]
  type Constraints = List[Formula]

  def vars: Set[Var[_]]
  def +[T](b: (Var[T], T)): VarEnv = Binding(b._1, b._2, this)
  def has[T](i: Var[T]): Boolean
  def apply[T](i: Var[T]): T
  def hasAll(vs: Traversable[Var[_]]) = vs.forall(has(_))
}
class UnboundVarException(i: Var[_]) extends RuntimeException("unbound variable: " + i)
object EmptyEnv extends VarEnv {
  def vars = Set()
  def has[T](i: Var[T]) = false
  def apply[T](i: Var[T]) = throw new UnboundVarException(i)
}
object DefaultEnv extends VarEnv {
  def vars = Set()
  def has[T](i: Var[T]) = false
  def apply[T](i: Var[T]) = i.default
}
case class Binding[U](bi: Var[U], bv: U, parent: VarEnv) extends VarEnv {
  def vars = parent.vars + bi
  def has[T](i: Var[T]) = (i == bi) || parent.has(i)
  def apply[T](i: Var[T]) = if (i == bi) bv.asInstanceOf[T] else parent(i)
}
