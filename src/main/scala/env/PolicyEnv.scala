package cap.jeeveslib.env

/*
 * A library for using ScalaSMT for privacy, using symbolic varaibles to
 * represent sensitive values.
 * @author jeanyang
 */
import scala.collection.mutable.HashMap;
import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.eval.Partial
import cap.jeeveslib.util.Debug

trait PolicyEnv[C >: Null <: Atom] extends ConstraintEnv with PC {
  // Label variables for confidentiality and integrity
  sealed trait Label extends Serializable
  object HIGH extends Label
  object LOW extends Label
  implicit def level2sym(l: Label): Formula = l match {
    case HIGH => true
    case LOW => false
  }

  private val _policies
    : HashMap[LabelVar, (Label, ObjectExpr[C] => Formula)] =
    new HashMap()

  def mkLabel(): LabelVar = pickBool(_ => true, HIGH)

  def mkSensitiveInt(lvar: LabelVar, high: IntExpr, low: IntExpr = -1)
    : IntExpr = 
    lvar ? high ! low
    def mkSensitive[T >: Null <: Atom](
      lvar: LabelVar, high: ObjectExpr[T], low: ObjectExpr[T] = NULL)
    : ObjectExpr[T] = lvar ? high ! low
  def mkSensitiveIntFunction(lvar: LabelVar
    , high: FunctionExpr[IntExpr, IntExpr], low: FunctionExpr[IntExpr, IntExpr])
  : FunctionExpr[IntExpr, IntExpr] = lvar ? high ! low
  def mkSensitiveFunction(lvar: LabelVar
    , high: FunctionExpr[Atom, Atom], low: FunctionExpr[Atom, Atom])
  : FunctionExpr[Atom, Atom] = lvar ? high ! low

  /**
   * Confidentiality policies take the form restrict(a, f), where a is the level
   * variable and f is a formula that sets a to LOW if f is true.
   * 
   * We store policies as a weak hash map between the level variable and a pair
   * of the value (LOW/HIGH) and the policy.  If the system has no more pointers
   * to the level variable, then the value/formula pair can be garbage-collected
   * as well.
   */
   def restrict(lvar: LabelVar
     , f: ObjectExpr[C] => Formula) = {
    _policies += (lvar ->
      ( LOW
        , mkGuardedConfPolicy(ctxt => Not (f (ctxt)))))
  }

  override def assume(f: Formula) = super.assume(Partial.eval(f)(EmptyEnv))
  
  def concretizeExp[T](ctx: ObjectExpr[C], e: FExpr[T]) = {
    Debug.debug(" *** # _policies: " + _policies.size)
    val context =
      AND(_policies.map{
        case (lvar, (level, f)) => f (ctx) ==> (lvar === level)
      })
    super.concretize(context, e);
  }

  // Debug function.
  def getPolicy(lvar: LabelVar): ObjectExpr[C] => Formula = _policies(lvar)._2
}
