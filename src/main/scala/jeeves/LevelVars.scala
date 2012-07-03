package cap.jeeves

/*
 * A library for using ScalaSMT for privacy, using symbolic varaibles to
 * represent sensitive values.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.mutable.WeakHashMap;
import Debug.debug
import JeevesTypes._

trait LevelVars extends Sceeves with PC {
  trait JeevesRecord extends Atom with Serializable

  // Level variables for confidentiality and integrity
  sealed trait Level extends Serializable
  object HIGH extends Level
  object LOW extends Level
  implicit def level2sym(l: Level): Formula = l match {
    case HIGH => true
    case LOW => false
  }

  private val _policies: WeakHashMap[LevelVar, (Level, ConfPolicy)] =
    new WeakHashMap()

  def mkLevel(): LevelVar = pickBool(_ => true, HIGH)

  def mkSensitiveInt(lvar: LevelVar, high: IntExpr, low: IntExpr = -1)
    : IntExpr = 
    lvar ? high ! low
  def mkSensitive(lvar: LevelVar, high: Sensitive, low: Sensitive = NULL)
    : Sensitive = 
    lvar ? high ! low
  def mkSensitiveIntFunction(lvar: LevelVar
    , high: FunctionExpr[IntExpr, IntExpr], low: FunctionExpr[IntExpr, IntExpr])
  : FunctionExpr[IntExpr, IntExpr] = lvar ? high ! low
  def mkSensitiveFunction(lvar: LevelVar
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
  def restrict(lvar: LevelVar, f: ConfPolicy) = {
    _policies += (lvar ->
      (LOW, mkGuardedConfPolicy ((ctxt: Sensitive) => Not (f (ctxt)))))
  }
  def allowWrite(lvar: LevelVar, f: ConfPolicy) = {
    _policies += (lvar -> (LOW, mkGuardedConfPolicy (f)))
  }

  override def assume(f: Formula) = super.assume(Partial.eval(f)(EmptyEnv))
  
  def unsafeConcretize[T](ctx: Sensitive, e: Expr[T]) = {
    debug(" *** # _policies: " + _policies.size)
    val context =
      AND(_policies.map{
        case (lvar, (level, f)) => f (ctx) ==> (lvar === level)
      })
    super.concretize(context, e);
  }
}
