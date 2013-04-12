package cap.jeeveslib.env

/*
 * A library for using ScalaSMT for privacy, using symbolic varaibles to
 * represent sensitive values.
 * @author jeanyang
 */
import scala.collection.immutable.List
import scala.collection.immutable.Set
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

  // Policies and label dependencies.
  private var _labels: List[LabelVar] = List()
  private val _policies : HashMap[LabelVar, ConfPolicy[C]] =
    new HashMap()

  def mkLabel(label: String=""): LabelVar = {
    val v = pickBool(label)
    _labels = v::_labels
    v
  }

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
   def restrict(lvar: LabelVar, f: ConfPolicy[C]) = {
    _policies += (lvar -> mkGuardedConfPolicy(ctxt => Not (f (ctxt))))
  }

  override def assume(f: Formula) = super.assume(Partial.eval(f)(EmptyEnv))
  
  private def getBoolVars(f: Formula): Set[BoolVar] = {
    def isBoolVar(v: Var[_]) = {
      v match {
        case (b:BoolVar) => true
        case _ => false
      }
    }
    f.vars.filter(isBoolVar).map(x => x.asInstanceOf[BoolVar])
  }
  
  def concretizeExp[T](ctx: ObjectExpr[C], e: FExpr[T]) = {
    Debug.debug(" *** # _policies: " + _policies.size)
    
    val varDeps: HashMap[LabelVar, Set[LabelVar]] = new HashMap()
    var context: List[Formula] = List()

    // Get the dependencies of each level variable and record in varDeps.
    // Build up a formula about the policies.
    _policies.toList.foreach {
      case (lvar, f) =>
        val pred = f (ctx)
        val predVars = getBoolVars(pred)
        context = (pred ==> (lvar === LOW)) :: context
        varDeps.get(lvar) match {
          case Some(deps) => varDeps += (lvar -> (deps ++ predVars))
          case None => varDeps += (lvar -> predVars)
        }
    }

    // Go through and add policies from variable dependencies to the context.
    // If d depends on a, b, and c, then
    // (a == low) /\ (b == low) /\ (c == low) ==> (d == low)
    varDeps.toList.foreach {
      case (lvar, deps) => {
        if (!deps.isEmpty) {
          val pred: Formula =
            deps.fold(BoolVal(true))(
              (accum, label) => (label === LOW) && accum)
          val newConstraint = pred ==> (lvar === LOW)
          context = newConstraint :: context
        }
      }
    }

    super.concretize(context, _labels.map(_ === HIGH), e);
  }

  // Debug function.
  def getPolicy(lvar: LabelVar): ObjectExpr[C] => Formula = _policies(lvar)
}
