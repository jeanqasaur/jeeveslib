package cap.jeeves

/*
 * A library for using ScalaSMT for privacy, using symbolic varaibles to
 * represent sensitive values.
 * @author jeanyang, kuat
 */

import cap.scalasmt._
import scala.collection.mutable.Map;
import scala.collection.mutable.WeakHashMap;
import scala.collection.mutable.Stack;
import Debug.debug

trait JeevesLib extends Sceeves {
  trait JeevesRecord extends Atom with Serializable

  type LevelVar = BoolVar;
  type Symbolic = ObjectExpr[Atom];
  
  sealed trait Level extends Serializable
  object HIGH extends Level
  object LOW extends Level
  implicit def level2sym(l: Level): Formula = l match {
    case HIGH => true
    case LOW => false
  }

  private var _policies: WeakHashMap[LevelVar, (Level, Symbolic => Formula)] =
    new WeakHashMap()
//  private var _storedPolicies: Map[LevelVar, List[

  sealed trait PathCondition
  case class PathVar (id: String) extends PathCondition
  case class NegPathVar (id: String) extends PathCondition

  private val _pc: Stack[PathCondition] = new Stack ()
  private def pushPC (id: String): Unit = _pc.push (PathVar (id))
  private def pushNegPC (id: String): Unit = _pc.push (NegPathVar (id))
  private def popPC (): PathCondition = _pc.pop ()
  private def getPCFormula (): Formula = {
    def pcToFormula (pc: PathCondition): Formula = {
      pc match {
        case PathVar (id) => BoolVar (id)
        case NegPathVar (id) => Not (BoolVar (id))
      }
    }
    def mkAnd (f: Formula, pc: PathCondition): Formula = {
      And(f, pcToFormula (pc))
    }
    _pc.foldLeft((BoolVal(true): Formula))(mkAnd)
  }
  private def mkGuardedPolicy (p: Symbolic => Formula): (Symbolic => Formula) = {
    (CONTEXT: Symbolic) => (getPCFormula () ==> p (CONTEXT))
  }

  def mkLevel(): LevelVar = pickBool(_ => true, HIGH)

  def mkSensitiveInt(lvar: LevelVar, high: IntExpr, low: IntExpr = -1)
    : IntExpr = 
    lvar ? high ! low
  def mkSensitive(lvar: LevelVar, high: Symbolic, low: Symbolic = NULL)
    : Symbolic = 
    lvar ? high ! low
  def mkSensitiveIntFunction(lvar: LevelVar
    , high: FunctionExpr[IntExpr, IntExpr], low: FunctionExpr[IntExpr, IntExpr])
  : FunctionExpr[IntExpr, IntExpr] = lvar ? high ! low
  def mkSensitiveFunction(lvar: LevelVar
    , high: FunctionExpr[Atom, Atom], low: FunctionExpr[Atom, Atom])
  : FunctionExpr[Atom, Atom] = lvar ? high ! low

  /**
   * Policies take the form restrict(a, f), where a is the level variable and f
   * is a formula that sets a to LOW if f is true.
   * 
   * We store policies as a weak hash map between the level variable and a pair
   * of the value (LOW/HIGH) and the policy.  If the system has no more pointers
   * to the level variable, then the value/formula pair can be garbage-collected
   * as well.
   */
  def restrict(lvar: LevelVar, f: Symbolic => Formula) = {
    _policies += (lvar ->
      (LOW, mkGuardedPolicy ((ctxt: Symbolic) => Not (f (ctxt)))))
  }

  /**
   * Programming with only "restrict" is a pain, so we allow people to collect
   * "permit" policies on things.
   */
  def permit(lvar: LevelVar, f: Symbolic => Formula) = {

  }
  // This function restricts everything except for what has been permitted.
  def commitPolicies(lvar: LevelVar) = {

  }
  
  override def assume(f: Formula) = super.assume(Partial.eval(f)(EmptyEnv))

  private def conditionOnPC[T](
    ctxt: Symbolic, f1: Unit => T, f2: Unit => T): T = {
    val path: Boolean = unsafeConcretize(ctxt, getPCFormula ())
    if (path) { f1 () } else { f2 () }
  }

  /**
   * Unsafe concretization (does not take PC into account).
   */ 
  private def unsafeConcretize[T](ctx: Symbolic, e: Expr[T]) = {
    debug(" *** # _policies: " + _policies.size)
    val context =
      AND(_policies.map{
        case (lvar, (level, f)) => f (ctx) ==> (lvar === level)
      })
    super.concretize(context, e);
  }

  /**
   * Concretization: Returns the default value if the path condition is not
   * satisfied.
   */
  def concretize[T] (ctxt: Symbolic, e: Expr[T]): T = {
    conditionOnPC (ctxt
      , (_: Unit) => unsafeConcretize(ctxt, e), (_: Unit) => e.default)
  }
  def concretize[T] (ctx: Symbolic, e: (Expr[T], Expr[T])): (T, T) =
    (concretize(ctx, e._1), concretize(ctx, e._2))
  def concretize[T >: Null <: Atom](
    ctx: Symbolic, lst: Traversable[Symbolic]): List[T] = {
    for (o <- lst.toList;
      t = concretize(ctx, o).asInstanceOf[T];
      if (t != null))
      yield t;
  }

  /**
   * Printing: only happens if the path condition allows it.
   */
  def jprint[T] (ctxt: Symbolic, e: Expr[T]): Unit = {
    conditionOnPC (ctxt
      , (_: Unit) => println (concretize(ctxt, e)), (_: Unit) => ())
  }

  /**
   * Produces a value for assignment.
   * TODO:
   * - Do we want to concretize under the primary context?
   *     NOTE: It seems like we should...
   * - Do we want to prevent assignments under conditionals?
   */
  def jassign(v: IntExpr, v_old: IntExpr): IntExpr = {
    IntFacet (getPCFormula (), v, v_old)
  }
  def jassign(v: Formula, v_old: Formula): Formula = {
    BoolConditional (getPCFormula (), v, v_old)
  }
  def jassign[T >: Null <: Atom](v: ObjectExpr[T], v_old: ObjectExpr[T])
    : ObjectConditional[T] = {
    ObjectConditional (getPCFormula (), v, v_old)
  }

  /**
   * Guarded assignment--integrity.
   */
  def guardedAssign[T](ctxt: Symbolic, k: LevelVar, v: T, v_old: T): T = {
    val kc: Boolean = unsafeConcretize(ctxt, k);
    if (kc) { v } else { v_old }
  }

  /**
   * Jeeves conditionals.
   */
  def jifEval[T](c: Formula, t: Unit => T, f: Unit => T
    , evalFun: T => T, facetCons: (Formula, T, T) => T): T = {
    val cs: Formula = Partial.eval(c)(EmptyEnv);
    cs match {
      case BoolVal (true) => evalFun (t ())
      case BoolVal (false) => evalFun (f ())
      case BoolConditional (BoolVar (v), tc, fc) => {
        // If we are already under a path containing the current condition,
        // we can just evaluate the true branch.
        if (_pc.contains (PathVar (v))) {
          evalFun (t ())
        } else if (_pc.contains (NegPathVar (v))) {
          evalFun (f ())
        } else {
          // First evaluate the true branch.
          pushPC (v);
          val tr: T = jifEval (tc, t, f, evalFun, facetCons);
          popPC ();

          // Then evaluate the false branch.
          pushNegPC (v);
          val fr: T = jifEval (fc, t, f, evalFun, facetCons);
          popPC ();

          val r: T = facetCons(BoolVar (v), evalFun (tr), evalFun (fr))
          r
        }
      }
      case _ => throw Impossible
    }
  }
  def jif (c: Formula, t: Unit => IntExpr, f: Unit => IntExpr) = {
    jifEval (c, t, f, (e: IntExpr) => Partial.eval (e)(EmptyEnv), IntFacet )
  }
  def jif (c: Formula, t: Unit => Formula, f: Unit => Formula): Formula = {
    jifEval (c, t, f, (e: Formula) => Partial.eval (e)(EmptyEnv)
      , BoolConditional)
  }
  def jif[T >: Null <: Atom] (
    c: Formula, t: Unit => ObjectExpr[T], f: Unit => ObjectExpr[T])
  : ObjectExpr[T] = {
    jifEval (c, t, f
      , ((e: ObjectExpr[T]) =>
          Partial.eval (e)(EmptyEnv).asInstanceOf[ObjectExpr[T]])
      , ObjectConditional[T])
  }
  def jif (c: Formula, t: Unit => Unit, f: Unit => Unit): Unit = {
    jifEval (c, t, f, (_: Unit) => (), (_: Formula, _: Unit, _: Unit) => ())
  }

  // TODO: Add more simplification.
  def jfun[A, B] (f: FunctionExpr[A, B], arg: A)
    (implicit vf: FacetCons[B]): B = {
    f match {
      case FunctionVal (f) => f (arg)
      case FunctionFacet (BoolVar (v), thn, els) =>
        if (_pc.contains (PathVar (v))) {
          jfun (thn, arg)
        } else if (_pc.contains (NegPathVar (v))) {
          jfun (els, arg)
        } else {
          // First evaluate the true branch.
          pushPC (v);
          val tr = jfun (thn, arg)
          popPC ();

          // Then evaluate the false branch.
          pushNegPC (v);
          val fr = jfun (els, arg)
          popPC ();

          val r = vf.facetCons(BoolVar (v), tr, fr)
          r
        }
      case _ => throw Impossible
    }
  }
}
