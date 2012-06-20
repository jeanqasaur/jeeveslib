package cap.jeeves

/*
 * A library for using ScalaSMT for privacy, using symbolic varaibles to
 * represent sensitive values.
 * @author jeanyang, kuat
 */

import cap.scalasmt._
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

  def mkSensitiveInt(lvar: LevelVar, high: IntExpr, low: IntExpr = -1): IntExpr = 
    lvar ? high ! low
  
  def mkSensitive(lvar: LevelVar, high: Symbolic, low: Symbolic = NULL): Symbolic = 
    lvar ? high ! low
  
  /**
   * Policies take the form policy(a, f), where a is the level variable and f isi
   * a formula that sets a to LOW if f is true.
   * 
   * We store policies as a weak hash map between the level variable and a pair
   * of the value (LOW/HIGH) and the policy.  If the system has no more pointers
   * to the level variable, then the value/formula pair can be garbage-collected
   * as well.
   */
  def policy(lvar: LevelVar, f: Symbolic => Formula) = {
    _policies += (lvar -> (LOW, mkGuardedPolicy (f)))
  }
  
  override def assume(f: Formula) = super.assume(Partial.eval(f)(EmptyEnv))

  def concretize[T](ctx: Symbolic, e: Expr[T]) = {
    debug(" *** # _policies: " + _policies.size)
    val context =
      AND(_policies.map{
        case (lvar, (level, f)) => f (ctx) ==> (lvar === level)
      })
    super.concretize(context, e);
  }

  /**
   * Collections of symbolic values.
   */ 

  def concretize[T](ctx: Symbolic, e: (Expr[T], Expr[T])): (T, T) = 
    (concretize(ctx, e._1), concretize(ctx, e._2))

  def concretize[T >: Null <: Atom](ctx: Symbolic, lst: Traversable[Symbolic])
  : List[T] = {
    for (o <- lst.toList;
      t = concretize(ctx, o).asInstanceOf[T];
      if (t != null))
      yield t;
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

  /* Jeeves functions. */
//  def jfunEval[T](
}
