package cap.jeeves

/*
 * A library for using ScalaSMT for privacy, using symbolic varaibles to
 * represent sensitive values.
 * @author jeanyang, kuat
 */

import cap.scalasmt._
import scala.collection.mutable.HashMap;
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

  private var _policies: HashMap[LevelVar, (Level, Symbolic => Formula)] =
    new HashMap()

  /* */
  private val _pc: Stack[LevelVar] = new Stack ()
  private def pushPC (v: LevelVar): Unit = _pc.push (v)
  private def popPC (): LevelVar = _pc.pop ()
  private def getPCFormula (): Formula = {
    _pc.fold(BoolVal(true))(And)
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
   * Policies take the form policy(a, f), where a is the level variable and f is a
   * formula that sets a to LOW if f is true.
   * 
   * We store policies as a weak hash map between the level variable and a pair of
   * the value (LOW/HIGH) and the policy.  If the system has no more pointers to the
   * level variable, then the value/formula pair can be garbage-collected as well.
   */
  def policy(lvar: LevelVar, f: Symbolic => Formula) = {
    _policies += (lvar -> (LOW, f))
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

  /* Jeeves conditionals. */
  def jif (c: Formula, t: Unit => IntExpr, f: Unit => IntExpr): IntExpr = {
    val cs: Formula = Partial.eval(c)(EmptyEnv);
    cs match {
      case BoolVal (true) => Partial.eval (t ())(EmptyEnv)
      case BoolVal (false) => Partial.eval (f ())(EmptyEnv)
      case BoolConditional (BoolVar (v), tc, fc) => {
        pushPC (BoolVar (v));
        val tr: IntExpr = jif (tc, t, f);
        val fr: IntExpr = jif (fc, t, f);
        val r: IntExpr = IntFacet (BoolVar (v)
          , Partial.eval (tr)(EmptyEnv), Partial.eval (fr)(EmptyEnv));
        popPC ();
        r
      }
      case _ => throw Impossible
    }
  }

  def jif (c: Formula, t: Unit => Formula, f: Unit => Formula): Formula = {
    val cs: Formula = Partial.eval(c)(EmptyEnv);
    cs match {
      case BoolVal (true) => Partial.eval (t ())(EmptyEnv)
      case BoolVal (false) => Partial.eval (f ())(EmptyEnv)
      case BoolConditional (BoolVar (v), tc, fc) => {
        pushPC (BoolVar (v));
        val tr: Formula = jif (tc, t, f);
        val fr: Formula = jif (fc, t, f);
        val r: Formula = BoolConditional (BoolVar (v)
          , Partial.eval (tr)(EmptyEnv), Partial.eval (fr)(EmptyEnv));
        popPC ();
        r
      }
      case _ => throw Impossible
    }
  }

  def jif[T >: Null <: Atom] (
    c: Formula, t: Unit => ObjectExpr[T], f: Unit => ObjectExpr[T]): ObjectExpr[T] = {
    val cs: Formula = Partial.eval(c)(EmptyEnv);
    cs match {
      case BoolVal (true) => Partial.eval (t ())(EmptyEnv).asInstanceOf[T]
      case BoolVal (false) => Partial.eval (f ())(EmptyEnv).asInstanceOf[T]
      case BoolConditional (BoolVar (v), tc, fc) => {
        pushPC (BoolVar (v));
        val tr: ObjectExpr[T] = jif (tc, t, f);
        val fr: ObjectExpr[T] = jif (fc, t, f);
        val r: ObjectExpr[T] = ObjectConditional[T] (BoolVar (v)
          , Partial.eval (tr)(EmptyEnv).asInstanceOf[ObjectExpr[T]]
          , Partial.eval (fr)(EmptyEnv).asInstanceOf[ObjectExpr[T]]);
        popPC ();
        r
      }
      case _ => throw Impossible
    }
  }
}

