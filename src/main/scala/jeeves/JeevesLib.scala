package cap.jeeves

/*
 * A library for using ScalaSMT for privacy, using symbolic varaibles to
 * represent sensitive values.
 * @author jeanyang, kuat
 */

import cap.scalasmt._
import scala.collection.immutable.Map;
import scala.collection.mutable.{Map => MMap};
import scala.collection.mutable.WeakHashMap;
import Debug.debug

trait JeevesLib extends Sceeves {
  trait JeevesRecord extends Atom with Serializable {
  }
  type LevelVar = BoolVar;
  type Symbolic = ObjectExpr[Atom];
  
  sealed trait Level extends Serializable
  object HIGH extends Level
  object LOW extends Level
  implicit def level2sym(l: Level): Formula = l match {
    case HIGH => true
    case LOW => false
  }

  val CONTEXT: Symbolic = pickObject[Atom];
  
  private var POLICIES: WeakHashMap[LevelVar, (Level, () => Formula)] = new WeakHashMap()

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
  def policy(lvar: LevelVar, f: => Formula) = {
    POLICIES += (lvar -> (LOW, f _))
  }
  
  override def assume(f: Formula) = super.assume(Partial.eval(f)(EmptyEnv))

  def concretize[T](ctx: Symbolic, e: Expr[T]) = {
    debug(" *** # POLICIES: " + POLICIES.size)
    val context = (CONTEXT === ctx) && 
      AND(POLICIES.map{
        case (lvar, (level, f)) => f() ==> (lvar === level)
      })
    super.concretize(context, e);
  }

  /**
   * Collections of symbolic values.
   */ 

  def concretize[T](ctx: Symbolic, e: (Expr[T], Expr[T])): (T, T) = 
    (concretize(ctx, e._1), concretize(ctx, e._2))

  def concretize[T >: Null <: Atom](ctx: Symbolic, lst: Traversable[Symbolic]): List[T] = 
    for (o <- lst.toList;
      t = concretize(ctx, o).asInstanceOf[T];
      if (t != null))
      yield t;
}

