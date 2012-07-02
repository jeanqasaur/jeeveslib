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

  // Level variables for confidentiality and integrity
  type LevelVar = BoolVar;
  type IntegrityVar = BoolVar;

  type Policy = Sensitive => Formula;
  type Sensitive = ObjectExpr[Atom];
  
  sealed trait Level extends Serializable
  object HIGH extends Level
  object LOW extends Level
  implicit def level2sym(l: Level): Formula = l match {
    case HIGH => true
    case LOW => false
  }

  /** 
   * Confidentiality state.
   */
  // The entire policy store.
  private val _policies: WeakHashMap[LevelVar, (Level, Policy)] =
    new WeakHashMap()
  // The temporary policy store for "permit."
  private val _storedPolicies: Map[LevelVar, List[Policy]] = Map()

  sealed trait PathCondition
  case class PathVar (id: String) extends PathCondition
  case class NegPathVar (id: String) extends PathCondition

  private val _pc: Stack[PathCondition] = new Stack ()
  private def pushPC (id: String): Unit = _pc.push (PathVar (id))
  private def pushNegPC (id: String): Unit = _pc.push (NegPathVar (id))
  private def popPC (): PathCondition = _pc.pop ()
  private def getPCFormula (): Option[Formula] = {
    if (_pc.isEmpty) { None
    } else {
      def pcToFormula (pc: PathCondition): Formula = {
        pc match {
          case PathVar (id) => BoolVar (id)
          case NegPathVar (id) => Not (BoolVar (id))
        }
      }
      def mkAnd (f: Formula, pc: PathCondition): Formula = {
        And(f, pcToFormula (pc))
      }
      Some(_pc.foldLeft((BoolVal(true): Formula))(mkAnd))
    }
  }
  private def mkGuardedPolicy (p: Sensitive => Formula)
  : (Sensitive => Formula) = {
    getPCFormula () match {
      case Some(f) => (CONTEXT: Sensitive) => (f ==> p (CONTEXT))
      case None => p
    }
  }

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
  def restrict(lvar: LevelVar, f: Policy) = {
    _policies += (lvar ->
      (LOW, mkGuardedPolicy ((ctxt: Sensitive) => Not (f (ctxt)))))
  }

  /**
   * Programming with only "restrict" is a pain, so we allow people to collect
   * "permit" policies on things.
   */
  def permit(lvar: LevelVar, p: Sensitive => Formula) = {
    val guardedPolicy = mkGuardedPolicy (p);
    _storedPolicies.get(lvar) match {
      case Some(policies: List[Sensitive => Formula]) =>
       _storedPolicies += (lvar -> (guardedPolicy :: policies))
      case None => _storedPolicies += (lvar -> List(guardedPolicy))
    }
  }
  // This function restricts everything except for what has been permitted.
  def commitPolicies(lvar: LevelVar) = {
    def mkSingleFormula (f_acc: Sensitive => Formula, f: Sensitive => Formula)
      : Sensitive => Formula = {
      (ctxt : Sensitive) => Or (f_acc (ctxt), f (ctxt))
    }

    _storedPolicies.get(lvar) match {
      case Some(policies) =>
        val policyFormula: Sensitive => Formula =
          policies.foldLeft(
            (ctxt: Sensitive) => BoolVal(true): Formula)(mkSingleFormula)
        _policies +=
          (lvar ->
            ( LOW
            , mkGuardedPolicy ((ctxt: Sensitive) => Not (policyFormula (ctxt)))))
        _storedPolicies.remove(lvar)
      case None => ()
    }
  }
  
  override def assume(f: Formula) = super.assume(Partial.eval(f)(EmptyEnv))

  private def conditionOnPC[T](
    ctxt: Sensitive, f1: Unit => T, f2: Unit => T): T = {
    getPCFormula ()  match {
      case Some(f) =>
        val path: Boolean = unsafeConcretize(ctxt, f)
        if (path) { f1 () } else { f2 () }
      case None => f1 ()
    }
  }

  /**
   * Unsafe concretization (does not take PC into account).
   */ 
  private def unsafeConcretize[T](ctx: Sensitive, e: Expr[T]) = {
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
  def concretize[T] (ctxt: Sensitive, e: Expr[T]): T = {
    conditionOnPC (ctxt
      , (_: Unit) => unsafeConcretize(ctxt, e), (_: Unit) => e.default)
  }
  def concretize[T] (ctx: Sensitive, e: (Expr[T], Expr[T])): (T, T) =
    (concretize(ctx, e._1), concretize(ctx, e._2))
  def concretize[T >: Null <: Atom](
    ctx: Sensitive, lst: Traversable[Sensitive]): List[T] = {
    for (o <- lst.toList;
      t = concretize(ctx, o).asInstanceOf[T];
      if (t != null))
      yield t;
  }
  
  /**
   * When we write something as a principal, we walk down the facet tree and
   * select a facet based on
a) Create a fresh integrity level variable ilv_x.
b) For integrity facet of the form <ilv_i ? u | v>, you will turn it into a confidentiality facet as follows:
    Create a new confidentiality level variable clv_x.
    let c = ctx(ilv_i) (the context associated with integrity level variable ilv_i).
    Replace the integrity facet with a confidentiality facet <clv_x ? u | v>.
    Add a confidentiality policy of the form restrict clv_x , P(c).
    Notice that if P does not depend on sensitive values, P(c) will evaluate to a concrete value, which means the facet can be trivially replaced with a or b. But, if P does depend on sensitive values, then the new facet will reflect that confidential information.

c) Once you have replaced all the integrity facets in a with confidentiality facets according to b), update x to be equal to <ilv_x ? a | old_x> , and set the context associated with ilv_x to be the primary context.
   * TODO: What do we want to do with the current path condition.
   */
  sealed trait IntegrityEval[TS] {
    def integrityEval (ctxt: Sensitive, facet: TS): TS
  }
  object IntegrityEval {
    implicit object BoolIntegrityEval
    extends IntegrityEval[Formula] {
      def integrityEval (ctxt: Sensitive, facet: Formula): Formula = {
        facet
      }
    }
  }

/*
  def eval[T >: Null <: Atom](e: ObjectExpr[T])(implicit env: Environment)
    : ObjectExpr[Atom] =
    {e match {
      case ObjectFacet(a, b, c) =>
        val sa = eval(a)
        ObjectFacet(sa, eval(b), eval(c))
      case ObjectField (root, f) =>
        evalDeref[Atom, ObjectExpr[Atom]] (root, f, ObjectFacet[Atom])
      case Object(_) => e
    }} match {
      case e if env.hasAll(e.vars) => Object(e.eval)
      case ObjectFacet(BoolVal(true), thn, _) => thn
      case ObjectFacet(BoolVal(false), _, els) => els
      case ObjectFacet(_, a, b) if a == b => a
      case e => e
    }
*/

  
  def writeAs[T] (ctxt: Sensitive, policy: Policy, oldVal: T, newVal: T
    , facetCons: (Formula, T, T) => T): T = {
    // Walk over the facet tree and choose facets.
    val ivar = mkLevel ()
    facetCons(ivar, oldVal, newVal)
  }

  /**
   * Printing: only happens if the path condition allows it.
   */
  def jprint[T] (ctxt: Sensitive, e: Expr[T]): Unit = {
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
    getPCFormula () match {
      case Some (f) => IntFacet (f, v, v_old)
      case None => v
    }
  }
  def jassign(v: Formula, v_old: Formula): Formula = {
    getPCFormula () match {
      case Some (f) => BoolFacet (f, v, v_old)
      case None => v
    }
  }
  def jassign[T >: Null <: Atom](v: ObjectExpr[T], v_old: ObjectExpr[T])
    : ObjectExpr[T] = {
    getPCFormula () match {
      case Some (f) => ObjectFacet (f, v, v_old)
      case None => v
    }
  }

  /**
   * Guarded assignment--integrity.
   */
  def guardedAssign[T](ctxt: Sensitive, k: LevelVar, v: T, v_old: T): T = {
    if (_pc.isEmpty) {
      val kc: Boolean = unsafeConcretize(ctxt, k);
      if (kc) { v } else { v_old }
    } else { v_old }
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
      case BoolFacet (BoolVar (v), tc, fc) => {
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
      , BoolFacet)
  }
  def jif[T >: Null <: Atom] (
    c: Formula, t: Unit => ObjectExpr[T], f: Unit => ObjectExpr[T])
  : ObjectExpr[T] = {
    jifEval (c, t, f
      , ((e: ObjectExpr[T]) =>
          Partial.eval (e)(EmptyEnv).asInstanceOf[ObjectExpr[T]])
      , ObjectFacet[T])
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
