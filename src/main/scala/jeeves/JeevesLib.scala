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
import JeevesTypes._

trait JeevesLib extends LevelVars with Integrity {
  /**
   * Given a context ctxt, applies f1 if the path condition is true under ctxt
   * and applies f2 otherwise.
   */
  private def conditionOnPC[T](
      ctxt: Sensitive
    , f1: Unit => T, f2: Unit => T): T = {
    getPCFormula ()  match {
      case Some(f) =>
        val path: Boolean = unsafeConcretize(ctxt, f)
        if (path) { f1 () } else { f2 () }
      case None => f1 ()
    }
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
   * Integrity.
   */
  private def genericWriteAs[T] (ctxt: Atom // Primary context is concrete
    , trusted: T, untrusted: T
    , iPolicy: IntegrityPolicy
    , facetCons: (Formula, T, T) => T)
    (implicit policyFun: T => T) : T = {
    // Make a new level variable based on this policy.
    val ivar = mkLevel ()
    mapPrimaryContext (ivar, ctxt)
    allowWrite (ivar, (octxt: Sensitive) => iPolicy (ctxt, octxt))

    // Walk over the facets and apply the integrity policy to existing integrity
    // facets as well.
    val pUntrusted = policyFun(untrusted)

    // Return a result that takes the path condition into account.
    pushPC(ivar.id)
    val r: T = mkFacetTree[T](getPCList(), trusted, pUntrusted)(facetCons)
    popPC()
    r
  }
  def writeAs (ctxt: Atom, iPolicy: IntegrityPolicy
    , trusted: IntExpr, untrusted: IntExpr): IntExpr = {
    genericWriteAs(ctxt, trusted, Partial.eval(untrusted)(EmptyEnv)
      , iPolicy, IntFacet)
  }

  /**
   * Printing: only happens if the path condition allows it.
   */
  def jprint[T] (ctxt: Sensitive, e: Expr[T]): Unit = {
    conditionOnPC (ctxt
      , (_: Unit) => println (concretize(ctxt, e)), (_: Unit) => ())
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
        if (pcHasVar(v)) {
          evalFun (t ())
        } else if (pcHasNegVar(v)) {
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
        if (pcHasVar(v)) {
          jfun (thn, arg)
        } else if (pcHasNegVar(v)) {
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

          // Produce a faceted result and partially evaluate it.
          val r = vf.facetCons(BoolVar (v), tr, fr)
          // TODO: Partial.eval(r)(EmptyEnv)
          r
        }
      case _ => throw Impossible
    }
  }
}
