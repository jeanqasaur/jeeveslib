package cap.jeeveslib.jeeves

/*
 * A library for using ScalaSMT for privacy, using symbolic varaibles to
 * represent sensitive values.
 * @author jeanyang, kuat
 */

import cap.jeeveslib.ast._
import cap.jeeveslib.env.{EmptyEnv, PolicyEnv, WritePolicyEnv}
import cap.jeeveslib.eval._
import JeevesTypes._

trait JeevesLib[C >: Null <: Atom]
  extends PolicyEnv[C] with WritePolicyEnv[C] {
  /**
   * Given a context ctxt, applies f1 if the path condition is true under ctxt
   * and applies f2 otherwise.
   */
   private def conditionOnPC[T](
      ctxt: ObjectExpr[C]
    , f1: Unit => T, f2: Unit => T): T = {
    getPCFormula ()  match {
      case Some(f) =>
        val path: Boolean = concretizeExp(ctxt, f)
        if (path) { f1 () } else { f2 () }
      case None => f1 ()
    }
  }

  /**
   * Concretization: Returns the default value if the path condition is not
   * satisfied.
   */
   def concretize[T](ctxt: ObjectExpr[C], e: FExpr[T]): T = {
    conditionOnPC (ctxt
      , (_: Unit) => concretizeExp(ctxt, e), (_: Unit) => e.default)
  }
  def concretize[T](ctxt: ObjectExpr[C], e: (FExpr[T], FExpr[T])): (T, T) =
    (concretize(ctxt, e._1), concretize(ctxt, e._2))
  def concretize[T >: Null <: Atom](
    ctx: ObjectExpr[C], lst: Traversable[ObjectExpr[T]]): List[T] = {
    for (o <- lst.toList;
      t = concretize(ctx, o).asInstanceOf[T];
      if (t != null))
      yield t;
  }
  
  /**
   * Printing: only happens if the path condition allows it.
   */
   def jprint[T](ctxt: ObjectExpr[C], e: FExpr[T]): Unit = {
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
      case BoolFacet (bv: BoolVar, tc, fc) => {
        // If we are already under a path containing the current condition,
        // we can just evaluate the true branch.
        if (pcHasVar(bv)) {
          evalFun(t())
        } else if (pcHasNegVar(bv)) {
          evalFun(f())
        } else {
          // First evaluate the true branch.
          pushPC(bv);
          val tr: T = jifEval(tc, t, f, evalFun, facetCons);
          popPC();

          // Then evaluate the false branch.
          pushNegPC(bv);
          val fr: T = jifEval(fc, t, f, evalFun, facetCons);
          popPC();

          val trueBranch = evalFun(tr)
          val falseBranch = evalFun(fr)
          if (trueBranch == falseBranch) {
            trueBranch
          } else { facetCons(bv, trueBranch, falseBranch) }
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
  def jfun[A, B](f: FunctionExpr[A, B], arg: A)
    (implicit vf: FacetCons[B]): B = {
    f match {
      case FunctionVal(f) => f(arg)
      case FunctionFacet(bv: BoolVar, thn, els) =>
        if (pcHasVar(bv)) {
          jfun(thn, arg)
        } else if (pcHasNegVar(bv)) {
          jfun(els, arg)
        } else {
          // First evaluate the true branch.
          pushPC(bv);
          val tr = jfun(thn, arg)
          popPC();

          // Then evaluate the false branch.
          pushNegPC(bv);
          val fr = jfun(els, arg)
          popPC();

          // Produce a faceted result and partially evaluate it.
          val r = vf.facetCons(bv, tr, fr)
          // TODO: Partial.eval(r)(EmptyEnv)
          r
        }
      case _ => throw Impossible
    }
  }

  /**
   * Some support for conditional data structures...
   */
  def jfilter[T >: Null <: Atom] (lst: List[T], default: ObjectExpr[T]
    , f: T => Formula)
    : List[ObjectExpr[T]] = {
    lst.map((elt: T) => jif[T](f(elt), (_: Unit) => elt, (_: Unit) => default))
  }
}
