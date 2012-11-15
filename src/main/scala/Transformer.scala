package cap.scalasmt

/**
 * AST transformers.
 * @author kuat, jeanyang
 */

/**
 * Faceted evaluation.
 */
object Partial {
  sealed trait TransformEval[T] {
    def teval (e: T) (implicit env: Environment) : T
  }
  object TransformEval {
    implicit object TransformEvalFormula extends TransformEval[Formula] {
      def teval (f: Formula) (implicit env: Environment) = eval(f)
    }
    implicit object TransformEvalIntExpr extends TransformEval[IntExpr] {
      def teval (e: IntExpr) (implicit env: Environment) = eval(e)
    }
    implicit object TransformEvalObject extends TransformEval[ObjectExpr[Atom]] {
      def teval (e: ObjectExpr[Atom]) (implicit env: Environment) = eval(e)
    }
  }

  sealed trait FacetJoin[T1, T1S, T2S] {
    def facetJoin (a: T1S, b: T1S
      , fVals: (T1, T1) => T2S
      , fFacets: (Formula, T1S, T1S, Formula, T1S, T1S) => T2S
      , fBothL: (T1S, Formula, T1S, T1S) => T2S
      , fBothR: (Formula, T1S, T1S, T1S) => T2S
      , fOther: (T1S, T1S) => T2S): T2S
  }
  object FacetJoin {
    implicit object BoolBoolFacetJoin
    extends FacetJoin[Boolean, Formula, Formula] {
      def facetJoin (sa, sb, fVals, fFacets, fBothL, fBothR, fOther): Formula = {
        (sa, sb) match {
          case (BoolVal (sa), BoolVal (sb)) => fVals (sa, sb)
          case (BoolFacet (c1, t1, f1), BoolFacet (c2, t2, f2)) =>
            fFacets (c1, t1, f1, c2, t2, f2)
          case (v1, BoolFacet (c2, t2, f2)) => fBothL (v1, c2, t2, f2)
          case (BoolFacet (c1, t1, f1), v2) => fBothR (c1, t1, f1, v2)
          case _ => fOther (sa, sb)
        }
      }
    }
    implicit object IntBoolFacetJoin
    extends FacetJoin[BigInt, IntExpr, Formula] {
      def facetJoin (sa, sb, fVals, fFacets, fBothL, fBothR, fOther): Formula = {
        (sa, sb) match {
          case (IntVal (sa), IntVal (sb)) => fVals (sa, sb)
          case (IntFacet (c1, t1, f1), IntFacet (c2, t2, f2)) =>
            fFacets (c1, t1, f1, c2, t2, f2)
          case (v1, IntFacet (c2, t2, f2)) => fBothL (v1, c2, t2, f2)
          case (IntFacet (c1, t1, f1), v2) => fBothR (c1, t1, f1, v2)
          case _ => fOther (sa, sb)
        }
      }
    }
    implicit object IntIntFacetJoin
    extends FacetJoin[BigInt, IntExpr, IntExpr] {
      def facetJoin (sa, sb, fVals, fFacets, fBothL, fBothR, fOther): IntExpr = {
        (sa, sb) match {
          case (IntVal (sa), IntVal (sb)) => fVals (sa, sb)
          case (IntFacet (c1, t1, f1), IntFacet (c2, t2, f2)) =>
            fFacets (c1, t1, f1, c2, t2, f2)
          case (v1, IntFacet (c2, t2, f2)) => fBothL (v1, c2, t2, f2)
          case (IntFacet (c1, t1, f1), v2) => fBothR (c1, t1, f1, v2)
          case _ => fOther (sa, sb)
        }
      }
    }
    implicit object ObjectBoolFacetJoin
    extends FacetJoin[Atom, ObjectExpr[Atom], Formula] {
      def facetJoin (sa, sb, fVals, fFacets, fBothL, fBothR, fOther): Formula = {
        (sa, sb) match {
          case (Object (sa), Object (sb)) => fVals (sa, sb)
          case (ObjectFacet (c1, t1, f1), ObjectFacet (c2, t2, f2)) =>
            fFacets (c1, t1, f1, c2, t2, f2)
          case (v1, ObjectFacet (c2, t2, f2)) => fBothL (v1, c2, t2, f2)
          case (ObjectFacet (c1, t1, f1), v2) => fBothR (c1, t1, f1, v2)
          case _ => fOther (sa, sb)
        }
      }
    }
  }

  def eval[T1, T1S, T2, T2S] (a: T1S, b: T1S
    , op: (T1, T1) => T2, exprCons: (T1S, T1S) => T2S)
    (implicit env: Environment
      , m: FacetJoin[T1, T1S, T2S]
      , v: ValFacet[T2, T2S]
      , te1: TransformEval[T1S], te2: TransformEval[T2S]): T2S = {
    m.facetJoin(te1.teval(a), te1.teval(b)
      // (constant, constant)
      , (sa: T1, sb: T1) => v.valCons (op (sa, sb))
      // (facet, facet)
      , (c1, t1, f1, c2, t2, f2) =>
        te2.teval (
          v.facetCons (c1
            , v.facetCons (c2, exprCons (t1, t2), exprCons (t1, f2))
            , v.facetCons (c2, exprCons (f1, t2), exprCons (f1, f2)) ) )
      // (_, facet)
      , (v1, c, t, f) =>
          te2.teval (v.facetCons (c, exprCons (v1, t), exprCons (v1, f)))
      // (facet, _)
      , (c, t, f, v2) =>
          te2.teval (v.facetCons (c, exprCons (t, v2), exprCons (f, v2)))
      // (_, _)
      , (sa, sb) => exprCons (sa, sb) )
  }

  def eval (f: Formula) (implicit env: Environment): Formula = 
    {f match {
      case BoolFacet(a, b, c) => 
        val sa = eval(a); 
        BoolFacet(sa, eval(b), eval(c))
      case BoolEq(a, b) =>
        BoolEq(eval(a), eval(b))
      case And(a, b) =>
        eval[Boolean, Formula, Boolean, Formula](
          a, b, (sa: Boolean, sb: Boolean) => sa && sb, And)
      case Or(a, b) =>
        eval[Boolean, Formula, Boolean, Formula](
          a, b, (sa: Boolean, sb: Boolean) => sa || sb, Or)
      case Not(f) => {
        eval(f) match {
          case BoolFacet(c, t, f) =>
            BoolFacet(c, eval(Not(t)), eval(Not(f)))
          case f => Not(eval(f))
        }
      }
      case GT(a, b) =>
        eval[BigInt, IntExpr, Boolean, Formula](
          a, b, (sa: BigInt, sb: BigInt) => sa > sb, GT)
      case LT(a, b) =>
        eval[BigInt, IntExpr, Boolean, Formula](
          a, b, (sa: BigInt, sb: BigInt) => sa < sb, LT)
      case Geq(a, b) =>
        eval[BigInt, IntExpr, Boolean, Formula](
          a, b, (sa: BigInt, sb: BigInt) => sa >= sb, Geq)
      case Leq(a, b) => Leq(eval(a), eval(b))
        eval[BigInt, IntExpr, Boolean, Formula](
          a, b, (sa: BigInt, sb: BigInt) => sa <= sb, Leq)
      case IntEq(a, b) =>
        eval[BigInt, IntExpr, Boolean, Formula](
          a, b, (sa: BigInt, sb: BigInt) => sa == sb, IntEq)
      // TODO: Should we do anything special for this case?
      case f: RelFormula => f
      case ObjectEq(a, b) =>
        eval[Atom, ObjectExpr[Atom], Boolean, Formula](
          a, b, (sa: Atom, sb: Atom) => sa == sb, ObjectEq)
      case b: BoolVar => b
      case BoolVal(true) => BoolVal(true)
      case BoolVal(false) => BoolVal(false)
    }} match {
      case f if env.hasAll(f.vars) => f.eval
      case BoolFacet(BoolVal(true), thn, _) => thn
      case BoolFacet(BoolVal(false), _, els) => els
      case BoolFacet(_, a, b) if a == b => a
      case And(BoolVal(false), _) => false
      case And(_, BoolVal(false)) => false
      case And(BoolVal(true), x) => x
      case And(x, BoolVal(true)) => x
      case Or(BoolVal(false), x) => x
      case Or(x, BoolVal(false)) => x
      case Or(BoolVal(true), x) => true
      case Or(x, BoolVal(true)) => true
      case BoolEq(x, BoolVal(true)) => x
      case BoolEq(BoolVal(true), x) => x
      case BoolEq(x, BoolVal(false)) => Not(x)
      case BoolEq(BoolVal(false), x) => Not(x)
      case Not(Not(f)) => f
      case ObjectEq(a, b) if (a == b) => true
      case IntEq(a, b) if (a == b) => true
      case f => f
    }

  def evalDeref[T, R] (root: ObjectExpr[Atom], f: FieldDesc[T]
    , facetCons: (Formula, R, R) => R)
    (implicit env: Environment, te: TransformEval[R]): R = {
    val sroot: ObjectExpr[Atom] = eval (root);
    sroot match {
      case Object (v) => te.teval ((f (v)).asInstanceOf[R])
      case ObjectFacet (c, tb, fb) => {
          facetCons (c
            , evalDeref[T, R] (tb, f, facetCons)
            , evalDeref[T, R] (fb, f, facetCons))
      }
      case _ => throw Impossible
    }
  }

  def eval(e: IntExpr)(implicit env: Environment): IntExpr = {
    def evalIntIntFacet = eval[BigInt, IntExpr, BigInt, IntExpr]_;
    {e match {
      case IntFacet (a, b, c) => IntFacet(eval(a), eval(b), eval(c))
      case Plus (a, b) =>
        evalIntIntFacet(a, b, (sa: BigInt, sb: BigInt) => sa + sb, Plus)
      case Minus (a, b) =>
        evalIntIntFacet(a, b, (sa: BigInt, sb: BigInt) => sa - sb, Minus)
      case Times (a, b) =>
        evalIntIntFacet(a, b, (sa: BigInt, sb: BigInt) => sa * sb, Times)
      // TODO: Test this.
      case ObjectIntField (root, f) => evalDeref[BigInt, IntExpr] (root, f, IntFacet)
      // Values do not need further simplification.
      case IntVal (_) => e
    }} match {
      case e if env.hasAll(e.vars) => e.eval
      case IntFacet(BoolVal(true), thn, _) => thn
      case IntFacet(BoolVal(false), _, els) => els
      case IntFacet(_, a, b) if a == b => a
      case e => e
    } 
  }

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

    // TODO: Field dereference
}

