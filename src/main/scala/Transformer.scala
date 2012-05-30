package cap.scalasmt

/**
 * AST transformers.
 * @author kuat, jeanyang
 */

/**
 * Partial evaluation with  
 * constant and equality propagation.
 */
object Impossible extends RuntimeException
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

  sealed trait ValFacet[T, TS] {
    def valCons (v: T): TS
    def facetCons(c: Formula, t: TS, f: TS): TS
  }
  object ValFacet {
    implicit object BoolValFacet extends ValFacet[Boolean, Formula] {
      def valCons (b) = BoolVal(b)
      def facetCons (c: Formula, t: Formula, f: Formula): Formula =
        BoolConditional (c, t, f)
    }
    implicit object IntValFacet extends ValFacet[BigInt, IntExpr] {
      def valCons (i) = IntVal(i)
      def facetCons (c: Formula, t: IntExpr, f: IntExpr): IntExpr =
        IntFacet (c, t, f)
    }
  }
  sealed trait Mapping[T1, T1S, T2S] {
    def caseMatch (a: T1S, b: T1S
      , fVals: (T1, T1) => T2S
      , fFacets: (Formula, T1S, T1S, Formula, T1S, T1S) => T2S
      , fBothL: (T1S, Formula, T1S, T1S) => T2S
      , fBothR: (Formula, T1S, T1S, T1S) => T2S
      , fOther: (T1S, T1S) => T2S): T2S
  }
  object Mapping {
    implicit object BoolBoolMapping
    extends Mapping[Boolean, Formula, Formula] {
      def caseMatch (sa, sb, fVals, fFacets, fBothL, fBothR, fOther): Formula = {
        (sa, sb) match {
          case (BoolVal (sa), BoolVal (sb)) => fVals (sa, sb)
          case (BoolConditional (c1, t1, f1), BoolConditional (c2, t2, f2)) =>
            fFacets (c1, t1, f1, c2, t2, f2)
          case (v1, BoolConditional (c2, t2, f2)) => fBothL (v1, c2, t2, f2)
          case (BoolConditional (c1, t1, f1), v2) => fBothR (c1, t1, f1, v2)
          case _ => fOther (sa, sb)
        }
      }
    }
    implicit object IntBoolMapping
    extends Mapping[BigInt, IntExpr, Formula] {
      def caseMatch (sa, sb, fVals, fFacets, fBothL, fBothR, fOther): Formula = {
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
    implicit object IntIntMapping
    extends Mapping[BigInt, IntExpr, IntExpr] {
      def caseMatch (sa, sb, fVals, fFacets, fBothL, fBothR, fOther): IntExpr = {
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
    implicit object ObjectBoolMapping
    extends Mapping[Atom, ObjectExpr[Atom], Formula] {
      def caseMatch (sa, sb, fVals, fFacets, fBothL, fBothR, fOther): Formula = {
        (sa, sb) match {
          case (Object (sa), Object (sb)) => fVals (sa, sb)
          case (ObjectConditional (c1, t1, f1), ObjectConditional (c2, t2, f2)) =>
            fFacets (c1, t1, f1, c2, t2, f2)
          case (v1, ObjectConditional (c2, t2, f2)) => fBothL (v1, c2, t2, f2)
          case (ObjectConditional (c1, t1, f1), v2) => fBothR (c1, t1, f1, v2)
          case _ => fOther (sa, sb)
        }
      }
    }
  }

  def eval[T1, T1S, T2, T2S] (a: T1S, b: T1S
    , op: (T1, T1) => T2, exprCons: (T1S, T1S) => T2S)
    (implicit env: Environment
      , m: Mapping[T1, T1S, T2S]
      , v: ValFacet[T2, T2S]
      , te1: TransformEval[T1S], te2: TransformEval[T2S]): T2S = {
    m.caseMatch(te1.teval(a), te1.teval(b)
      // (constant, constant)
      , (sa: T1, sb: T1) => v.valCons (op (sa, sb))
      // (facet, facet)
      , (c1, t1, f1, c2, t2, f2) =>
        te2.teval (
          v.facetCons (c1 && c2, exprCons (t1, t2)
            , v.facetCons (
              c1, exprCons (t1, f2)
              , v.facetCons (c2, exprCons (f1, t2), exprCons (f1, f2)))) )
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
      case BoolConditional(a, b, c) => 
        val sa = eval(a); 
        BoolConditional(sa, eval(b), eval(c))
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
          case BoolConditional(c, t, f) =>
            BoolConditional(c, eval(Not(t)), eval(Not(f)))
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
      case BoolConditional(BoolVal(true), thn, _) => thn
      case BoolConditional(BoolVal(false), _, els) => els
      case BoolConditional(_, a, b) if a == b => a
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

  def eval(e: IntExpr)(implicit env: Environment): IntExpr = 
    {e match {
      case IntFacet(a, b, c) => 
        val sa = eval(a) 
        IntFacet(sa, eval(b), eval(c))
      case Plus(a, b) =>
        eval[BigInt, IntExpr, BigInt, IntExpr](
          a, b, (sa: BigInt, sb: BigInt) => sa + sb, Plus)
      case Minus(a, b) =>
        eval[BigInt, IntExpr, BigInt, IntExpr](a, b, (sa: BigInt, sb: BigInt) => sa - sb, Minus)
      case Times(a, b) =>
        eval[BigInt, IntExpr, BigInt, IntExpr](a, b, (sa: BigInt, sb: BigInt) => sa * sb, Times)
      case e => e
    }} match {
      case e if env.hasAll(e.vars) => e.eval
      case IntFacet(BoolVal(true), thn, _) => thn
      case IntFacet(BoolVal(false), _, els) => els
      case IntFacet(_, a, b) if a == b => a
      case e => e
    } 

  def eval[T >: Null <: Atom](e: ObjectExpr[T])(implicit env: Environment)
    : ObjectExpr[Atom] =
    {e match {
      case ObjectConditional(a, b, c) => 
        val sa = eval(a)
        ObjectConditional(sa, eval(b), eval(c))
      case e => e
    }} match {
      case e if env.hasAll(e.vars) => Object(e.eval)
      case ObjectConditional(BoolVal(true), thn, _) => thn
      case ObjectConditional(BoolVal(false), _, els) => els
      case ObjectConditional(_, a, b) if a == b => a
      case e => e
    }

    // TODO: Field dereference
}

