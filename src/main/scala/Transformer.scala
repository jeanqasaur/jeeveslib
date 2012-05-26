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
  /**
   * TODO: What is the purpose of this function?
   */
  def eqs(f: Formula)(implicit env: Environment) = {
    var out = env;
    for (c <- f.clauses) c match {    
      // This is used only for contexts, right?
      case ObjectEq(v: ObjectVar[_], Object(o)) => out = out + (v -> o)
      case ObjectEq(Object(o), v: ObjectVar[_]) => out = out + (v -> o)
      case _ =>
    }
    out
  }

  @inline
  def evalBoolExpr(a: Formula, b: Formula
    , op: (Boolean, Boolean) => Boolean, exprCons: (Formula, Formula) => Formula)
    (implicit env: Environment) =
    (eval(a), eval(b)) match {
    case (BoolVal(sa), BoolVal(sb)) => BoolVal(op(sa, sb))
    case (BoolConditional(c1, t1, f1), BoolConditional(c2, t2, f2)) =>
      eval(
        BoolConditional(c1 && c2, exprCons(t1, t2)
        , BoolConditional (
          c1, exprCons(t1, f2)
          , BoolConditional (c2, exprCons(f1, t2), exprCons(f1, f2))) ) )
    case (BoolConditional(c, t, f), v2) =>
      eval(BoolConditional(c, exprCons(t, v2), exprCons(f, v2)))
    case (v1, BoolConditional(c, t, f)) =>
      eval(BoolConditional(c, exprCons(v1, t), exprCons(v1, f)))
    // TODO: Look into this "impossible" case
    case (sa, sb) => exprCons(sa, sb)
  }
  
  @inline
  def evalIntBoolExpr (a: IntExpr, b: IntExpr
    , op: (BigInt, BigInt) => Boolean, exprCons: (IntExpr, IntExpr) => Formula)
    (implicit env: Environment) =
    (eval(a), eval(b)) match {
    case (IntVal(sa), IntVal(sb)) => BoolVal(op(sa, sb))
    case (IntFacet(c1, t1, f1), IntFacet(c2, t2, f2)) =>
      eval(
        BoolConditional(c1 && c2, exprCons(t1, t2)
          , BoolConditional (
            c1, exprCons(t1, f2)
            , BoolConditional (c2, exprCons(f1, t2), exprCons(f1, f2))) ) )
    case (IntFacet(c, t, f), v2) =>
      eval(BoolConditional(c, exprCons(t, v2), exprCons(f, v2)))
    case (v1, IntFacet(c, t, f)) =>
      eval(BoolConditional(c, exprCons(v1, t), exprCons(v1, f)))
    case (_, _) => throw Impossible
  } 

  @inline
  def evalIntIntExpr (a: IntExpr, b: IntExpr
    , op: (BigInt, BigInt) => BigInt, exprCons: (IntExpr, IntExpr) => IntExpr)
    (implicit env: Environment) =
    (eval(a), eval(b)) match {
    case (IntVal(sa), IntVal(sb)) => IntVal(op(sa, sb))
    case (IntFacet(c1, t1, f1), IntFacet(c2, t2, f2)) =>
      eval(
        IntFacet(c1 && c2, exprCons(t1, t2)
          , IntFacet (
            c1, exprCons(t1, f2)
            , IntFacet (c2, exprCons(f1, t2), exprCons(f1, f2))) ) )
    case (IntFacet(c, t, f), v2) =>
      eval(IntFacet(c, exprCons(t, v2), exprCons(f, v2)))
    case (v1, IntFacet(c, t, f)) =>
      eval(IntFacet(c, exprCons(v1, t), exprCons(v1, f)))
    case (_, _) => throw Impossible
  }

  @inline
  def evalObjectExpr[T >: Null <: Atom] (a: ObjectExpr[T], b: ObjectExpr[T]
    , op: (Atom, Atom) => Boolean
    , exprCons: (ObjectExpr[Atom], ObjectExpr[Atom]) => Formula)
    (implicit env: Environment) =
    (eval(a), eval(b)) match {
    case (Object (sa), Object (sb)) => BoolVal (op (sa, sb))
    case (ObjectConditional (c1, t1, f1), ObjectConditional (c2, t2, f2)) =>
      eval(
        BoolConditional (c1 && c2, exprCons(t1, t2)
          , BoolConditional (
            c1, exprCons(t1, f2)
            , BoolConditional (c2, exprCons(f1, t2), exprCons(f1, f2))) ) )
    case (ObjectConditional (c, t, f), v2) =>
      eval(BoolConditional (c, exprCons(t, v2), exprCons(f, v2)))
    case (v1, ObjectConditional (c, t, f)) =>
      eval(BoolConditional (c, exprCons(v1, t), exprCons(v1, f)))
    // This case can arise if one of the objects is symbolic.
    case (sa, sb) => exprCons(sa, sb)
  }

  def eval(f: Formula)(implicit env: Environment): Formula = 
    {f match {
      case BoolConditional(a, b, c) => 
        val sa = eval(a); 
        BoolConditional(sa, eval(b)(eqs(sa)), eval(c)(eqs(eval(! sa))))
      case BoolEq(a, b) =>
        BoolEq(eval(a), eval(b))
      case And(a, b) =>
        evalBoolExpr(a, b, (sa: Boolean, sb: Boolean) => sa && sb, And)
      case Or(a, b) =>
        evalBoolExpr(a, b, (sa: Boolean, sb: Boolean) => sa || sb, Or)
      case Not(f) => {
        eval(f) match {
          case BoolConditional(c, t, f) =>
            BoolConditional(c, eval(Not(t)), eval(Not(f)))
          case f => Not(eval(f))
        }
      }
      case GT(a, b) =>
        evalIntBoolExpr(a, b, (sa: BigInt, sb: BigInt) => sa > sb, GT)
      case LT(a, b) =>
        evalIntBoolExpr(a, b, (sa: BigInt, sb: BigInt) => sa < sb, LT)
      case Geq(a, b) =>
        evalIntBoolExpr(a, b, (sa: BigInt, sb: BigInt) => sa >= sb, Geq)
      case Leq(a, b) => Leq(eval(a), eval(b))
        evalIntBoolExpr(a, b, (sa: BigInt, sb: BigInt) => sa <= sb, Leq)
      case IntEq(a, b) =>
        evalIntBoolExpr(a, b, (sa: BigInt, sb: BigInt) => sa == sb, IntEq)
      // TODO: Should we do anything special for this case?
      case f: RelFormula => f
      case ObjectEq(a, b) =>
        evalObjectExpr(a, b, (sa: Atom, sb: Atom) => sa == sb, ObjectEq)
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
        IntFacet(sa, eval(b)(eqs(sa)), eval(c)(eqs(eval(! sa))))
      case Plus(a, b) =>
        evalIntIntExpr(a, b, (sa: BigInt, sb: BigInt) => sa + sb, Plus)
      case Minus(a, b) =>
        evalIntIntExpr(a, b, (sa: BigInt, sb: BigInt) => sa - sb, Minus)
      case Times(a, b) =>
        evalIntIntExpr(a, b, (sa: BigInt, sb: BigInt) => sa * sb, Times)
      case e => e
    }} match {
      case e if env.hasAll(e.vars) => e.eval
      case IntFacet(BoolVal(true), thn, _) => thn
      case IntFacet(BoolVal(false), _, els) => els
      case IntFacet(_, a, b) if a == b => a
      case e => e
    } 

  def eval[T >: Null <: Atom](e: ObjectExpr[T])(implicit env: Environment): ObjectExpr[Atom] =
    {e match {
      case ObjectConditional(a, b, c) => 
        val sa = eval(a)
        ObjectConditional(sa, eval(b)(eqs(sa)), eval(c)(eqs(eval(! sa))))
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

