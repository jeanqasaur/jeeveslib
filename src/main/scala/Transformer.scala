package cap.scalasmt

/**
 * AST transformers.
 * @author kuat
 */

/**
 * Partial evaluation with  
 * constant and equality propagation.
 */
object Partial {
  def eqs(f: Formula)(implicit env: Environment) = {
    var out = env;
    for (c <- f.clauses) c match {    
      case ObjectEq(v: ObjectVar[_], Object(o)) => out = out + (v -> o)
      case ObjectEq(Object(o), v: ObjectVar[_]) => out = out + (v -> o)
      case IntEq(v: IntVar, IntVal(i)) => out = out + (v -> i)
      case IntEq(IntVal(i), v: IntVar) => out = out + (v -> i)
      case _ =>
    }
    out
  }

  def eval(f: Formula)(implicit env: Environment): Formula = 
    {f match {
      case BoolConditional(a, b, c) => 
        val sa = eval(a); 
        BoolConditional(sa, eval(b)(eqs(sa)), eval(c)(eqs(eval(! sa))))
      case BoolEq(a, b) => BoolEq(eval(a), eval(b))
      case And(a, b) => And(eval(a), eval(b))
      case Or(a, b) => Or(eval(a), eval(b))
      case Not(f) => Not(eval(f))
      case GT(a, b) => GT(eval(a), eval(b))
      case LT(a, b) => LT(eval(a), eval(b))
      case Geq(a, b) => Geq(eval(a), eval(b))
      case Leq(a, b) => Leq(eval(a), eval(b))
      case IntEq(a, b) => IntEq(eval(a), eval(b))
      case f: RelFormula => f
      case ObjectEq(a, b) => ObjectEq(eval(a), eval(b))
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
      case IntConditional(a, b, c) => 
        val sa = eval(a) 
        IntConditional(sa, eval(b)(eqs(sa)), eval(c)(eqs(eval(! sa))))
      case Plus(a, b) => Plus(eval(a), eval(b))
      case Minus(a, b) => Minus(eval(a), eval(b))
      case Times(a, b) => Times(eval(a), eval(b))
      case e => e
    }} match {
      case e if env.hasAll(e.vars) => e.eval
      case IntConditional(BoolVal(true), thn, _) => thn
      case IntConditional(BoolVal(false), _, els) => els
      case IntConditional(_, a, b) if a == b => a
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
}

