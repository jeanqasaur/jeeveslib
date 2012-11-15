package cap.scalasmt

/** 
 * Constraint environment for ConstraintEnv.
 * @author kuat
 */

object Inconsistency extends RuntimeException
trait ConstraintEnv {
  type Defaults = List[Formula]
  type Constraints = List[Formula]

  private var CONSTRAINTS: Constraints = Nil
  private var DEFAULTS: Defaults = Nil
  private var SCOPE: Set[Atom] = Set()
  private var ENV: VarEnv = DefaultEnv

  private def solve(fs: List[Formula]) =  
    SMT.solve(fs, DEFAULTS, SCOPE)(ENV) match {
      case Some(e) => e
      case None => throw Inconsistency
    }
  
  def pickBool(spec: BoolVar => Formula = _ => true): BoolVar = {
    val x = Var.makeBool; 
    assume(spec(x)); 
    x
  }
  def pickBool(spec: BoolVar => Formula, default: Formula): BoolVar = {
    val x = pickBool(spec); 
    usually(x === default); 
    x
  }

  def assume(f: Formula) {
    CONSTRAINTS = f :: CONSTRAINTS
  }

  def usually(f: Formula) {
    DEFAULTS = f :: DEFAULTS
  }

  def concretize[T](e: Expr[T]): T = {
    if (CONSTRAINTS.size > 0) {  
      ENV = solve(CONSTRAINTS);
      CONSTRAINTS = Nil;
    }
    e.eval(ENV)
  }
    
  def concretize[T](f: Formula, e: Expr[T]): T = 
    e.eval(solve(f :: CONSTRAINTS));
}
