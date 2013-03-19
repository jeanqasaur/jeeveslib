package cap.jeeveslib.env

import cap.jeeveslib.ast.{Atom, BoolVar, FExpr, Formula, Var}
import cap.jeeveslib.util._

/** 
 * Constraint environment for ConstraintEnv.
 * @author kuat
 */

object Inconsistency extends RuntimeException
trait ConstraintEnv {
  type Defaults = List[Formula]
  type Constraints = List[Formula]

  private var _boolVars: Set[BoolVar] = Set()
  private var CONSTRAINTS: Constraints = Nil
  private var SCOPE: Set[Atom] = Set()
  private var ENV: VarEnv = DefaultEnv

  private def solve(fs: List[Formula], defaults: List[Formula]) =  
    cap.jeeveslib.smt.SMT.solve(fs, defaults, SCOPE)(ENV) match {
      case Some(e) =>
        Debug.debug("solved: " + e.show())
        e
      case None => throw Inconsistency
    }
  
  /*
  def pickBool(spec: BoolVar => Formula = _ => true): BoolVar = {
    val x = Var.makeBool; 
    assume(spec(x)); 
    x
  }
  */
  def pickBool(): BoolVar = {
    val x = Var.makeBool
    _boolVars = _boolVars + x
    x
  }

  def assume(f: Formula) {
    CONSTRAINTS = f :: CONSTRAINTS
  }

  /*
  def concretize[T](e: FExpr[T], defaults: List[Formula]): T = {
    if (CONSTRAINTS.size > 0) {  
      ENV = solve(CONSTRAINTS, defaults);
      CONSTRAINTS = Nil;
    }
    e.eval(ENV)
  }
  */

  def concretize[T](f: Iterable[Formula], defaults: List[Formula]
    , e: FExpr[T]): T = {
    e.eval(solve(f :: CONSTRAINTS, defaults))
  }
}
