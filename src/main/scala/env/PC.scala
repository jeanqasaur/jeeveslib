package cap.jeeveslib.env

/*
 * Manipulating the path condition.
 * @author jeanyang
 */

import scala.collection.mutable.Stack;

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._

sealed trait PathCondition
case class PathVar(bv: BoolVar) extends PathCondition
case class NegPathVar (bv: BoolVar) extends PathCondition

trait PC {
  private val _pc: Stack[PathCondition] = new Stack ()
 
  def getPCList(): List[PathCondition] = _pc.toList

  def pushPC(bv: BoolVar): Unit = _pc.push(PathVar(bv))
  def pushNegPC(bv: BoolVar): Unit = _pc.push (NegPathVar(bv))
  def popPC (): PathCondition = _pc.pop ()

  def pcHasVar(bv: BoolVar) = _pc.contains(PathVar(bv))
  def pcHasNegVar(bv: BoolVar) = _pc.contains (NegPathVar(bv))

  def getPCFormula (): Option[Formula] = {
    if (_pc.isEmpty) { None
    } else {
      def pcToFormula (pc: PathCondition): Formula = {
        pc match {
          case PathVar(bv) => bv
          case NegPathVar(bv) => Not(bv)
        }
      }
      def mkAnd (f: Formula, pc: PathCondition): Formula = {
        And(f, pcToFormula (pc))
      }
      Some(_pc.foldLeft((BoolVal(true): Formula))(mkAnd))
    }
  }
  def mkGuardedConfPolicy[C >: Null <: Atom](p: ObjectExpr[C] => Formula)
  : (ObjectExpr[C] => Formula) = {
    getPCFormula () match {
      case Some(f) => (context: ObjectExpr[C]) => (f ==> p(context))
      case None => p
    }
  }
}
