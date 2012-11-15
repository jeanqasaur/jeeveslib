package cap.jeeveslib.env

/*
 * Manipulating the path condition.
 * @author jeanyang
 */

import scala.collection.mutable.Stack;

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._

trait PC {
  sealed trait PathCondition
  case class PathVar (id: String) extends PathCondition
  case class NegPathVar (id: String) extends PathCondition

  private val _pc: Stack[PathCondition] = new Stack ()
 
  def getPCList (): List[PathCondition] = _pc.toList

  def pushPC (id: String): Unit = _pc.push (PathVar (id))
  def pushNegPC (id: String): Unit = _pc.push (NegPathVar (id))
  def popPC (): PathCondition = _pc.pop ()

  def pcHasVar (v: String) = _pc.contains (PathVar (v))
  def pcHasNegVar (v: String) = _pc.contains (NegPathVar (v))

  def getPCFormula (): Option[Formula] = {
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
  def mkGuardedConfPolicy (p: Sensitive => Formula)
  : (Sensitive => Formula) = {
    getPCFormula () match {
      case Some(f) => (CONTEXT: Sensitive) => (f ==> p (CONTEXT))
      case None => p
    }
  }

  def mkFacetTree[T](lvfun: LevelVar => Unit
    , guardSet: List[PathCondition]
    , high: T, low: T) (implicit facetCons: (Formula, T, T) => T): T = {
    guardSet match {
      case Nil => high
      case g::gs =>
        g match {
          case PathVar (id) =>
            val lv = BoolVar(id);
            lvfun (lv);
            facetCons (lv, mkFacetTree[T](lvfun, gs, high, low), low)
          case NegPathVar (id) =>
            val lv = BoolVar (id);
            lvfun (lv);
            facetCons (lv, low, mkFacetTree[T](lvfun, gs, high, low))
        }
    }
  }
}
