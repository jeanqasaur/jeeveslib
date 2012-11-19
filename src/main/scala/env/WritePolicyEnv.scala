package cap.jeeveslib.env

/*
 * A library for using ScalaSMT for privacy, using symbolic varaibles to
 * represent sensitive values.
 * @author jeanyang
 */

import scala.collection.mutable.Map;
import scala.collection.mutable.WeakHashMap;
import scala.collection.mutable.Stack;

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._

trait WritePolicyEnv {
  private val _primaryContexts: WeakHashMap[LevelVar, Atom] =
    new WeakHashMap()
  def mapPrimaryContext (lvar: LevelVar, ctxt: Atom): Unit = {
    _primaryContexts += (lvar -> ctxt)
  }

  def addWritePolicy (lvar: LevelVar, iPolicy: WritePolicy)
    (implicit lvars: PolicyEnv)
  : LevelVar = {
    _primaryContexts.get(lvar) match {
      // If there is a context associated, create a fresh level variable and
      // attached the new integrity policy to it.
      case Some(ictxt) =>
        val newLvar = lvars.mkLevel ()
        mapPrimaryContext (newLvar, ictxt)
        lvars.restrict (newLvar
          , (octxt: Sensitive) => lvar && iPolicy (ictxt, octxt))
        newLvar
      // Otherwise return the old level variable.
      case None => lvar
    }
  }

  def addPolicy(f: Formula)
    (implicit lvars: PolicyEnv, iPolicy: WritePolicy)
  : Formula = {
    f match {
      case BoolFacet(cond, t, f) =>
      val newCond =
        cond match {
        case c: BoolVar => addWritePolicy(c, iPolicy)
        case _ => throw Impossible
      }
      BoolFacet(newCond, addPolicy (t), addPolicy (f))
      case BoolEq(a, b) => BoolEq(addPolicy(a), addPolicy(b))
      case And(a, b) => And(addPolicy(a), addPolicy(b))
      case Or(a, b) => Or(addPolicy(a), addPolicy(b))
      case Not(f) => Not(addPolicy(f))
      case GT(a, b) => GT(addPolicy(a), addPolicy(b))
      case LT(a, b) => LT(addPolicy(a), addPolicy(b))
      case Geq(a, b) => Geq(addPolicy(a), addPolicy(b))
      case Leq(a, b) => Leq(addPolicy(a), addPolicy(b))
      case IntEq(a, b) => IntEq(addPolicy(a), addPolicy(b))
      case f: RelFormula => f // TODO??
      case ObjectEq(a, b) => ObjectEq (addPolicy(a), addPolicy(b))
      case b: BoolVar => f
      case BoolVal(_) => f
    }
  }
  def addPolicy(e: IntExpr)
    (implicit lvars: PolicyEnv, iPolicy: WritePolicy)
  : IntExpr = {
    e match {
      case IntFacet (cond, t, f) =>
      val newCond =
        cond match {
        case c: BoolVar => addWritePolicy(c, iPolicy)
        case _ => throw Impossible
      }
      IntFacet (newCond, addPolicy(t), addPolicy(f))
      case Plus (a, b) => Plus (addPolicy(a), addPolicy(b))
      case Minus (a, b) => Minus (addPolicy(a), addPolicy(b))
      case Times (a, b) => Times (addPolicy(a), addPolicy(b))
      case ObjectIntField (root, f) => ObjectIntField (addPolicy (root), f)
      case IntVal (_) => e
    }
  }
  def addPolicy[T >: Null <: Atom](e: ObjectExpr[T])
    (implicit lvars: PolicyEnv, iPolicy: WritePolicy): ObjectExpr[T] =
    e match {
      case ObjectFacet(cond, t, f) =>
        val newCond =
          cond match {
            case c: BoolVar => addWritePolicy(c, iPolicy)
            case _ => throw Impossible
          }
        ObjectFacet(newCond, addPolicy(t), addPolicy (f))
      case ObjectField (root, f) =>
        ObjectField (addPolicy (root), f).asInstanceOf[ObjectExpr[T]]
      case Object(_) => e
  }
  def addPolicy[A, B](e: FunctionExpr[A, B])
    (implicit lvars: PolicyEnv, iPolicy: WritePolicy): FunctionExpr[A, B] =
    e match {
      case FunctionVal(_) => e
      case FunctionFacet(cond, t, f) =>
        val newCond =
            cond match {
              case c: BoolVar => addWritePolicy(c, iPolicy)
              case _ => throw Impossible
            }
        FunctionFacet(newCond, addPolicy(t), addPolicy(f))
    }
}
