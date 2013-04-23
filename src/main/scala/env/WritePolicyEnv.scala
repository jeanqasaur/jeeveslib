package cap.jeeveslib.env

/*
 * A library for using ScalaSMT for privacy, using symbolic varaibles to
 * represent sensitive values.
 * @author jeanyang
 */

import scala.collection.mutable.Map;
import scala.collection.mutable.HashMap;
import scala.collection.mutable.Stack;

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.eval.Partial

trait WritePolicyEnv[OC >: Null <: Atom] {
  type WritePolicy = (ObjectExpr[Atom], ObjectExpr[Atom]) => Formula

  private val _primaryContexts: HashMap[LabelVar, ObjectExpr[Atom]] =
    new HashMap()
  def mapPrimaryContext (lvar: LabelVar, ctxt: ObjectExpr[Atom]): Unit = {
    _primaryContexts += (lvar -> ctxt)
  }

  /**
   * Takes an existing label associated with some input context and applies
   * new write/integrity policies.
   */
  def addWritePolicy[IC >: Null <: Atom](lvar: LabelVar
    , inputWritePolicy: Option[ObjectExpr[IC] => Boolean]
    , icOutputWritePolicy: Option[ObjectExpr[IC] => Formula]
    , outputWritePolicy: Option[OutputWritePolicy[IC, OC]])
    (implicit lvars: PolicyEnv[OC])
  : LabelVar = {
    _primaryContexts.get(lvar) match {
      // If there is a context associated, create a fresh level variable and
      // attached the new integrity policy to it.
      case Some(ic) => {
        val ictxt = ic.asInstanceOf[ObjectExpr[IC]]
        val newLvar = lvars.mkLabel(lvar.label)
        mapPrimaryContext (newLvar, ictxt)
        def owp(octxt: ObjectExpr[OC]): Formula = {
          def withOutputWritePolicy(octxt: ObjectExpr[OC]): Formula = {
            outputWritePolicy match {
              case Some(owp) => owp(ictxt.asInstanceOf[ObjectExpr[IC]])(octxt)
              case None => BoolVal(true)
            }
          }
          def withICOutputWritePolicy(): Formula =
            icOutputWritePolicy match {
              case Some(iowp) =>
                Partial.eval(iowp(ictxt.asInstanceOf[ObjectExpr[IC]]))(EmptyEnv)
              case None => BoolVal(true)
            }
          withICOutputWritePolicy() match {
            case BoolVal(false) => BoolVal(false)
            case r => lvar && r && withOutputWritePolicy(octxt)
          }
        }
        val policy =
          inputWritePolicy match {
            case Some(iwp) =>
              if (iwp(ictxt)) { owp _ }
              else { (octxt: ObjectExpr[OC]) => BoolVal(false) }
            case None => owp _
          }
        lvars.restrict (newLvar, policy)
        newLvar
      }
      // Otherwise return the old level variable.
      case None => { lvar }
    }
  }

  def addPolicy[IC >: Null <: Atom](f: Formula)
    (implicit lvars: PolicyEnv[OC]
    , inputWritePolicy: Option[ObjectExpr[IC] => Boolean]
    , icOutputWritePolicy: Option[ObjectExpr[IC] => Formula]
    , outputWritePolicy: Option[OutputWritePolicy[IC, OC]])
  : Formula = {
    f match {
      case BoolFacet(cond, t, f) =>
      val newCond =
        cond match {
        case c: BoolVar =>
          addWritePolicy(
            c, inputWritePolicy, icOutputWritePolicy, outputWritePolicy)
        case _ => throw Impossible
      }
      BoolFacet(newCond, addPolicy(t), addPolicy(f))
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
  def addPolicy[IC >: Null <: Atom](e: IntExpr)
    (implicit lvars: PolicyEnv[OC]
    , inputWritePolicy: Option[ObjectExpr[IC] => Boolean]
    , icOutputWritePolicy: Option[ObjectExpr[IC] => Formula]
    , outputWritePolicy: Option[OutputWritePolicy[IC, OC]])
  : IntExpr = {
    e match {
      case IntFacet (cond, t, f) =>
      val newCond =
        cond match {
        case c: BoolVar =>
          addWritePolicy(
            c, inputWritePolicy, icOutputWritePolicy, outputWritePolicy)
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
  def addPolicy[T >: Null <: Atom, IC >: Null <: Atom](
    e: ObjectExpr[T])
    (implicit lvars: PolicyEnv[OC]
      , inputWritePolicy: Option[ObjectExpr[IC] => Boolean]
      , icOutputWritePolicy: Option[ObjectExpr[IC] => Formula]
      , outputWritePolicy: Option[OutputWritePolicy[IC, OC]])
    : ObjectExpr[T] =
    e match {
      case ObjectFacet(cond, t, f) =>
        val newCond =
          cond match {
            case c: BoolVar =>
              addWritePolicy(
                c, inputWritePolicy, icOutputWritePolicy, outputWritePolicy)
            case _ => throw Impossible
          }
        ObjectFacet(newCond, addPolicy(t), addPolicy (f))
      case ObjectField (root, f) =>
        ObjectField (addPolicy (root), f).asInstanceOf[ObjectExpr[T]]
      case Object(_) => e
  }
  def addPolicy[A, B, IC >: Null <: Atom](
    e: FunctionExpr[A, B])
    (implicit lvars: PolicyEnv[OC]
      , inputWritePolicy: Option[ObjectExpr[IC] => Boolean]
      , icOutputWritePolicy: Option[ObjectExpr[IC] => Formula]
      , outputWritePolicy: Option[OutputWritePolicy[IC, OC]])
    : FunctionExpr[A, B] =
    e match {
      case FunctionVal(_) => e
      case FunctionFacet(cond, t, f) =>
        val newCond =
            cond match {
              case c: BoolVar =>
                addWritePolicy(
                  c, inputWritePolicy, icOutputWritePolicy, outputWritePolicy)
              case _ => throw Impossible
            }
        FunctionFacet(newCond, addPolicy(t), addPolicy(f))
    }
}
