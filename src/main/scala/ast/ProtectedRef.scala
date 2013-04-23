package cap.jeeveslib.ast

/*
 * Definition of references protected by write policies.
 * @author jeanyang
 */
/*
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
*/

import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.env.{EmptyEnv, NegPathVar, PathCondition, PathVar}
import cap.jeeveslib.eval.Partial
import cap.jeeveslib.jeeves._
import cap.jeeveslib.util.Debug._

sealed trait ProtectedRef[T <: FExpr[_], IC >: Null <: Atom, OC >: Null <: Atom] {
  protected var v: T
  def getValue(): T = v

  // Policies directly associated with writing.
  val inputWritePolicy: Option[InputWritePolicy[T, IC]]
  def inputWritePolicyFun(asOCtxt: ObjectExpr[OC])(
    implicit jeevesEnv: JeevesLib[OC]): Option[ObjectExpr[IC] => Boolean] = {
    inputWritePolicy match {
      case Some(wp) =>
        Some(ic =>
          Partial.eval(wp(v)(ic))(EmptyEnv) match {
            case BoolVal(bv) => bv
            case facetedExpr => jeevesEnv.concretize(asOCtxt, facetedExpr)
          })
      case None => None
    }
  }
  
  val icOutputWritePolicy: Option[ICOutputWritePolicy[T, IC]]
  def getICOutputWritePolicy(): Option[ObjectExpr[IC] => Formula] = {
    icOutputWritePolicy match {
      case Some(iowp) => Some(iowp(v))
      case None => None
    }
  }

  val outputWritePolicy: Option[T => OutputWritePolicy[IC, OC]]
  def getOutputWritePolicy(): Option[OutputWritePolicy[IC, OC]] = {
    outputWritePolicy match {
      case Some(owp) => Some(owp(v))
      case None => None
    }
  }

  def varLabel: String

  /**
   * This variable adds the path conditions to the facet tree.
   */
  private def mkFacetTree[T](ictxt: ObjectExpr[IC]
    , lvfun: LabelVar => Unit
    , guardSet: List[PathCondition]
    , high: T, low: T)(
    implicit facetCons: (Formula, T, T) => T, jeevesEnv: JeevesLib[OC]): T = {
    guardSet match {
      case Nil => high
      case g::gs =>
        g match {
          // Make a copy of the path variable so we can customize it for this
          // integrity expression.
          case PathVar(bv) =>
            val lv = jeevesEnv.mkLabel(bv.label, jeevesEnv.IntegrityLabel);
            jeevesEnv.restrict(lv, ctxt => lv);
            jeevesEnv.mapPrimaryContext(lv, ictxt)
            lvfun(bv);
            facetCons (bv, mkFacetTree[T](ictxt, lvfun, gs, high, low), low)
          case NegPathVar(bv) =>
            val lv = jeevesEnv.mkLabel(bv.label, jeevesEnv.IntegrityLabel);
            jeevesEnv.restrict(lv, ctxt => !(lv))
            jeevesEnv.mapPrimaryContext(lv, ictxt)
            lvfun(bv);
            facetCons (bv, low, mkFacetTree[T](ictxt, lvfun, gs, high, low))
        }
    }
  }

  // TODO: When we crawl over the facet tree, make sure we're attaching the
  // the new policy to everything else that came before...
  protected def writeAs
    (ctxt: ObjectExpr[IC], asOCtxt: ObjectExpr[OC]
      , newVal: T, policyFun: T => T
      , facetCons: (Formula, T, T) => T)
    (implicit jeevesEnv: JeevesLib[OC]): UpdateResult = {
    def applyInputWritePolicy(): UpdateResult = {
      inputWritePolicyFun(asOCtxt)(jeevesEnv) match {
        case Some(iwp) => if (iwp(ctxt)) { Success } else { Failure }
        case None => Success
      }
    }
    def applyICOutputWritePolicy(): (UpdateResult, Option[Formula])  = {
      icOutputWritePolicy match {
        case Some(iop) => {
          Partial.eval(iop(v)(ctxt))(EmptyEnv) match {
            case BoolVal(false) => return (Failure, None)
            case BoolVal(true) => return (Success, None)
            case pred => return (Success, Some(pred))
          }
        }
        case None => {
          return (Success, None)
        }
      }
    }

    // If the input write policy passes, then apply to subfacets and apply the
    // output write policy.
    applyInputWritePolicy() match {
      case Success =>
        applyICOutputWritePolicy() match {
          case (Failure, _) => return Failure
          case (result, formula) =>
            val writePolicy =
              (formula, outputWritePolicy) match {
                case (Some(p), Some(ip)) =>
                  val f: ObjectExpr[IC] => (=> ObjectExpr[OC]) => Formula =
                    ic => oc => p && ip(v)(ic)(oc)
                  Some(f)
                case (Some(p), None) => None
                case (None, Some(ip)) => Some(ip(v))
                case (None, None) => None
              }
            val ivar = jeevesEnv.mkLabel(varLabel, jeevesEnv.IntegrityLabel)
            jeevesEnv.mapPrimaryContext (ivar, ctxt)

            writePolicy match {
              case Some(iPolicy) =>
                jeevesEnv.restrict(ivar, octxt => iPolicy(ctxt)(octxt))
              case None => ()
            }

            // Apply the integrity policy to the untrusted facet.
            val pUntrusted = policyFun(newVal)

              // Return a result that takes the path condition into account.
            jeevesEnv.pushPC(ivar)
            val r: T = mkFacetTree[T](ctxt
              , (lv: LabelVar) =>
              jeevesEnv.addWritePolicy[IC](
                lv, inputWritePolicyFun(asOCtxt)
                , getICOutputWritePolicy(), getOutputWritePolicy())(
                jeevesEnv)
              , jeevesEnv.getPCList(), pUntrusted, v)(facetCons, jeevesEnv)
            jeevesEnv.popPC()
            v = r
            
            writePolicy match {
              case Some(_) => return Unresolved
              case None => return result
            }
          }
      case Failure => return Failure
      case _ => throw Unexpected("")
    }
  }
  def update(ctxt: ObjectExpr[IC], asOctxt: ObjectExpr[OC], vNew: T)
    : UpdateResult
}

case class ProtectedIntRef[IC >: Null <: Atom, OC >: Null <: Atom](
  protected var v: IntExpr
  , val inputWritePolicy: Option[InputWritePolicy[IntExpr, IC]]
  , val icOutputWritePolicy: Option[ICOutputWritePolicy[IntExpr, IC]]
  , val outputWritePolicy
    : Option[IntExpr => OutputWritePolicy[IC, OC]] = None
  , val varLabel: String = "")
  (implicit val jeevesEnv: JeevesLib[OC])
  extends ProtectedRef[IntExpr, IC, OC] {
  def update(ctxt: ObjectExpr[IC], asOctxt: ObjectExpr[OC], vNew: IntExpr)
    : UpdateResult = {
    writeAs(ctxt, asOctxt, Partial.eval(vNew)(EmptyEnv)
      , ((e: IntExpr) =>
          jeevesEnv.addPolicy[IC](e)(
            jeevesEnv, inputWritePolicyFun(asOctxt)
            , getICOutputWritePolicy(), getOutputWritePolicy()))
      , IntFacet)(jeevesEnv)
  }
}
  
case class ProtectedBoolRef[IC >: Null <: Atom, OC >: Null <: Atom](
  protected var v: Formula
  , val inputWritePolicy: Option[InputWritePolicy[Formula, IC]]
  , val icOutputWritePolicy: Option[ICOutputWritePolicy[Formula, IC]]
  , val outputWritePolicy: Option[Formula => OutputWritePolicy[IC, OC]] = None
  , val varLabel: String = "")
  (implicit val jeevesEnv: JeevesLib[OC])
  extends ProtectedRef[Formula, IC, OC] {
  def update(ctxt: ObjectExpr[IC], asOctxt: ObjectExpr[OC], vNew: Formula)
    : UpdateResult = {
    writeAs(ctxt, asOctxt, Partial.eval(vNew)(EmptyEnv)
      , ((e: Formula) =>
          jeevesEnv.addPolicy[IC](e)(
            jeevesEnv, inputWritePolicyFun(asOctxt)
            , getICOutputWritePolicy(), getOutputWritePolicy()))
      , BoolFacet)(jeevesEnv)
  }
}
 
case class ProtectedObjectRef[T >: Null <: Atom, IC >: Null <: Atom, OC >: Null <: Atom](
  protected var v: ObjectExpr[T]
  , val inputWritePolicy: Option[InputWritePolicy[ObjectExpr[T], IC]]
  , val icOutputWritePolicy: Option[ICOutputWritePolicy[ObjectExpr[T], IC]]
  , val outputWritePolicy
    : Option[ObjectExpr[T] => OutputWritePolicy[IC, OC]] = None
  , val varLabel: String = "")
  (implicit val jeevesEnv: JeevesLib[OC])
  extends ProtectedRef[ObjectExpr[T], IC, OC] {
  def update(ctxt: ObjectExpr[IC], asOctxt: ObjectExpr[OC]
    , vNew: ObjectExpr[T]): UpdateResult = {
    writeAs(ctxt, asOctxt
      , Partial.eval(vNew)(EmptyEnv).asInstanceOf[ObjectExpr[T]]
      , ((e: ObjectExpr[T]) =>
          jeevesEnv.addPolicy(e)(
            jeevesEnv, inputWritePolicyFun(asOctxt)
            , getICOutputWritePolicy(), getOutputWritePolicy()))
      , (c: Formula, t: ObjectExpr[T], f: ObjectExpr[T]) =>
          ObjectFacet (c, t, f))
  }
}

case class ProtectedFunctionRef[A, B, IC >: Null <: Atom, OC >: Null <: Atom](
  protected var v: FunctionExpr[A, B]
  , val inputWritePolicy: Option[InputWritePolicy[FunctionExpr[A, B], IC]]
  , val icOutputWritePolicy: Option[ICOutputWritePolicy[FunctionExpr[A, B], IC]]
  , val outputWritePolicy
    : Option[FunctionExpr[A, B] => OutputWritePolicy[IC, OC]] = None
  , val varLabel: String = "")
  (implicit val jeevesEnv: JeevesLib[OC])
  extends ProtectedRef[FunctionExpr[A, B], IC, OC] {
  def update(ctxt: ObjectExpr[IC], asOctxt: ObjectExpr[OC]
    , vNew: FunctionExpr[A, B]): UpdateResult ={
    writeAs(ctxt, asOctxt, Partial.eval(vNew)(EmptyEnv)
      , ((e: FunctionExpr[A, B]) =>
          jeevesEnv.addPolicy[A, B, IC](e)(
            jeevesEnv, inputWritePolicyFun(asOctxt)
            , getICOutputWritePolicy(), getOutputWritePolicy()))
      , (c: Formula, t: FunctionExpr[A, B], f: FunctionExpr[A, B]) =>
          FunctionFacet (c, t, f))
  }
}
