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
  var v: T

  // Policies directly associated with writing.
  val writePolicy: Option[(T, ObjectExpr[IC]) => Formula]
  def writePolicyFun(ctxt: ObjectExpr[OC])(implicit jeevesEnv: JeevesLib[OC])
    : ObjectExpr[IC] => Boolean = {
    writePolicy match {
      case Some(wp) => ic => jeevesEnv.concretize(ctxt, wp(v, ic))
      case None => ic => true
    }
  }
  
  val outputPolicy: Option[ObjectExpr[IC] => (=> ObjectExpr[OC]) => Formula]
  val defaultOutputPolicy: ObjectExpr[IC] => (=> ObjectExpr[OC]) => Formula =
    ic => oc => BoolVal(true)
  def getOutputPolicy(): ObjectExpr[IC] => (=> ObjectExpr[OC]) => Formula =
    outputPolicy match {
      case Some(ip) => ip
      case None => defaultOutputPolicy
    }

  def varLabel: String

  /*
  private val cm = scala.reflect.runtime.currentMirror
  private val tb = cm.mkToolBox()
  */

    /*
    e.tree match {
      case Function(params, body) =>
        println(params)
        println(body)
        return Unresolved
      case Select(c, field) => return Unresolved
      case _other => return Unresolved
    }
    */
    /*
    val u.Function(params, body) = e.tree
    val paramNames = params map (_.name)
    val firstParam = paramNames.head
    println(u.showRaw(body))
    println(body.collect { case t @ u.Ident(x) if paramNames contains x => t })
    val t: u.Traverser = new u.Traverser {
      override def traverse(tree: u.Tree) = tree match {
        case ident: u.Ident if paramNames contains ident.name => println(s"got ${ident.symbol.name}")
        case _ => super.traverse(tree) } }
    t.traverse(body)
    */

  /**
   * This variable adds the path conditions to the facet tree.
   */
  private def mkFacetTree[T](ictxt: ObjectExpr[IC]
    , lvfun: LabelVar => Unit
    , guardSet: List[PathCondition]
    , high: T, low: T)
    (implicit facetCons: (Formula, T, T) => T, jeevesEnv: JeevesLib[OC]): T = {
    guardSet match {
      case Nil => high
      case g::gs =>
        g match {
          // Make a copy of the path variable so we can customize it for this
          // integrity expression.
          case PathVar(bv) =>
            val lv = jeevesEnv.mkLabel(bv.label);
            jeevesEnv.restrict(lv, ctxt => lv);
            jeevesEnv.mapPrimaryContext(lv, ictxt)
            lvfun(bv);
            facetCons (bv, mkFacetTree[T](ictxt, lvfun, gs, high, low), low)
          case NegPathVar(bv) =>
            val lv = jeevesEnv.mkLabel(bv.label);
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
    (implicit jeevesEnv: JeevesLib[OC]): (UpdateResult, T) = {
    // If the input write policy passes, then apply to subfacets and apply the
    // output write policy.
    if ((writePolicyFun(asOCtxt)(jeevesEnv))(ctxt)) {
      val (outcome, iPolicy):
        (UpdateResult, ObjectExpr[IC] => (=> ObjectExpr[OC]) => Formula) =
        outputPolicy match {
        case Some(ip) =>
          try {
            lazy val undefined = ???
            Partial.eval(ip(ctxt)(undefined))(EmptyEnv) match {
              case BoolVal(true) =>
                println("successful eval to true")
                (Success, ip)
              case BoolVal(false) => println("successful eval to false"); (Failure, ip)
              case _otherwise => (Unresolved, ip)
            }
          }
          catch {
            case u: NotImplementedError =>
              (Unresolved, ip)
          }
        case None =>
          (Success
            , ic => oc => BoolVal(true))
      }
      // Make a new label based on this policy.
      val ivar = jeevesEnv.mkLabel(varLabel)
      jeevesEnv.mapPrimaryContext (ivar, ctxt)
      val cfun: ObjectExpr[OC] => Formula = octxt => iPolicy(ctxt)(octxt)
      jeevesEnv.restrict (ivar, cfun)
//        , octxt => iPolicy(ctxt)(octxt))

      // Apply the integrity policy to the untrusted facet.
      val pUntrusted = policyFun(newVal)

      // Return a result that takes the path condition into account.
      jeevesEnv.pushPC(ivar)
      val r: T = mkFacetTree[T](ctxt
        , (lv: LabelVar) =>
            jeevesEnv.addWritePolicy[IC](
              lv, writePolicyFun(asOCtxt), iPolicy)(jeevesEnv)
        , jeevesEnv.getPCList(), pUntrusted, v)(facetCons, jeevesEnv)
      jeevesEnv.popPC()

      return (outcome, r)
    } else {
      // If we can't write anything, then report that we failed and return
      // the old value.
      return (Failure, v)
    }
  }
  def update(ctxt: ObjectExpr[IC], asOctxt: ObjectExpr[OC], vNew: T)
    : UpdateResult
}

case class ProtectedIntRef[IC >: Null <: Atom, OC >: Null <: Atom](
  var v: IntExpr
  , val writePolicy: Option[(IntExpr, ObjectExpr[IC]) => Formula]
  , val outputPolicy
    : Option[ObjectExpr[IC] => (=> ObjectExpr[OC]) => Formula] = None
  , val varLabel: String = "")
  (implicit val jeevesEnv: JeevesLib[OC])
  extends ProtectedRef[IntExpr, IC, OC] {
  def update(ctxt: ObjectExpr[IC], asOctxt: ObjectExpr[OC], vNew: IntExpr)
    : UpdateResult = {
    val (updateResult, r) = writeAs(ctxt, asOctxt, Partial.eval(vNew)(EmptyEnv)
          , ((e: IntExpr) =>
              jeevesEnv.addPolicy[IC](e)(
                jeevesEnv, writePolicyFun(asOctxt), getOutputPolicy()))
          , IntFacet)(jeevesEnv)
    v = r
    return updateResult
  }
}
  
case class ProtectedBoolRef[IC >: Null <: Atom, OC >: Null <: Atom](
  var v: Formula
  , val writePolicy: Option[(Formula, ObjectExpr[IC]) => Formula]
  , val outputPolicy
    : Option[ObjectExpr[IC] => (=> ObjectExpr[OC]) => Formula] = None
  , val varLabel: String = "")
  (implicit val jeevesEnv: JeevesLib[OC])
  extends ProtectedRef[Formula, IC, OC] {
  def update(ctxt: ObjectExpr[IC], asOctxt: ObjectExpr[OC], vNew: Formula)
    : UpdateResult = {
    val (updateResult, r) = writeAs(ctxt, asOctxt, Partial.eval(vNew)(EmptyEnv)
      , ((e: Formula) =>
          jeevesEnv.addPolicy[IC](e)(
            jeevesEnv, writePolicyFun(asOctxt), getOutputPolicy()))
      , BoolFacet)(jeevesEnv)
    v = r
    return updateResult
  }
}
 
case class ProtectedObjectRef[IC >: Null <: Atom, OC >: Null <: Atom](
  var v: ObjectExpr[Atom]
  , val writePolicy: Option[(ObjectExpr[Atom], ObjectExpr[IC]) => Formula]
  , val outputPolicy
    : Option[ObjectExpr[IC] => (=> ObjectExpr[OC]) => Formula] = None
  , val varLabel: String = "")
  (implicit val jeevesEnv: JeevesLib[OC])
  extends ProtectedRef[ObjectExpr[Atom], IC, OC] {
  def update(ctxt: ObjectExpr[IC], asOctxt: ObjectExpr[OC]
    , vNew: ObjectExpr[Atom]): UpdateResult = {
    val (updateResult, r) = writeAs(
          ctxt, asOctxt, Partial.eval(vNew)(EmptyEnv)
          , ((e: ObjectExpr[Atom]) =>
              jeevesEnv.addPolicy(e)(
                jeevesEnv, writePolicyFun(asOctxt), getOutputPolicy()))
          , (c: Formula, t: ObjectExpr[Atom], f: ObjectExpr[Atom]) =>
            ObjectFacet (c, t, f))
    v = r
    return updateResult
  }
}

case class ProtectedFunctionRef[A, B, IC >: Null <: Atom, OC >: Null <: Atom](
  var v: FunctionExpr[A, B]
  , val writePolicy: Option[(FunctionExpr[A, B], ObjectExpr[IC]) => Formula]
  , val outputPolicy
    : Option[ObjectExpr[IC] => (=> ObjectExpr[OC]) => Formula] = None
  , val varLabel: String = "")
  (implicit val jeevesEnv: JeevesLib[OC])
  extends ProtectedRef[FunctionExpr[A, B], IC, OC] {
  def update(ctxt: ObjectExpr[IC], asOctxt: ObjectExpr[OC]
    , vNew: FunctionExpr[A, B]): UpdateResult ={
    val (updateResult, r) = writeAs(ctxt, asOctxt, Partial.eval(vNew)(EmptyEnv)
        , ((e: FunctionExpr[A, B]) =>
            jeevesEnv.addPolicy[A, B, IC](e)(
              jeevesEnv, writePolicyFun(asOctxt), getOutputPolicy()))
        , (c: Formula, t: FunctionExpr[A, B], f: FunctionExpr[A, B]) =>
            FunctionFacet (c, t, f))
    v = r
    return updateResult
  }
}
