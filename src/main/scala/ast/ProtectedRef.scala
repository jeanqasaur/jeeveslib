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
import cap.jeeveslib.env.EmptyEnv
import cap.jeeveslib.eval.Partial
import cap.jeeveslib.jeeves._
import cap.jeeveslib.util.Debug._

sealed trait ProtectedRef[T <: FExpr[_], IC >: Null <: Atom, OC >: Null <: Atom] {
  var v: T
  val writePolicy: ObjectExpr[IC] => Formula
  val outputPolicy: Option[ObjectExpr[IC] => (ObjectExpr[OC] => Formula)]

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

  // TODO: When we crawl over the facet tree, make sure we're attaching the
  // the new policy to everything else that came before...
  protected def writeAs
    (ctxt: ObjectExpr[IC], asOCtxt: ObjectExpr[OC]
      , newVal: T, policyFun: T => T
      , facetCons: (Formula, T, T) => T)
    (implicit jeevesEnv: JeevesLib[OC]): (UpdateResult, T) = {
    if (jeevesEnv.concretize(asOCtxt, writePolicy(ctxt))) {
      val (outcome, iPolicy) = outputPolicy match {
        case Some(ip) => (Unresolved, ip)
        case None =>
          (Success
            , (ic: ObjectExpr[IC]) => (oc: ObjectExpr[OC]) => BoolVal(true))
      }
      // Make a new level variable based on this policy.
      val ivar = jeevesEnv.mkLabel ()
      jeevesEnv.mapPrimaryContext (ivar, ctxt)
      jeevesEnv.restrict (ivar, iPolicy(ctxt)(_))

      // Apply the integrity policy to the untrusted facet.
      val pUntrusted = policyFun(newVal)

      // Return a result that takes the path condition into account.
      jeevesEnv.pushPC(ivar.id)
      val r: T = jeevesEnv.mkFacetTree[T](
        (lv: LabelVar) =>
          jeevesEnv.addWritePolicy[IC](lv, iPolicy)(jeevesEnv)
        , jeevesEnv.getPCList(), pUntrusted, v)(facetCons)
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
  , val writePolicy: ObjectExpr[IC] => Formula
  , val outputPolicy
    : Option[ObjectExpr[IC] => ObjectExpr[OC] => Formula] = None)
  (implicit val jeevesEnv: JeevesLib[OC])
  extends ProtectedRef[IntExpr, IC, OC] {
  def update(ctxt: ObjectExpr[IC], asOctxt: ObjectExpr[OC], vNew: IntExpr)
    : UpdateResult = {
    val (updateResult, r) = writeAs(ctxt, asOctxt, Partial.eval(vNew)(EmptyEnv)
          , outputPolicy match {
            case Some(iPolicy) =>
              ((e: IntExpr) =>
                jeevesEnv.addPolicy[IC](e)(jeevesEnv, iPolicy))
            case None => (e: IntExpr) => e }
          , IntFacet)(jeevesEnv)
    v = r
    return updateResult
  }
}
  
case class ProtectedBoolRef[IC >: Null <: Atom, OC >: Null <: Atom](
  var v: Formula
  , val writePolicy: ObjectExpr[IC] => Formula
  , val outputPolicy
    : Option[ObjectExpr[IC] => ObjectExpr[OC] => Formula] = None)
  (implicit val jeevesEnv: JeevesLib[OC])
  extends ProtectedRef[Formula, IC, OC] {
  def update(ctxt: ObjectExpr[IC], asOctxt: ObjectExpr[OC], vNew: Formula)
    : UpdateResult = {
    val (updateResult, r) = writeAs(ctxt, asOctxt, Partial.eval(vNew)(EmptyEnv)
      , outputPolicy match {
            case Some(iPolicy) =>
              ((e: Formula) =>
                jeevesEnv.addPolicy[IC](e)(jeevesEnv, iPolicy))
            case None => (e: Formula) => e }
      , BoolFacet)(jeevesEnv)
    v = r
    return updateResult
  }
}
 
case class ProtectedObjectRef[IC >: Null <: Atom, OC >: Null <: Atom](
  var v: ObjectExpr[Atom]
  , val writePolicy: ObjectExpr[IC] => Formula
  , val outputPolicy
    : Option[ObjectExpr[IC] => ObjectExpr[OC] => Formula] = None)
  (implicit val jeevesEnv: JeevesLib[OC])
  extends ProtectedRef[ObjectExpr[Atom], IC, OC] {
  def update(ctxt: ObjectExpr[IC], asOctxt: ObjectExpr[OC]
    , vNew: ObjectExpr[Atom]): UpdateResult = {
    val (updateResult, r) = writeAs(
          ctxt, asOctxt, Partial.eval(vNew)(EmptyEnv)
          , outputPolicy match {
            case Some(iPolicy) =>
              ((e: ObjectExpr[Atom]) =>
                jeevesEnv.addPolicy(e)(jeevesEnv, iPolicy))
            case None => (e: ObjectExpr[Atom]) => e }
          , (c: Formula, t: ObjectExpr[Atom], f: ObjectExpr[Atom]) =>
            ObjectFacet (c, t, f))
    v = r
    return updateResult
  }
}

case class ProtectedFunctionRef[A, B, IC >: Null <: Atom, OC >: Null <: Atom](
  var v: FunctionExpr[A, B]
  , val writePolicy: ObjectExpr[IC] => Formula
  , val outputPolicy
    : Option[ObjectExpr[IC] => ObjectExpr[OC] => Formula] = None)
  (implicit val jeevesEnv: JeevesLib[OC])
  extends ProtectedRef[FunctionExpr[A, B], IC, OC] {
  def update(ctxt: ObjectExpr[IC], asOctxt: ObjectExpr[OC]
    , vNew: FunctionExpr[A, B]): UpdateResult ={
    val (updateResult, r) = writeAs(ctxt, asOctxt, Partial.eval(vNew)(EmptyEnv)
        , outputPolicy match {
            case Some(iPolicy) =>
              ((e: FunctionExpr[A, B]) =>
                jeevesEnv.addPolicy[A, B, IC](e)(jeevesEnv, iPolicy))
            case None => (e: FunctionExpr[A, B]) => e }
        , (c: Formula, t: FunctionExpr[A, B], f: FunctionExpr[A, B]) =>
            FunctionFacet (c, t, f))
    v = r
    return updateResult
  }
}
