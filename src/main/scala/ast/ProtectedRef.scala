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

sealed trait ProtectedRef[T <: FExpr[_], IC >: Null <: Atom, OC >: Null <: Atom] {
  var v: T
  val iPolicy: ObjectExpr[IC] => (ObjectExpr[OC] => Formula)
  val needsOutput: Boolean

  /*
  private val cm = scala.reflect.runtime.currentMirror
  private val tb = cm.mkToolBox()
  */

  private def evaluateIPolicy(ic: ObjectExpr[IC]): UpdateResult = {
    if (needsOutput) {
      return Unresolved
    } else {
      Partial.eval(iPolicy(ic)(NULL))(EmptyEnv) match {
        case BoolVal(b) => if (b) Success else Failure
        case other => return Unresolved
      }
    }
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
  }

  protected def writeAs
    (ctxt: ObjectExpr[IC], newVal: T, policyFun: T => T
      , facetCons: (Formula, T, T) => T)
    (implicit jeevesEnv: JeevesLib[OC]): (UpdateResult, T) = {
    evaluateIPolicy(ctxt) match {
      // If we can't write anything, then report that we failed and return
      // the old value.
      case Failure => return (Failure, v)
      // If we were not able to tell whether we wrote the value, then report
      // "Unresolved" and write a faceted value.
      // Even if we succeeded,
      case other =>
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
        return (other, r)
    }
  }
  def update(ctxt: ObjectExpr[IC], vNew: T): UpdateResult
}

case class ProtectedIntRef[IC >: Null <: Atom, OC >: Null <: Atom](
  var v: IntExpr, val iPolicy: ObjectExpr[IC] => ObjectExpr[OC] => Formula
  , val needsOutput: Boolean)
  (implicit val jeevesEnv: JeevesLib[OC])
  extends ProtectedRef[IntExpr, IC, OC] {
  def update(ctxt: ObjectExpr[IC], vNew: IntExpr): UpdateResult = {
    val (updateResult, r) = writeAs(ctxt, Partial.eval(vNew)(EmptyEnv)
          , ((e: IntExpr) =>
              jeevesEnv.addPolicy[IC](e)(jeevesEnv, iPolicy))
          , IntFacet)(jeevesEnv)
    v = r
    return updateResult
  }
}
  
case class ProtectedBoolRef[IC >: Null <: Atom, OC >: Null <: Atom](
  var v: Formula, val iPolicy: ObjectExpr[IC] => ObjectExpr[OC] => Formula
  , val needsOutput: Boolean)
  (implicit val jeevesEnv: JeevesLib[OC])
  extends ProtectedRef[Formula, IC, OC] {
  def update(ctxt: ObjectExpr[IC], vNew: Formula): UpdateResult = {
    val (updateResult, r) = writeAs(ctxt, Partial.eval(vNew)(EmptyEnv)
      , (e: Formula) => jeevesEnv.addPolicy(e)(jeevesEnv, iPolicy)
      , BoolFacet)(jeevesEnv)
    v = r
    return updateResult
  }
}
 
case class ProtectedObjectRef[IC >: Null <: Atom, OC >: Null <: Atom](
  var v: ObjectExpr[Atom]
  , val iPolicy: ObjectExpr[IC] => ObjectExpr[OC] => Formula
  , val needsOutput: Boolean)
  (implicit val jeevesEnv: JeevesLib[OC])
  extends ProtectedRef[ObjectExpr[Atom], IC, OC] {
  def update(ctxt: ObjectExpr[IC], vNew: ObjectExpr[Atom]): UpdateResult = {
    val (updateResult, r) = writeAs(
          ctxt, Partial.eval(vNew)(EmptyEnv)
          , (e: ObjectExpr[Atom]) => jeevesEnv.addPolicy(e)(jeevesEnv, iPolicy)
          , (c: Formula, t: ObjectExpr[Atom], f: ObjectExpr[Atom]) =>
            ObjectFacet (c, t, f))
    v = r
    return updateResult
  }
}

case class ProtectedFunctionRef[A, B, IC >: Null <: Atom, OC >: Null <: Atom](
  var v: FunctionExpr[A, B]
  , val iPolicy: ObjectExpr[IC] => ObjectExpr[OC] => Formula
  , val needsOutput: Boolean)
  (implicit val jeevesEnv: JeevesLib[OC])
  extends ProtectedRef[FunctionExpr[A, B], IC, OC] {
  def update(ctxt: ObjectExpr[IC], vNew: FunctionExpr[A, B]): UpdateResult ={
    val (updateResult, r) = writeAs(ctxt, Partial.eval(vNew)(EmptyEnv)
        , (e: FunctionExpr[A, B]) =>
            jeevesEnv.addPolicy[A, B, IC](e)(jeevesEnv, iPolicy)
        , (c: Formula, t: FunctionExpr[A, B], f: FunctionExpr[A, B]) =>
            FunctionFacet (c, t, f))
    v = r
    return updateResult
  }
}
