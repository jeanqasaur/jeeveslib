package cap.jeeveslib.ast

/*
 * Definition of references protected by write policies.
 * @author jeanyang
 */
import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.env.EmptyEnv
import cap.jeeveslib.eval.Partial
import cap.jeeveslib.jeeves._

sealed trait ProtectedRef[T <: Expr[_], IC >: Null <: Atom, OC >: Null <: Atom] {
  var v: T
  val iPolicy: (IC, ObjectExpr[OC]) => Formula

  protected def writeAs
    (ctxt: IC, newVal: T, policyFun: T => T, facetCons: (Formula, T, T) => T)
    (implicit jeevesEnv: JeevesLib): T = {
    // Make a new level variable based on this policy.
    val ivar = jeevesEnv.mkLevel ()
    jeevesEnv.mapPrimaryContext (ivar, ctxt)
    jeevesEnv.restrict (ivar
      , (octxt: ObjectExpr[OC]) =>
          iPolicy (ctxt, octxt.asInstanceOf[ObjectExpr[OC]]))

    // Apply the integrity policy to the untrusted facet.
    val pUntrusted = policyFun(newVal)

    // Return a result that takes the path condition into account.
    jeevesEnv.pushPC(ivar.id)
    val r: T = jeevesEnv.mkFacetTree[T](
      (lv: LevelVar) =>
        jeevesEnv.addWritePolicy (lv, iPolicy) (jeevesEnv)
      , jeevesEnv.getPCList(), pUntrusted, v)(facetCons)
    jeevesEnv.popPC()
    r
  }
  def update(ctxt: IC, vNew: T): Unit
}

case class ProtectedIntRef[IC >: Null <: Atom, OC >: Null <: Atom](
  var v: IntExpr, val iPolicy: (IC, ObjectExpr[OC]) => Formula)
  (implicit val jeevesEnv: JeevesLib) extends ProtectedRef[IntExpr, IC, OC] {
  def update(ctxt: IC, vNew: IntExpr): Unit = {
    v = writeAs(ctxt, Partial.eval(vNew)(EmptyEnv)
          , ((e: IntExpr) =>
              jeevesEnv.addPolicy(e)(jeevesEnv, iPolicy))
          , IntFacet)(jeevesEnv)
  }
}
  
case class ProtectedBoolRef[IC >: Null <: Atom, OC >: Null <: Atom](
  var v: Formula, val iPolicy: (IC, ObjectExpr[OC]) => Formula)
  (implicit val jeevesEnv: JeevesLib) extends ProtectedRef[Formula, IC, OC] {
  def update(ctxt: IC, vNew: Formula): Unit = {
    v = writeAs(ctxt, Partial.eval(vNew)(EmptyEnv)
      , (e: Formula) => jeevesEnv.addPolicy(e)(jeevesEnv, iPolicy)
      , BoolFacet)(jeevesEnv)
  }
}
  
case class ProtectedObjectRef[IC >: Null <: Atom, OC >: Null <: Atom](
  var v: ObjectExpr[Atom], val iPolicy: (IC, ObjectExpr[OC]) => Formula)
  (implicit val jeevesEnv: JeevesLib)
  extends ProtectedRef[ObjectExpr[Atom], IC, OC] {
  def update(ctxt: IC, vNew: ObjectExpr[Atom]): Unit = {
    v = writeAs(
          ctxt, Partial.eval(vNew)(EmptyEnv)
          , (e: ObjectExpr[Atom]) => jeevesEnv.addPolicy(e)(jeevesEnv, iPolicy)
          , (c: Formula, t: ObjectExpr[Atom], f: ObjectExpr[Atom]) =>
            ObjectFacet (c, t, f))
  }
}

case class ProtectedFunctionRef[A, B, IC >: Null <: Atom, OC >: Null <: Atom](
  var v: FunctionExpr[A, B], val iPolicy: (IC, ObjectExpr[OC]) => Formula)
  (implicit val jeevesEnv: JeevesLib)
  extends ProtectedRef[FunctionExpr[A, B], IC, OC] {
  def update(ctxt: IC, vNew: FunctionExpr[A, B]): Unit ={
    v = writeAs(ctxt, Partial.eval(vNew)(EmptyEnv)
        , (e: FunctionExpr[A, B]) => jeevesEnv.addPolicy(e)(jeevesEnv, iPolicy)
        , (c: Formula, t: FunctionExpr[A, B], f: FunctionExpr[A, B]) =>
            FunctionFacet (c, t, f))
  }
}


