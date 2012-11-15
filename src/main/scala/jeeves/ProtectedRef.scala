package cap.jeeves

/*
 * Definition of references protected by write policies.
 * @author jeanyang
 */

import cap.scalasmt._
import JeevesTypes._

sealed trait ProtectedRef[T <: Expr[_]] {
  var v: T
  val iPolicy: WritePolicy

  protected def writeAs
    (ctxt: Atom, newVal: T, policyFun: T => T, facetCons: (Formula, T, T) => T)
    (implicit jeevesEnv: JeevesLib): T = {
    // Make a new level variable based on this policy.
    val ivar = jeevesEnv.mkLevel ()
    jeevesEnv.mapPrimaryContext (ivar, ctxt)
    jeevesEnv.restrict (ivar, (octxt: Sensitive) => iPolicy (ctxt, octxt))

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
  def update(ctxt: Atom, vNew: T): Unit
}

case class ProtectedIntRef(var v: IntExpr, val iPolicy: WritePolicy)
  (implicit val jeevesEnv: JeevesLib) extends ProtectedRef[IntExpr] {
  def update(ctxt: Atom, vNew: IntExpr): Unit = {
    v = writeAs(ctxt, Partial.eval(vNew)(EmptyEnv)
          , ((e: IntExpr) =>
              jeevesEnv.addPolicy(e)(jeevesEnv, iPolicy))
          , IntFacet)(jeevesEnv)
  }
}
  
case class ProtectedBoolRef(var v: Formula, val iPolicy: WritePolicy)
  (implicit val jeevesEnv: JeevesLib) extends ProtectedRef[Formula] {
  def update(ctxt: Atom, vNew: Formula): Unit = {
    v = writeAs(ctxt, Partial.eval(vNew)(EmptyEnv)
      , (e: Formula) => jeevesEnv.addPolicy(e)(jeevesEnv, iPolicy)
      , BoolFacet)(jeevesEnv)
  }
}
  
case class ProtectedObjectRef(var v: ObjectExpr[Atom]
  , val iPolicy: WritePolicy)
  (implicit val jeevesEnv: JeevesLib) extends ProtectedRef[ObjectExpr[Atom]] {
  def update(ctxt: Atom, vNew: ObjectExpr[Atom]): Unit = {
    writeAs(
      ctxt, Partial.eval(vNew)(EmptyEnv)
      , (e: ObjectExpr[Atom]) => jeevesEnv.addPolicy(e)(jeevesEnv, iPolicy)
      , (c: Formula, t: ObjectExpr[Atom], f: ObjectExpr[Atom]) =>
        ObjectFacet (c, t, f))
  }
}
