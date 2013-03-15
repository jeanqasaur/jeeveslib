package cap.jeeveslib.debug

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.jeeves._

object DebugPrint {
  def debugPrint[C >: Null <: Atom](ctxt: C, e: IntExpr)
    (implicit jlib: JeevesLib[C])
    : Unit = {
    e match {
      case IntFacet(lv: LabelVar, t, f) =>
        println(jlib.getPolicy(lv)(ctxt))
        println(e)
      case _ => println(e)
    }
  }

  /*
  def debugPrint[T >: Null <: Atom, C >: Null <: Atom](
    ctxt: C, of: ObjectFacet[T])
    (implicit jlib: JeevesLib[C])
    : Unit = {
    val ObjectFacet(cond, _t, _f) = of
    cond match {
      case (lv: LabelVar) =>
        println(jlib.getPolicy(lv)(ctxt))
        println(of)
      case _ => println(of)
    }
  }
  */
}
