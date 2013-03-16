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

  def debugPrint[C >: Null <: Atom](ctxt: C, e: Formula)
    (implicit jlib: JeevesLib[C])
    : Unit = {
    e match {
      case BoolFacet(lv: LabelVar, t, f) =>
        println(jlib.getPolicy(lv)(ctxt))
        println(e)
      case _ => println(e)
    }
  }

  def debugPrint[T >: Null <: Atom, C >: Null <: Atom](
    ctxt: C, e: ObjectExpr[T])
    (implicit jlib: JeevesLib[C])
    : Unit = {
    e match {
      case ObjectFacet(lv: LabelVar, _t, _f) =>
        println(jlib.getPolicy(lv)(ctxt))
        println(e)
      case _ => println(e)
    }
  }
}
