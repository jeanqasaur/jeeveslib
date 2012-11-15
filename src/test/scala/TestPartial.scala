package test.cap.jeeveslib

import org.scalatest.FunSuite
import org.scalatest.Assertions

import cap.jeeveslib.ast._
import Expr._
import cap.jeeveslib.env.EmptyEnv
import cap.jeeveslib.eval.Partial._

class ExamplePartial extends FunSuite {
  implicit val env = EmptyEnv
  val T: Formula = true
  val F: Formula = false

  val b = Var.makeBool
  val c = Var.makeBool
//  val a = Var.makeObject[Atom]

  test ("boolean simplifications") {
    expect(T) {eval(true && true)}
    expect(F) {eval((b && false) || (false && true))}
    expect(F) {eval((false || false) || (false || false))}
    expect(F) {eval(false && b)}
    expect(F) {eval(b && false)}
    expect(b) {eval(true && b)}
    expect(b) {eval(b && true)}
    expect(b) {eval(b || false)}
    expect(b) {eval(false || b)}
    expect(T) {eval(true || b)}
    expect(T) {eval(b || true)}
    expect(b) {eval(! (! b))}
    expect(T) {eval(! false)}
    expect(F) {eval(! (true || b))}
  }

  test ("integer simplifications") {
    expect(IntVal(3)) { eval(Plus(IntVal(1), IntVal(2))) }
    expect(IntFacet(b, 1, 2)) { eval(Plus(IntFacet(b, 0, 1), 1)) }
    expect(IntFacet(b, 1, 2)) { eval(Plus(1, IntFacet(b, 0, 1))) }
    expect(IntVal(1)) { eval(Plus(IntFacet(true, 0, 1), 1)) }
    expect(IntVal(2)) { eval(Plus(IntFacet(false, 0, 1), 1)) }
    expect(IntVal(7)) {
      eval(Plus(IntFacet(true, 0, 1), IntFacet(false, 5, 7)))
    }
    expect(IntVal(5)) {
      eval(Plus(IntFacet(true, 0, 1), IntFacet(true, 5, 7)))
    }
    expect(IntVal(6)) {
      eval(Plus(IntFacet(false, 0, 1), IntFacet(true, 5, 7)))
    }
    expect(IntVal(8)) {
      eval(Plus(IntFacet(false, 0, 1), IntFacet(false, 5, 7)))
    }
  }

  test ("constant propagation") {
    expect(b) {eval(T ? b ! F)}
    expect(F) {eval(F ? b ! F)}
/*    expect(a) {eval(T ? a ! NULL)}
    expect(NULL) {eval(F ? a ! NULL)} */
  }
}
