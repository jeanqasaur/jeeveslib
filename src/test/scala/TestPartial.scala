package test.cap.jeeveslib

import org.scalatest.FunSuite
import org.scalatest.Assertions

import cap.jeeveslib.ast._
import FExpr._
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
    expectResult(T) {eval(true && true)}
    expectResult(F) {eval((b && false) || (false && true))}
    expectResult(F) {eval((false || false) || (false || false))}
    expectResult(F) {eval(false && b)}
    expectResult(F) {eval(b && false)}
    expectResult(b) {eval(true && b)}
    expectResult(b) {eval(b && true)}
    expectResult(b) {eval(b || false)}
    expectResult(b) {eval(false || b)}
    expectResult(T) {eval(true || b)}
    expectResult(T) {eval(b || true)}
    expectResult(b) {eval(! (! b))}
    expectResult(T) {eval(! false)}
    expectResult(F) {eval(! (true || b))}
  }

  test ("integer simplifications") {
    expectResult(IntVal(3)) { eval(Plus(IntVal(1), IntVal(2))) }
    expectResult(IntFacet(b, 1, 2)) { eval(Plus(IntFacet(b, 0, 1), 1)) }
    expectResult(IntFacet(b, 1, 2)) { eval(Plus(1, IntFacet(b, 0, 1))) }
    expectResult(IntVal(1)) { eval(Plus(IntFacet(true, 0, 1), 1)) }
    expectResult(IntVal(2)) { eval(Plus(IntFacet(false, 0, 1), 1)) }
    expectResult(IntVal(7)) {
      eval(Plus(IntFacet(true, 0, 1), IntFacet(false, 5, 7)))
    }
    expectResult(IntVal(5)) {
      eval(Plus(IntFacet(true, 0, 1), IntFacet(true, 5, 7)))
    }
    expectResult(IntVal(6)) {
      eval(Plus(IntFacet(false, 0, 1), IntFacet(true, 5, 7)))
    }
    expectResult(IntVal(8)) {
      eval(Plus(IntFacet(false, 0, 1), IntFacet(false, 5, 7)))
    }
  }

  test ("constant propagation") {
    expectResult(b) {eval(T ? b ! F)}
    expectResult(F) {eval(F ? b ! F)}
/*    expectResult(a) {eval(T ? a ! NULL)}
    expectResult(NULL) {eval(F ? a ! NULL)} */
  }
}
