package test.cap.scalasmt

import cap.scalasmt._
import org.scalatest.FunSuite
import org.scalatest.Assertions
import Partial.eval
import Expr._

class ExamplePartial extends FunSuite {
  implicit val env = EmptyEnv
  val T: Formula = true
  val F: Formula = false

  val b = Var.makeBool
  val i = Var.makeInt
  val a = Var.makeObject[Atom]

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

  test ("constant propagation") {
    expect(i + 7) {eval(i + (3 + 4))}
    expect(b) {eval(T ? b ! F)}
    expect(F) {eval(F ? b ! F)}
    expect(a) {eval(T ? a ! NULL)}
    expect(NULL) {eval(F ? a ! NULL)}
    expect(i) {eval(T ? i ! 0)}
    expect(i + 1) {eval(F ? i ! i + 1)}
  }

  test ("equality propagation") {
    expect(1: IntExpr) {eval((i === 2) ? (i - 1) ! 1)}
    expect(NULL) {eval((! (a === NULL)) ? NULL ! a)}
  }



}
