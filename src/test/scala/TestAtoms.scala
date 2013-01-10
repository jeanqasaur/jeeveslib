package test.cap.jeeveslib

import org.scalatest.FunSuite
import org.scalatest.Assertions
import cap.jeeveslib.ast._
import cap.jeeveslib.smt._

class ExampleAtoms extends FunSuite {

  case class Dummy(ID: BigInt) extends Atom

  def eval[T](expr: Expr[T]) = expr.eval

  test ("set operations") {
    import RelExpr._
    val List(a,b,c,d,e,f) = (1 to 6).toList.map(Dummy(_))
    expectResult(Set(a,b,c)) {eval(((a ++ b ++ c ++ d ++ e) -- d) & (a ++ b ++ c))}
  }

  test ("object set") {
    import RelExpr._
    val s @ List(a,b,c) = (1 to 3).toList.map(Dummy(_))
    expectResult(true) {eval(a in s)}
    expectResult(true) {eval(a ++ b in s)}
  }

  test ("large object set") {
    import RelExpr._
    val k = 20
    val a = (1 to k).toList.map(Dummy(_))
    val b = (k + 1 to 2*k).toList.map(Dummy(_))
    expectResult(false) {SMT.solve(a in (a ++ b)).isEmpty}
    expectResult(false) {SMT.solve((a & b) === Set()).isEmpty}
  } 

  test ("conditional") {
    import Expr._
    val a = Dummy(1);
    expectResult(a) {eval ((a === a) ? a ! NULL)}
  }

  test ("singleton and conditional") {
    import RelExpr._
    val a = Dummy(1);
    expectResult(Set(a)) {eval(a)}
    expectResult(Set(a)) {eval(a ++ a)}
    expectResult(Set(a, null)) {eval(a ++ NULL)}
  }

  case class Node(sub: Dummy) extends Atom

  test ("relational join expression") {
    import RelExpr._

    val a = Node(null)
    val b = Dummy(1)
    val c = Node(b)
    expectResult(Set(b, null)) {eval((a ++ c).sub)}
  }

  test ("object int field") {
    import Expr._
    val a: ObjectExpr[_] = Dummy(1);
    expectResult(1) {eval(a.ID: IntExpr)}
  }

  test ("SMT set translation") {
    import RelExpr._

    val List(a,b,c) = (1 to 3).toList.map(Dummy(_))
    expectResult(false) {SMT.solve(a in ((a ++ b) -- ((b ++ c) & b))).isEmpty}
    expectResult(false) {SMT.solve(a in a).isEmpty}
    expectResult(false) {SMT.solve((b in (b ++ c)) && (b in (b -- c))).isEmpty}
  }  

  test ("SMT relational join") {
    import RelExpr._

    val x = Dummy(1);
    val y: RelExpr = Node(x);
    expectResult(false) {SMT.solve(y.sub === x).isEmpty}
    expectResult(false) {SMT.solve(y.sub.sub === NULL).isEmpty}
  }

  /*
  test ("SMT variable translation") {
    val a = Var.makeObject[Atom];
    val b = Dummy(1);
    val env = SMT.solve( a === ((a === a) ? b ! NULL)); 
    expectResult(false) {env.isEmpty}
    expectResult(b) {env.get(a)}
  }
  */

  case class Record(F: IntExpr, I: BigInt) extends Atom

  test ("SMT object field constraints") {
    import Expr._
    var x = Dummy(1);
    var y = Node(x);
    expectResult(false) {SMT.solve(y.sub === x).isEmpty}
    expectResult(true) {SMT.solve(y.sub === NULL).isEmpty}
  }

  /*
  test ("string expression") {
    var x = Var.makeObject[Atom];
    expectResult(false) {SMT.solve(x === "me").isEmpty}
    expectResult(true) {SMT.solve(x === "me" && x === "you").isEmpty}
  }
  */
}
