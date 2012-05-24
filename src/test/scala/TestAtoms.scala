package test.cap.scalasmt

import org.scalatest.FunSuite
import org.scalatest.Assertions
import cap.scalasmt._

class ExampleAtoms extends FunSuite {

  case class Dummy(ID: BigInt) extends Atom

  def eval[T](expr: Expr[T]) = expr.eval

  test ("set operations") {
    import RelExpr._
    val List(a,b,c,d,e,f) = (1 to 6).toList.map(Dummy(_))
    expect(Set(a,b,c)) {eval(((a ++ b ++ c ++ d ++ e) -- d) & (a ++ b ++ c))}
  }

  test ("object set") {
    import RelExpr._
    val s @ List(a,b,c) = (1 to 3).toList.map(Dummy(_))
    expect(true) {eval(a in s)}
    expect(true) {eval(a ++ b in s)}
  }

  test ("large object set") {
    import RelExpr._
    val k = 20
    val a = (1 to k).toList.map(Dummy(_))
    val b = (k + 1 to 2*k).toList.map(Dummy(_))
    expect(false) {SMT.solve(a in (a ++ b)).isEmpty}
    expect(false) {SMT.solve((a & b) === Set()).isEmpty}
  } 

  test ("conditional") {
    import Expr._
    val a = Dummy(1);
    expect(a) {eval ((a === a) ? a ! NULL)}
  }

  test ("singleton and conditional") {
    import RelExpr._
    val a = Dummy(1);
    expect(Set(a)) {eval(a)}
    expect(Set(a)) {eval(a ++ a)}
    expect(Set(a, null)) {eval(a ++ NULL)}
  }

  case class Node(sub: Dummy) extends Atom

  test ("relational join expression") {
    import RelExpr._

    val a = Node(null)
    val b = Dummy(1)
    val c = Node(b)
    expect(Set(b, null)) {eval((a ++ c).sub)}
  }

  test ("object int field") {
    import Expr._
    val a: ObjectExpr[_] = Dummy(1);
    expect(1) {eval(a.ID: IntExpr)}
  }

  test ("SMT set translation") {
    import RelExpr._

    val List(a,b,c) = (1 to 3).toList.map(Dummy(_))
    expect(false) {SMT.solve(a in ((a ++ b) -- ((b ++ c) & b))).isEmpty}
    expect(false) {SMT.solve(a in a).isEmpty}
    expect(false) {SMT.solve((b in (b ++ c)) && (b in (b -- c))).isEmpty}
  }  

  test ("SMT relational join") {
    import RelExpr._

    val x = Dummy(1);
    val y: RelExpr = Node(x);
    expect(false) {SMT.solve(y.sub === x).isEmpty}
    expect(false) {SMT.solve(y.sub.sub === NULL).isEmpty}
  }

  test ("SMT variable translation") {
    val a = Var.makeObject[Atom];
    val b = Dummy(1);
    val env = SMT.solve( a === ((a === a) ? b ! NULL)); 
    expect(false) {env.isEmpty}
    expect(b) {env.get(a)}
  }

  case class Record(F: IntExpr, I: BigInt) extends Atom

  test ("SMT int field constraints") {
    val i = Var.makeInt;
    val a = Var.makeObject[Record];
    val r = Record(i, 1337);
    val env = SMT.solve( (a === r && (a.F: IntExpr) === (a.I: IntExpr))) match {
      case Some(e) => e
      case None => fail()
    }

    expect(1337) {env(i)}; 
    expect(r) {env(a)};
  }

  test ("SMT object field constraints") {
    import Expr._
    var x = Dummy(1);
    var y = Node(x);
    expect(false) {SMT.solve(y.sub === x).isEmpty}
    expect(true) {SMT.solve(y.sub === NULL).isEmpty}
  }

  test ("string expression") {
    var x = Var.makeObject[Atom];
    expect(false) {SMT.solve(x === "me").isEmpty}
    expect(true) {SMT.solve(x === "me" && x === "you").isEmpty}
  }
}
