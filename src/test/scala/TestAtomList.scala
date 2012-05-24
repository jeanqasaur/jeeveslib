package test.cap.scalasmt

import org.scalatest.FunSuite
import org.scalatest.Assertions
import cap.scalasmt._

class ExampleAtomList extends FunSuite with Sceeves {

  case class Dummy(ID: BigInt) extends Atom

  def eval[T](expr: Expr[T]) = expr.eval

  def mkElt(x: Dummy): ObjectVar[Dummy] = {
    val v: ObjectVar[Dummy] = pickObject(x);
    assume (v === x);
    v
  }
  val c = (1 to 3).toList.map(Dummy(_))
  val s = c.map(mkElt)

  test ("object list has element") {
    expect(true) { concretize(s.has(Dummy(1))) }
  }

  test ("object list has all elements") {
    c.foreach(e => expect(true) { concretize(s.has(e)) } )
  }

  test ("object list does not have element") {
    expect(false) {concretize(s.has(Dummy(4)))}
  }

  test ("object list has elt greater than 1") {
    expect(true) { concretize(s.hasFormula(_.ID > 1)) }
  }

  test("object field does not have elt greater than 3") {
    expect(false) { concretize(s.hasFormula(_.ID > 3)) }
  }
}
