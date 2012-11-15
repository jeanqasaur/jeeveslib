package test.cap.jeeves

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map

import cap.jeeveslib.ast.Atom
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.jeeves._

/**
 * Making sure the right thing is happening under conditionals.
 * @author jeanyang
 */
class Circular extends FunSuite with JeevesLib {
  case class Node(v: Int) extends Atom

  test("circular dependency") {
    val a = mkLevel()
    val v = mkSensitive(a, Node(1), Node(0))
    restrict(a, (CONTEXT: Sensitive) => CONTEXT === Node(1))
    expect(Node(1)) {
      concretize(v, v)
    }
  }
}
