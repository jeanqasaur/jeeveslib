package test.cap.jeeves

import org.scalatest.FunSuite
import org.scalatest.Assertions
import cap.scalasmt._
import cap.jeeves._
import cap.jeeves.JeevesTypes._

class TestIntegrity extends FunSuite with JeevesLib {
  case class DummyUser(id: BigInt) extends JeevesRecord
  val alice = DummyUser(0);
  val bob = DummyUser(1);
  val carol = DummyUser(2);

  def allowUserWrite (user: DummyUser): (Atom, Sensitive) => Formula =
    (ictxt, otxt) => ictxt == user
  

  test ("write allowed for all viewers") {
    val x: IntExpr = writeAs(alice, allowUserWrite (alice), 0, 42)
    expect (42) { concretize(alice, x) }
    expect (42) { concretize(bob, x) }
    expect (42) { concretize(carol, x) }
  }

  test ("write disallowed for all viewers") {
    val x: IntExpr = writeAs(alice, allowUserWrite (bob), 0, 42)
    expect (0) { concretize(alice, x) }
    expect (0) { concretize(bob, x) }
    expect (0) { concretize(carol, x) }
  }

  test ("write selectively allowed for some viewers") {
    val x: IntExpr =
      writeAs(alice, (ictxt, octxt) =>
        ((ictxt == alice) && (octxt === bob)), 0, 42)
    expect (0) { concretize(alice, x) }
    expect (42) { concretize(bob, x) }
    expect (0) { concretize(carol, x) }
  }
}
