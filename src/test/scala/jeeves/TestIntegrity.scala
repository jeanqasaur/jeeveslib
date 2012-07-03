package test.cap.jeeves

import org.scalatest.FunSuite
import org.scalatest.Assertions
import cap.scalasmt._
import cap.jeeves._
import cap.jeeves.JeevesTypes._

class TestIntegrity extends FunSuite with JeevesLib {
  case class DummyUser(id: BigInt) extends JeevesRecord
  val nobody = DummyUser(-1);
  val alice = DummyUser(0);
  val bob = DummyUser(1);
  val carol = DummyUser(2);

  def allowUserWrite (user: DummyUser): (Sensitive, Sensitive) => Formula =
    (ictxt, otxt) => ictxt === user
  
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
        ((ictxt === alice) && (octxt === bob)), 0, 42)
    expect (0) { concretize(alice, x) }
    expect (42) { concretize(bob, x) }
    expect (0) { concretize(carol, x) }
  }

  test ("permitted writer overwite") {
    var x: IntExpr = writeAs(alice, allowUserWrite (bob), 0, 42)
    x = writeAs(bob, allowUserWrite(bob), x, 43)
    expect (43) { concretize(alice, x) }
    expect (43) { concretize(bob, x) }
    expect (43) { concretize(carol, x) }
  }

  test ("restricted writer overwrite") {
    var x: IntExpr = writeAs(bob, allowUserWrite (bob), 0, 42)
    x = writeAs(alice, allowUserWrite(bob), x, 43)
    expect (42) { concretize(alice, x) }
    expect (42) { concretize(bob, x) }
    expect (42) { concretize(carol, x) }
  }

  test ("output varies depending on who is viewing") {
    var x: IntExpr = writeAs(bob, allowUserWrite (bob), 0, 42)
    x = writeAs(alice, (ictxt, octxt) =>
        ((ictxt === alice) && (octxt === bob)), x, 43)
    expect (42) { concretize(alice, x) }
    expect (43) { concretize(bob, x) }
    expect (42) { concretize(carol, x) }
  }

  test ("combining integrity policies in an operation") {
    val x: IntExpr = writeAs(bob, allowUserWrite (bob), 0, 42)
    val y: IntExpr = writeAs(alice, (ictxt, octxt) =>
        ((ictxt === alice) && (octxt === bob)), 2, 43)
    expect (44) { concretize(alice, x + y) }
    expect (85) { concretize(bob, x + y) }
    expect (44) { concretize(carol, x + y) }
  }

  test ("combining values into restrictive write") {
    val x: IntExpr = writeAs(bob, allowUserWrite (bob), 0, 42)
    val y: IntExpr = writeAs(alice, allowUserWrite (alice), 1, 43)
    val z: IntExpr = writeAs(carol, allowUserWrite (carol), x + y, x + y)
    expect (1) { concretize(alice, z) }
    expect (1) { concretize(bob, z) }
    expect (1) { concretize(carol, z) }
  }

  // If Alice and Bob are allowed to write to x and y respectively, then x + y
  // should be allowed to be written to a value where they are both allowed to
  // write.
  test ("combining values into permissive write") {
    val x: IntExpr = writeAs(bob, allowUserWrite (bob), 0, 42)
    val y: IntExpr = writeAs(alice, allowUserWrite (alice), 1, 43)
    val z: IntExpr = writeAs(carol, (ictxt, octxt) =>
      (ictxt === alice) || (ictxt === bob) || (ictxt === carol), x + y, x + y)
    expect (85) { concretize(alice, z) }
    expect (85) { concretize(bob, z) }
    expect (85) { concretize(carol, z) }
  }

  // Only bob can see the special value alice wrote to him...
  test ("finer-grained policy layering...") {
    val x: IntExpr = writeAs(bob, allowUserWrite (bob), 0, 42)
    val y: IntExpr = writeAs(alice, (ictxt, octxt) =>
        ((ictxt === alice) && (octxt === bob)), 2, 43)
    val z: IntExpr = writeAs(carol, (ictxt, octxt) =>
      (ictxt === alice) || (ictxt === bob) || (ictxt === carol), x + y, x + y)
    expect (44) { concretize(alice, z) }
    expect (85) { concretize(bob, z) }
    expect (44) { concretize(carol, z) }
  }

  // Since only bob knows that he can write, only he can see his value...
  test ("capturing confidentiality policies") {
    val a = mkLevel ()
    restrict (a, (ctxt: Sensitive) => ctxt === bob)
    val secretWriter: Sensitive = mkSensitive(a, bob, nobody)
    val x: IntExpr = writeAs(bob, ((ictxt: Sensitive, octxt: Sensitive) =>
      ictxt === secretWriter), 0, 42)
    expect (bob) { concretize(bob, secretWriter) }
    expect (nobody) { concretize(alice, secretWriter) }
    expect (nobody) { concretize(carol, secretWriter) }
    expect (0) { concretize(alice, x) }
    expect (42) { concretize(bob, x) }
    expect (0) { concretize(carol, x) }
  }
}
