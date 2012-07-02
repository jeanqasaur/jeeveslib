package test.cap.jeeves

import org.scalatest.FunSuite
import org.scalatest.Assertions
import cap.scalasmt._
import cap.jeeves._

class TestJif extends FunSuite with JeevesLib {
  case class Dummy(id: BigInt) extends JeevesRecord

  test ("jif with IntExpr") {
    val a = mkLevel();
    val x = mkSensitiveInt(a, 0, 1)
    // If ctxt != 0, then a is LOW.
    restrict (a, (ctxt: Sensitive) => ctxt === Dummy(0))
    val r = jif (x === 0, (_: Unit) => IntVal(3), (_: Unit) => IntVal(4))
    expect (IntFacet(a, IntVal(3), IntVal(4))) { r }
    expect (3) { concretize(Dummy(0), r) }
    expect (4) { concretize(Dummy(1), r) }
  }

  test ("jif with ObjectExpr") {
    val a = mkLevel();
    val x = mkSensitive(a, Dummy(0), Dummy(1))
    // If ctxt != 0, then a is LOW.
    restrict (a, (ctxt: Sensitive) => ctxt === Dummy(0))
    val r =
      jif (x === Dummy(0)
        , (_: Unit) => Object(Dummy(3)), (_: Unit) => Object(Dummy(4)))
    expect (ObjectFacet(a, Dummy(3), Dummy(4))) { r }
    expect (Dummy(3)) { concretize(Dummy(0), r) }
    expect (Dummy(4)) { concretize(Dummy(1), r) }
  }

  test ("restrict under conditional") {
    // TODO: Do we want this?
  }

  test ("nested conditionals with shared path condition") {
    val a = mkLevel();
    val x = mkSensitiveInt(a, 0, 1)
    val y = mkSensitiveInt(a, 2, 3)
    restrict (a, (ctxt: Sensitive) => ctxt === Dummy(0))
    val r =
      jif ( x === 0
        , ((_: Unit) =>
            jif (y === 2, (_: Unit) => IntVal (7), (_: Unit) => IntVal (8)))
        , ((_: Unit) =>
            IntVal (9)) )
    expect (IntFacet(a, IntVal(7), IntVal(9))) { r }
    expect (7) { concretize(Dummy(0), r) }
    expect (9) { concretize(Dummy(1), r) }
  }

  test ("nested conditionals with no shared path condition") {
    val a = mkLevel();
    val b = mkLevel();

    val x = mkSensitiveInt(a, 0, 1)
    val y = mkSensitiveInt(b, 2, 3)

    restrict (a, (ctxt: Sensitive) => ctxt === Dummy(0))
    restrict (b, (ctxt: Sensitive) => ctxt === Dummy(1))

    val r =
      jif ( x === 0
        , ((_: Unit) =>
            jif (y === 2, (_: Unit) => IntVal (7), (_: Unit) => IntVal (8)))
        , ((_: Unit) =>
            IntVal (9)) )
    expect (IntFacet(a, IntFacet(b, IntVal(7), IntVal(8)), IntVal(9))) { r }
    expect (8) { concretize(Dummy(0), r) }
    expect (9) { concretize(Dummy(1), r) }
  }
}
