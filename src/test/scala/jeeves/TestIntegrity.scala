package test.cap.jeeveslib.jeeves

import org.scalatest.FunSuite
import org.scalatest.Assertions
import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.jeeves._

class TestIntegrity extends FunSuite with JeevesLib {
  case class DummyUser(id: BigInt) extends Atom
  val nobody = DummyUser(-1);
  val alice = DummyUser(0);
  val bob = DummyUser(1);
  val carol = DummyUser(2);

  def allowUserWrite (user: DummyUser): (Sensitive, Sensitive) => Formula =
    (ictxt, otxt) => ictxt === user

  test ("write allowed for all viewers") {
    val x = ProtectedIntRef(0, allowUserWrite (alice))(this)
    x.update(alice, 42)
    expect (42) { concretize(alice, x.v) }
    expect (42) { concretize(bob, x.v) }
    expect (42) { concretize(carol, x.v) }
  }

  test ("write disallowed for all viewers") {
    val x = ProtectedIntRef(0, allowUserWrite (alice))(this)
    x.update(bob, 42)
    expect (0) { concretize(alice, x.v) }
    expect (0) { concretize(bob, x.v) }
    expect (0) { concretize(carol, x.v) }
  }

  test ("write selectively allowed for some viewers") {
    val x = ProtectedIntRef(0
              , (ictxt, octxt) => (ictxt === alice) && (octxt === bob))(this)
    x.update(alice, 42)
    expect (0) { concretize(alice, x.v) }
    expect (42) { concretize(bob, x.v) }
    expect (0) { concretize(carol, x.v) }
  }

  test ("permitted writer overwite") {
    val x = ProtectedIntRef(0, allowUserWrite (bob))(this)
    x.update(alice, 42)
    x.update(bob, 43)
    expect (43) { concretize(alice, x.v) }
    expect (43) { concretize(bob, x.v) }
    expect (43) { concretize(carol, x.v) }
  }

  test ("restricted writer overwrite") {
    var x = ProtectedIntRef(0, allowUserWrite (bob))(this)
    x.update(bob, 43)
    x.update(alice, 42)
    expect (43) { concretize(alice, x.v) }
    expect (43) { concretize(bob, x.v) }
    expect (43) { concretize(carol, x.v) }
  }

  test ("output varies depending on who is viewing") {
    val x = ProtectedIntRef(0
            , (ictxt, octxt) => ((ictxt === alice) && (octxt === bob)))(this)
    x.update(alice, 42)
    expect (0) { concretize(alice, x.v) }
    expect (42) { concretize(bob, x.v) }
    expect (0) { concretize(carol, x.v) }
  }

  test ("combining integrity policies in an operation") {
    val x = ProtectedIntRef(0, allowUserWrite (bob))(this)
    x.update(bob, 42)
    val y = ProtectedIntRef(2
            , (ictxt, octxt) => (ictxt === alice) && (octxt === bob))(this)
    y.update(alice, 43)
    expect (44) { concretize(alice, x.v + y.v) }
    expect (85) { concretize(bob, x.v + y.v) }
    expect (44) { concretize(carol, x.v + y.v) }
  }

   /* Alice is allowed to write to x and only Bob is allowed to write to y.
     Our write policies disallow Bob from accidentally writing a value from
     Alice into y. (That is, without an explicit endorsement...) */
  test ("Prevent flow of untrusted writes") {
    val x = ProtectedIntRef(0, allowUserWrite(alice))(this)
    x.update(alice, 42) 
    val y = ProtectedIntRef(1, allowUserWrite(bob))(this)
    y.update(bob, x.v)
    expect (42) { concretize(alice, x.v) }
    expect (42) { concretize(bob, x.v) }
    expect (0) { concretize(alice, y.v) }
    expect (0) { concretize(bob, y.v) }
  }

  test ("Prevent flow of operations on untrusted writes") {
    val x = ProtectedIntRef(0, allowUserWrite(alice))(this)
    x.update(alice, 42)
    val y = ProtectedIntRef(1, allowUserWrite(bob))(this)
    y.update(bob, 43)
    val z = ProtectedIntRef(0, allowUserWrite(carol))(this)
    z.update(carol, x.v + y.v)
    expect (1) { concretize(alice, z.v) }
    expect (1) { concretize(bob, z.v) }
    expect (1) { concretize(carol, z.v) }
  }

  /* Alice is allowed to write to x and only Bob is allowed to write to y.
     Our policy enforcement prevents Alice from influencing values that Bob
     writes. */
  test ("Prevent untrusted writes through implicit flows.") {
    val x = ProtectedIntRef(0, allowUserWrite(alice))(this)
    x.update(alice, 42)
    val y = ProtectedIntRef(1, allowUserWrite(bob))(this)
    y.update(bob, jif (x.v === 42, _ => 2, _ => 3))
    expect (3) { concretize(alice, y.v) }
    expect (3) { concretize(bob, y.v) }
  }

  test ("Prevent implicit flows of confidential values") {
    val x = ProtectedIntRef(0
              , (ictxt, octxt) => ictxt === alice && octxt === alice)(this)
    x.update(alice, 42)
    val y = ProtectedIntRef(1
              , (ictxt, octxt) => ictxt === bob || ictxt === alice)(this)
    y.update(bob, jif(x.v === 42, _ => 2, _ => 3))
    expect (2) { concretize(alice, y.v) }
    expect (3) { concretize(bob, y.v) }
  }

  /* If Alice and Bob are allowed to write to x and y respectively, then
     x + y should be allowed to be written to a value where they are both
     allowed to write. */
  test ("combining values into permissive write") {
    val x = ProtectedIntRef(0, allowUserWrite (bob))(this)
    x.update(bob, 42)
    val y = ProtectedIntRef(1, allowUserWrite (alice))(this)
    y.update(alice, 43)
    val z =
      ProtectedIntRef(0
        , (ictxt, octxt) =>
            (ictxt === alice) || (ictxt === bob) || (ictxt === carol))(this)
    z.update(carol, x.v + y.v)
    expect (85) { concretize(alice, z.v) }
    expect (85) { concretize(bob, z.v) }
    expect (85) { concretize(carol, z.v) }
  }

  // Only bob can see the special value alice wrote to him...
  test ("Combining confidentiality with operations") {
    val x = ProtectedIntRef(0, allowUserWrite (bob))(this)
    x.update(bob, 42)
    val y = ProtectedIntRef(2
            , (ictxt, octxt) => (ictxt === alice) && (octxt === bob))(this)
    y.update(alice, 43)
    val z = ProtectedIntRef(0
            , (ictxt, octxt) =>
              (ictxt === alice) || (ictxt === bob) || (ictxt === carol))(this)
    z.update(carol, x.v + y.v)
    expect (44) { concretize(alice, z.v) }
    expect (85) { concretize(bob, z.v) }
    expect (44) { concretize(carol, z.v) }
  }

  // Since only bob knows that he can write, only he can see his value...
  test ("Integrity policies that involve confidentiality policies") {
    val a = mkLevel ()
    restrict (a, (ctxt: Sensitive) => ctxt === bob)
    val secretWriter: Sensitive = mkSensitive(a, bob, nobody)
    val x = ProtectedIntRef(0
            , (ictxt, octxt) => ictxt === secretWriter)(this)
    x.update(bob, 42)
    expect (bob) { concretize(bob, secretWriter) }
    expect (nobody) { concretize(alice, secretWriter) }
    expect (nobody) { concretize(carol, secretWriter) }
    expect (0) { concretize(alice, x.v) }
    expect (42) { concretize(bob, x.v) }
    expect (0) { concretize(carol, x.v) }
  }

  /* If Alice does something bad, then we will reject all of her influences.
   */
  test ("Determine whether a writer is trusted later") {
    val x = ProtectedIntRef(0, allowUserWrite(alice))(this)
    x.update(alice, 42)
  }

  def id[T](x: T): T = x
  def inc(x: IntExpr): IntExpr = x + 1

  test ("Function facets--allowed to write.") {
    val x =
      ProtectedFunctionRef(FunctionVal(id[IntExpr]_)
        , allowUserWrite(bob))(this)
    expect (1) { concretize(alice, jfun[IntExpr, IntExpr](x.v, 1)) }
    x.update(bob, FunctionVal(inc))
    expect (2) { concretize(alice, jfun[IntExpr, IntExpr](x.v, 1)) }
  }

  test ("Function facets--not allowed to write.") {
    val x =
      ProtectedFunctionRef(FunctionVal(id[IntExpr]_)
        , allowUserWrite(bob))(this)
    expect (1) { concretize(alice, jfun[IntExpr, IntExpr](x.v, 1)) }
    x.update(alice, FunctionVal(inc))
    expect (1) { concretize(alice, jfun[IntExpr, IntExpr](x.v, 1)) }
  }
}
