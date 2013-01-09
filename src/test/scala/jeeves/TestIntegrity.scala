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

  def allowUserWrite (user: DummyUser)
    : (DummyUser, ObjectExpr[DummyUser]) => Formula = {
    (ictxt, otxt) => ictxt == user
  }

  test ("write allowed for all viewers") {
    val x =
      ProtectedIntRef[DummyUser, DummyUser](0, allowUserWrite (alice))(this)
    x.update(alice, 42)
    expectResult (42) { concretize(alice, x.v) }
    expectResult (42) { concretize(bob, x.v) }
    expectResult (42) { concretize(carol, x.v) }
  }

  test ("write disallowed for all viewers") {
    val x = ProtectedIntRef(0, allowUserWrite (alice))(this)
    x.update(bob, 42)
    expectResult (0) { concretize(alice, x.v) }
    expectResult (0) { concretize(bob, x.v) }
    expectResult (0) { concretize(carol, x.v) }
  }

  test ("write selectively allowed for some viewers") {
    val x = ProtectedIntRef(0
              , (ictxt: DummyUser, octxt: ObjectExpr[DummyUser]) =>
                  (BoolVal(ictxt == alice) && (octxt === bob)))(this)
    x.update(alice, 42)
    expectResult (0) { concretize(alice, x.v) }
    expectResult (42) { concretize(bob, x.v) }
    expectResult (0) { concretize(carol, x.v) }
  }

  test ("permitted writer overwite") {
    val x = ProtectedIntRef[DummyUser, DummyUser](0, allowUserWrite (bob))(this)
    x.update(alice, 42)
    x.update(bob, 43)
    expectResult (43) { concretize(alice, x.v) }
    expectResult (43) { concretize(bob, x.v) }
    expectResult (43) { concretize(carol, x.v) }
  }

  test ("restricted writer overwrite") {
    var x = ProtectedIntRef(0, allowUserWrite (bob))(this)
    x.update(bob, 43)
    x.update(alice, 42)
    expectResult (43) { concretize(alice, x.v) }
    expectResult (43) { concretize(bob, x.v) }
    expectResult (43) { concretize(carol, x.v) }
  }

  test ("output varies depending on who is viewing") {
    val x = ProtectedIntRef[DummyUser, DummyUser](0
            , (ictxt, octxt) =>
                (BoolVal(ictxt == alice) && (octxt === bob)))(this)
    x.update(alice, 42)
    expectResult (0) { concretize(alice, x.v) }
    expectResult (42) { concretize(bob, x.v) }
    expectResult (0) { concretize(carol, x.v) }
  }

  test ("combining integrity policies in an operation") {
    val x = ProtectedIntRef(0, allowUserWrite (bob))(this)
    x.update(bob, 42)
    val y = ProtectedIntRef[DummyUser, DummyUser](2
            , (ictxt, octxt) =>
                BoolVal(ictxt == alice) && (octxt === bob))(this)
    y.update(alice, 43)
    expectResult (44) { concretize(alice, x.v + y.v) }
    expectResult (85) { concretize(bob, x.v + y.v) }
    expectResult (44) { concretize(carol, x.v + y.v) }
  }

   /* Alice is allowed to write to x and only Bob is allowed to write to y.
     Our write policies disallow Bob from accidentally writing a value from
     Alice into y. (That is, without an explicit endorsement...) */
  test ("Prevent flow of untrusted writes") {
    val x = ProtectedIntRef(0, allowUserWrite(alice))(this)
    x.update(alice, 42) 
    val y = ProtectedIntRef(1, allowUserWrite(bob))(this)
    y.update(bob, x.v)
    expectResult (42) { concretize(alice, x.v) }
    expectResult (42) { concretize(bob, x.v) }
    expectResult (0) { concretize(alice, y.v) }
    expectResult (0) { concretize(bob, y.v) }
  }

  test ("Prevent flow of operations on untrusted writes") {
    val x = ProtectedIntRef(0, allowUserWrite(alice))(this)
    x.update(alice, 42)
    val y = ProtectedIntRef(1, allowUserWrite(bob))(this)
    y.update(bob, 43)
    val z = ProtectedIntRef(0, allowUserWrite(carol))(this)
    z.update(carol, x.v + y.v)
    expectResult (1) { concretize(alice, z.v) }
    expectResult (1) { concretize(bob, z.v) }
    expectResult (1) { concretize(carol, z.v) }
  }

  /* Alice is allowed to write to x and only Bob is allowed to write to y.
     Our policy enforcement prevents Alice from influencing values that Bob
     writes. */
  test ("Prevent untrusted writes through implicit flows.") {
    val x = ProtectedIntRef(0, allowUserWrite(alice))(this)
    x.update(alice, 42)
    val y = ProtectedIntRef(1, allowUserWrite(bob))(this)
    y.update(bob, jif (x.v === 42, _ => 2, _ => 3))
    expectResult (3) { concretize(alice, y.v) }
    expectResult (3) { concretize(bob, y.v) }
  }

  test ("Prevent implicit flows of confidential values") {
    val x = ProtectedIntRef[DummyUser, DummyUser](0
              , (ictxt, octxt) =>
                  BoolVal(ictxt == alice) && octxt === alice)(this)
    x.update(alice, 42)
    val y = ProtectedIntRef[DummyUser, DummyUser](1
              , (ictxt, octxt) =>
                  BoolVal(ictxt == bob || ictxt == alice))(this)
    y.update(bob, jif(x.v === 42, _ => 2, _ => 3))
    expectResult (2) { concretize(alice, y.v) }
    expectResult (3) { concretize(bob, y.v) }
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
      ProtectedIntRef[DummyUser, DummyUser](0
        , (ictxt, octxt) =>
            BoolVal(ictxt == alice || ictxt == bob || ictxt == carol))(
            this)
    z.update(carol, x.v + y.v)
    expectResult (85) { concretize(alice, z.v) }
    expectResult (85) { concretize(bob, z.v) }
    expectResult (85) { concretize(carol, z.v) }
  }

  // Only bob can see the special value alice wrote to him...
  test ("Combining confidentiality with operations") {
    val x = ProtectedIntRef(0, allowUserWrite (bob))(this)
    x.update(bob, 42)
    val y = ProtectedIntRef[DummyUser, DummyUser](2
            , (ictxt, octxt) =>
                BoolVal(ictxt == alice) && (octxt === bob))(this)
    y.update(alice, 43)
    val z = ProtectedIntRef[DummyUser, DummyUser](0
            , (ictxt, octxt) =>
              BoolVal(ictxt == alice || ictxt == bob || ictxt == carol))(this)
    z.update(carol, x.v + y.v)
    expectResult (44) { concretize(alice, z.v) }
    expectResult (85) { concretize(bob, z.v) }
    expectResult (44) { concretize(carol, z.v) }
  }

  // Since only bob knows that he can write, only he can see his value...
  test ("Integrity policies that involve confidentiality policies") {
    val a = mkLevel ()
    restrict (a, (ctxt: ObjectExpr[DummyUser]) => ctxt === bob)
    val secretWriter: ObjectExpr[DummyUser] = mkSensitive(a, bob, nobody)
    val x = ProtectedIntRef[DummyUser, DummyUser](0
            , (ictxt, octxt) => Object(ictxt) === secretWriter)(this)
    x.update(bob, 42)
    expectResult (bob) { concretize(bob, secretWriter) }
    expectResult (nobody) { concretize(alice, secretWriter) }
    expectResult (nobody) { concretize(carol, secretWriter) }
    expectResult (0) { concretize(alice, x.v) }
    expectResult (42) { concretize(bob, x.v) }
    expectResult (0) { concretize(carol, x.v) }
  }

  /* If Alice does something bad, then we will reject all of her influences.
   */
  case class DummyContext(badUsers: List[DummyUser]) extends Atom
  test ("Determine whether a writer is trusted later") {
    val x = ProtectedIntRef[DummyUser, DummyContext](0
      , (ictxt, octxt) => (
        octxt.applyFunction((oc: DummyContext) =>
          !oc.badUsers.contains(ictxt))))(this)
    x.update(alice, 42)
    expectResult(0) { concretize(DummyContext(List(alice)), x.v) }
    expectResult(42) { concretize(DummyContext(List()), x.v) }
  }
  
  def id[T](x: T): T = x
  def inc(x: IntExpr): IntExpr = x + 1

  test ("Function facets--allowed to write.") {
    val x =
      ProtectedFunctionRef[IntExpr, IntExpr, DummyUser, DummyUser](
        FunctionVal(id[IntExpr]_)
        , allowUserWrite(bob))(this)
    expectResult (1) { concretize(alice, jfun[IntExpr, IntExpr](x.v, 1)) }
    x.update(bob, FunctionVal(inc))
    expectResult (2) { concretize(alice, jfun[IntExpr, IntExpr](x.v, 1)) }
  }

  test ("Function facets--not allowed to write.") {
    val x =
      ProtectedFunctionRef[IntExpr, IntExpr, DummyUser, DummyUser](
        FunctionVal(id[IntExpr]_)
        , allowUserWrite(bob))(this)
    expectResult (1) { concretize(alice, jfun[IntExpr, IntExpr](x.v, 1)) }
    x.update(alice, FunctionVal(inc))
    expectResult (1) { concretize(alice, jfun[IntExpr, IntExpr](x.v, 1)) }
  }
}
