package test.cap.jeeveslib.jeeves.integrity

import org.scalatest.FunSuite
import org.scalatest.Assertions
import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.jeeves._

case class DummyUser(id: BigInt) extends Atom
case class DummyContext(viewer: DummyUser, badUsers: List[DummyUser])
  extends Atom
class TestIntegrity extends FunSuite with JeevesLib[DummyContext] {
  val nobody = DummyUser(-1);
  val alice = DummyUser(0);
  val bob = DummyUser(1);
  val carol = DummyUser(2);

  def aliceContext(badUsers: List[DummyUser] = List()) =
    DummyContext(alice, badUsers)
  def bobContext(badUsers: List[DummyUser] = List()) =
    DummyContext(bob, badUsers)
  def carolContext(badUsers: List[DummyUser] = List()) =
    DummyContext(carol, badUsers)

  def allowUserWrite (user: DummyUser)
    : (ObjectExpr[DummyUser], ObjectExpr[DummyContext]) => Formula = {
    (ictxt, otxt) => ictxt === user
  }

  test ("write allowed for all viewers") {
    val x =
      ProtectedIntRef[DummyUser, DummyContext](0, allowUserWrite (alice))(this)
    x.update(alice, 42)
    expectResult (42) { concretize(aliceContext(), x.v) }
    expectResult (42) { concretize(bobContext(), x.v) }
    expectResult (42) { concretize(carolContext(), x.v) }
  }

  test ("write disallowed for all viewers") {
    val x = ProtectedIntRef(0, allowUserWrite (alice))(this)
    x.update(bob, 42)
    expectResult (0) { concretize(aliceContext(), x.v) }
    expectResult (0) { concretize(bobContext(), x.v) }
    expectResult (0) { concretize(carolContext(), x.v) }
  }

  test ("write selectively allowed for some viewers") {
    val x = ProtectedIntRef(0
              , (ictxt: ObjectExpr[DummyUser]
                , octxt: ObjectExpr[DummyContext]) =>
                  (ictxt === alice && (octxt.viewer === bob)))(this)
    x.update(alice, 42)
    expectResult (0) { concretize(aliceContext(), x.v) }
    expectResult (42) { concretize(bobContext(), x.v) }
    expectResult (0) { concretize(carolContext(), x.v) }
  }

  test ("permitted writer overwite") {
    val x = ProtectedIntRef[DummyUser, DummyContext](
              0, allowUserWrite (bob))(this)
    x.update(alice, 42)
    x.update(bob, 43)
    expectResult (43) { concretize(aliceContext(), x.v) }
    expectResult (43) { concretize(bobContext(), x.v) }
    expectResult (43) { concretize(carolContext(), x.v) }
  }

  test ("restricted writer overwrite") {
    var x = ProtectedIntRef(0, allowUserWrite (bob))(this)
    x.update(bob, 43)
    x.update(alice, 42)
    expectResult (43) { concretize(aliceContext(), x.v) }
    expectResult (43) { concretize(bobContext(), x.v) }
    expectResult (43) { concretize(carolContext(), x.v) }
  }

  test ("output varies depending on who is viewing") {
    val x = ProtectedIntRef[DummyUser, DummyContext](0
            , (ictxt, octxt) =>
                (ictxt === alice && (octxt.viewer === bob)))(this)
    x.update(alice, 42)
    expectResult (0) { concretize(aliceContext(), x.v) }
    expectResult (42) { concretize(bobContext(), x.v) }
    expectResult (0) { concretize(carolContext(), x.v) }
  }

  test ("combining integrity policies in an operation") {
    val x = ProtectedIntRef(0, allowUserWrite (bob))(this)
    x.update(bob, 42)
    val y = ProtectedIntRef[DummyUser, DummyContext](2
            , (ictxt, octxt) =>
                ictxt === alice && octxt.viewer === bob)(this)
    y.update(alice, 43)
    expectResult (44) { concretize(aliceContext(), x.v + y.v) }
    expectResult (85) { concretize(bobContext(), x.v + y.v) }
    expectResult (44) { concretize(carolContext(), x.v + y.v) }
  }

   /* Alice is allowed to write to x and only Bob is allowed to write to y.
     Our write policies disallow Bob from accidentally writing a value from
     Alice into y. (That is, without an explicit endorsement...) */
  test ("Prevent flow of untrusted writes") {
    val x = ProtectedIntRef(0, allowUserWrite(alice))(this)
    x.update(alice, 42) 
    val y = ProtectedIntRef(1, allowUserWrite(bob))(this)
    y.update(bob, x.v)
    expectResult (42) { concretize(aliceContext(), x.v) }
    expectResult (42) { concretize(bobContext(), x.v) }
    expectResult (0) { concretize(aliceContext(), y.v) }
    expectResult (0) { concretize(bobContext(), y.v) }
  }

  test ("Prevent flow of operations on untrusted writes") {
    val x = ProtectedIntRef(0, allowUserWrite(alice))(this)
    x.update(alice, 42)
    val y = ProtectedIntRef(1, allowUserWrite(bob))(this)
    y.update(bob, 43)
    val z = ProtectedIntRef(0, allowUserWrite(carol))(this)
    z.update(carol, x.v + y.v)
    expectResult (1) { concretize(aliceContext(), z.v) }
    expectResult (1) { concretize(bobContext(), z.v) }
    expectResult (1) { concretize(carolContext(), z.v) }
  }

  /* Alice is allowed to write to x and only Bob is allowed to write to y.
     Our policy enforcement prevents Alice from influencing values that Bob
     writes. */
  test ("Prevent untrusted writes through implicit flows.") {
    val x = ProtectedIntRef(0, allowUserWrite(alice))(this)
    x.update(alice, 42)
    val y = ProtectedIntRef(1, allowUserWrite(bob))(this)
    y.update(bob, jif (x.v === 42, _ => 2, _ => 3))
    expectResult (3) { concretize(aliceContext(), y.v) }
    expectResult (3) { concretize(bobContext(), y.v) }
  }

  test ("Prevent implicit flows of confidential values") {
    val x = ProtectedIntRef[DummyUser, DummyContext](0
              , (ictxt, octxt) =>
                  ictxt === alice && octxt.viewer === alice)(this)
    x.update(alice, 42)
    val y = ProtectedIntRef[DummyUser, DummyContext](1
              , (ictxt, octxt) =>
                  ictxt === bob || ictxt === alice)(this)
    y.update(bob, jif(x.v === 42, _ => 2, _ => 3))
    expectResult (2) { concretize(aliceContext(), y.v) }
    expectResult (3) { concretize(bobContext(), y.v) }
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
      ProtectedIntRef[DummyUser, DummyContext](0
        , (ictxt, octxt) =>
            ictxt === alice || ictxt === bob || ictxt === carol)(
            this)
    z.update(carol, x.v + y.v)
    expectResult (85) { concretize(aliceContext(), z.v) }
    expectResult (85) { concretize(bobContext(), z.v) }
    expectResult (85) { concretize(carolContext(), z.v) }
  }

  // Only bob can see the special value alice wrote to him...
  test ("Combining confidentiality with operations") {
    val x = ProtectedIntRef(0, allowUserWrite (bob))(this)
    x.update(bob, 42)
    val y = ProtectedIntRef[DummyUser, DummyContext](2
            , (ictxt, octxt) =>
                ictxt === alice && octxt.viewer === bob)(this)
    y.update(alice, 43)
    val z = ProtectedIntRef[DummyUser, DummyContext](0
            , (ictxt, octxt) =>
              ictxt === alice || ictxt === bob || ictxt === carol)(this)
    z.update(carol, x.v + y.v)
    expectResult (44) { concretize(aliceContext(), z.v) }
    expectResult (85) { concretize(bobContext(), z.v) }
    expectResult (44) { concretize(carolContext(), z.v) }
  }

  // Since only bob knows that he can write, only he can see his value...
  test ("Integrity policies that involve confidentiality policies") {
    val a = mkLevel ()
    restrict (a, (ctxt: ObjectExpr[DummyContext]) => ctxt.viewer === bob)
    val secretWriter: ObjectExpr[DummyUser] = mkSensitive(a, bob, nobody)
    val x = ProtectedIntRef[DummyUser, DummyContext](0
            , (ictxt, octxt) => ictxt === secretWriter)(this)
    x.update(bob, 42)
    expectResult (bob) { concretize(bobContext(), secretWriter) }
    expectResult (nobody) { concretize(aliceContext(), secretWriter) }
    expectResult (nobody) { concretize(carolContext(), secretWriter) }
    expectResult (0) { concretize(aliceContext(), x.v) }
    expectResult (42) { concretize(bobContext(), x.v) }
    expectResult (0) { concretize(carolContext(), x.v) }
  }

  /* If Alice does something bad, then we will reject all of her influences.
   */
  test ("Determine whether a writer is trusted later") {
    val x = ProtectedIntRef[DummyUser, DummyContext](0
      , (ictxt, octxt) => (
        octxt.applyFunction((oc: DummyContext) =>
           !Atoms(oc.badUsers).has(ictxt))))(this)
    x.update(alice, 42)
    expectResult(0) { concretize(aliceContext(List(alice)), x.v) }
    expectResult(42) { concretize(aliceContext(List()), x.v) }
  }
  
  def id[T](x: T): T = x
  def inc(x: IntExpr): IntExpr = x + 1

  test ("Function facets--allowed to write.") {
    val x =
      ProtectedFunctionRef[IntExpr, IntExpr, DummyUser, DummyContext](
        FunctionVal(id[IntExpr]_)
        , allowUserWrite(bob))(this)
    expectResult (1) {
      concretize(aliceContext(), jfun[IntExpr, IntExpr](x.v, 1)) }
    x.update(bob, FunctionVal(inc))
    expectResult (2) {
      concretize(aliceContext(), jfun[IntExpr, IntExpr](x.v, 1)) }
  }

  test ("Function facets--not allowed to write.") {
    val x =
      ProtectedFunctionRef[IntExpr, IntExpr, DummyUser, DummyContext](
        FunctionVal(id[IntExpr]_)
        , allowUserWrite(bob))(this)
    expectResult (1) {
      concretize(aliceContext(), jfun[IntExpr, IntExpr](x.v, 1)) }
    x.update(alice, FunctionVal(inc))
    expectResult (1) {
      concretize(aliceContext(), jfun[IntExpr, IntExpr](x.v, 1)) }
  }
}
