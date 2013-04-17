package test.cap.jeeveslib.jeeves.integrity

import org.scalatest.FunSuite
import org.scalatest.Assertions

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.debug._
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

  def allowUserWrite[T](user: DummyUser)
    : T => OutputWritePolicy[DummyUser, DummyContext] = {
    _this => ictxt => _octxt => ictxt === user
  }

  test ("write allowed for all viewers") {
    val x =
      ProtectedIntRef[DummyUser, DummyContext](
        0, None, Some(allowUserWrite (alice)))(this)
    assert(x.update(alice, aliceContext(), 42) == Success)
    expectResult (42) { concretize(aliceContext(), x.getValue()) }
    expectResult (42) { concretize(bobContext(), x.getValue()) }
    expectResult (42) { concretize(carolContext(), x.getValue()) }
  }

  test ("write disallowed for all viewers") {
    val x = ProtectedIntRef[DummyUser, DummyContext](
      0, None, Some(allowUserWrite (alice)))(this)
    assert(x.update(bob, bobContext(), 42) == Failure)
    expectResult (0) { concretize(aliceContext(), x.getValue()) }
    expectResult (0) { concretize(bobContext(), x.getValue()) }
    expectResult (0) { concretize(carolContext(), x.getValue()) }
  }

  test ("write selectively allowed for some viewers") {
    val x = ProtectedIntRef[DummyUser, DummyContext](0
      , None // (_, ictxt: ObjectExpr[DummyUser]) => (ictxt === alice)
      , Some(
        _this => ictxt => octxt =>
          (ictxt === alice) && octxt.viewer === bob))(this)
    assert(x.update(alice, aliceContext(), 42) == Unresolved)
    expectResult (0) { concretize(aliceContext(), x.getValue()) }
    expectResult (42) { concretize(bobContext(), x.getValue()) }
    expectResult (0) { concretize(carolContext(), x.getValue()) }
  }

  test ("permitted writer overwite") {
    val x = ProtectedIntRef[DummyUser, DummyContext](
              0, None, Some(allowUserWrite (bob)))(this)
    assert(x.update(alice, aliceContext(), 42) == Failure)
    assert(x.update(bob, bobContext(), 43) == Success)
    expectResult (43) { concretize(aliceContext(), x.getValue()) }
    expectResult (43) { concretize(bobContext(), x.getValue()) }
    expectResult (43) { concretize(carolContext(), x.getValue()) }
  }

  test ("restricted writer overwrite") {
    var x = ProtectedIntRef[DummyUser, DummyContext](
              0, None, Some(allowUserWrite (bob)))(this)
    x.update(bob, bobContext(), 43)
    x.update(alice, aliceContext(), 42)
    expectResult (43) { concretize(aliceContext(), x.getValue()) }
    expectResult (43) { concretize(bobContext(), x.getValue()) }
    expectResult (43) { concretize(carolContext(), x.getValue()) }
  }

  test ("output varies depending on who is viewing") {
    val x = ProtectedIntRef[DummyUser, DummyContext](0
      , None // (_, ictxt) => ictxt === alice
      , Some(_this => ictxt => octxt =>
          ictxt === alice && octxt.viewer === bob))(this)
    x.update(alice, aliceContext(), 42)
    expectResult (0) { concretize(aliceContext(), x.getValue()) }
    expectResult (42) { concretize(bobContext(), x.getValue()) }
    expectResult (0) { concretize(carolContext(), x.getValue()) }
  }

  test ("combining integrity policies in an operation") {
    val x = ProtectedIntRef[DummyUser, DummyContext](
              0, None, Some(allowUserWrite (bob)))(this)
    x.update(bob, bobContext(), 42)
    val y = ProtectedIntRef[DummyUser, DummyContext](2
      , None
      , Some(_this => ictxt => octxt =>
          ictxt === alice && octxt.viewer === bob))(this)
    y.update(alice, aliceContext(), 43)
    expectResult (44) {
      concretize(aliceContext(), x.getValue() + y.getValue()) }
    expectResult (85) { concretize(bobContext(), x.getValue() + y.getValue()) }
    expectResult (44) {
      concretize(carolContext(), x.getValue() + y.getValue()) }
  }
  
  /* Alice is allowed to write to x and only Bob is allowed to write to y.
     Our write policies disallow Bob from accidentally writing a value from
     Alice into y. (That is, without an explicit endorsement...) */
  test ("Prevent flow of untrusted writes") {
    val x = ProtectedIntRef[DummyUser, DummyContext](
              0, None, Some(allowUserWrite(alice)), "x")(this)
    x.update(alice, aliceContext(), 42) 
    val y = ProtectedIntRef[DummyUser, DummyContext](
              1, None, Some(allowUserWrite(bob)), "y")(this)
    y.update(bob, bobContext(), x.getValue())
    expectResult (42) { concretize(aliceContext(), x.getValue()) }
    expectResult (42) { concretize(bobContext(), x.getValue()) }
    expectResult (0) { concretize(aliceContext(), y.getValue()) }
    expectResult (0) { concretize(bobContext(), y.getValue()) }
  }

  test ("Prevent flow of operations on untrusted writes") {
    val x = ProtectedIntRef[DummyUser, DummyContext](
              0, None, Some(allowUserWrite(alice)))(this)
    x.update(alice, aliceContext(), 42)
    val y = ProtectedIntRef[DummyUser, DummyContext](
              1, None, Some(allowUserWrite(bob)))(this)
    y.update(bob, aliceContext(), 43)
    val z = ProtectedIntRef[DummyUser, DummyContext](
              0, None, Some(allowUserWrite(carol)))(this)
    z.update(carol, carolContext(), x.getValue() + y.getValue())
    expectResult (1) { concretize(aliceContext(), z.getValue()) }
    expectResult (1) { concretize(bobContext(), z.getValue()) }
    expectResult (1) { concretize(carolContext(), z.getValue()) }
  }

  /* Alice is allowed to write to x and only Bob is allowed to write to y.
     Our policy enforcement prevents Alice from influencing values that Bob
     writes. */
  test ("Prevent untrusted writes through implicit flows.") {
    val x = ProtectedIntRef[DummyUser, DummyContext](
              0, None, Some(allowUserWrite(alice)))(this)
    x.update(alice, aliceContext(), 42)
    val y = ProtectedIntRef[DummyUser, DummyContext](
              1, None, Some(allowUserWrite(bob)))(this)
    y.update(bob, bobContext(), jif (x.getValue() === 42, _ => 2, _ => 3))
    expectResult (3) { concretize(aliceContext(), y.getValue()) }
    expectResult (3) { concretize(bobContext(), y.getValue()) }
  }

  test ("Prevent implicit flows of confidential values") {
    val x = ProtectedIntRef[DummyUser, DummyContext](0, None
      , Some(_this => ictxt => octxt =>
          ictxt === alice && octxt.viewer === alice))(this)
    x.update(alice, aliceContext(), 42)
    val y = ProtectedIntRef[DummyUser, DummyContext](1, None
      , Some(_this => ictxt => octxt => ictxt === bob || ictxt === alice))(this)
    y.update(bob, bobContext(), jif(x.getValue() === 42, _ => 2, _ => 3))
    expectResult (2) { concretize(aliceContext(), y.getValue()) }
    expectResult (3) { concretize(bobContext(), y.getValue()) }
  }

  /* If Alice and Bob are allowed to write to x and y respectively, then
     x + y should be allowed to be written to a value where they are both
     allowed to write. */
  test ("combining values into permissive write") {
    val x = ProtectedIntRef[DummyUser, DummyContext](
              0, None, Some(allowUserWrite (bob)))(this)
    x.update(bob, bobContext(), 42)
    val y = ProtectedIntRef[DummyUser, DummyContext](
              1, None, Some(allowUserWrite (alice)))(this)
    y.update(alice, aliceContext(), 43)
    val z =
      ProtectedIntRef[DummyUser, DummyContext](0, None
      , Some(_this => ictxt => octxt =>
          ictxt === alice || ictxt === bob || ictxt === carol))(this)
    z.update(carol, carolContext(), x.getValue() + y.getValue())
    expectResult (85) { concretize(aliceContext(), z.getValue()) }
    expectResult (85) { concretize(bobContext(), z.getValue()) }
    expectResult (85) { concretize(carolContext(), z.getValue()) }
  }

  // Only bob can see the special value alice wrote to him...
  test ("Combining confidentiality with operations") {
    val x = ProtectedIntRef[DummyUser, DummyContext](
              0, None, Some(allowUserWrite (bob)))(this)
    x.update(bob, bobContext(), 42)
    val y = ProtectedIntRef[DummyUser, DummyContext](2, None
      , Some(_this => ictxt => octxt =>
          ictxt === alice && octxt.viewer === bob))(this)
    y.update(alice, aliceContext(), 43)
    val z = ProtectedIntRef[DummyUser, DummyContext](0, None
      , Some(_this => ictxt => _octxt =>
          ictxt === alice || ictxt === bob || ictxt === carol))(this)
    z.update(carol, carolContext(),  x.getValue() + y.getValue())
    expectResult (44) { concretize(aliceContext(), z.getValue()) }
    expectResult (85) { concretize(bobContext(), z.getValue()) }
    expectResult (44) { concretize(carolContext(), z.getValue()) }
  }

  // Since only bob knows that he can write, only he can see his value...
  test ("Integrity policies that involve confidentiality policies") {
    val a = mkLabel ()
    restrict (a, (ctxt: ObjectExpr[DummyContext]) => ctxt.viewer === bob)
    val secretWriter: ObjectExpr[DummyUser] = mkSensitive(a, bob, nobody)
    val x = ProtectedIntRef[DummyUser, DummyContext](0, None
    , Some(_this => ictxt => octxt => ictxt === secretWriter))(this)
    x.update(bob, bobContext(), 42)
    expectResult (bob) { concretize(bobContext(), secretWriter) }
    expectResult (nobody) { concretize(aliceContext(), secretWriter) }
    expectResult (nobody) { concretize(carolContext(), secretWriter) }
    expectResult (0) { concretize(aliceContext(), x.getValue()) }
    expectResult (42) { concretize(bobContext(), x.getValue()) }
    expectResult (0) { concretize(carolContext(), x.getValue()) }
  }

  /* If Alice does something bad, then we will reject all of her influences.
   */
  test ("Determine whether a writer is trusted later") {
    val x = ProtectedIntRef[DummyUser, DummyContext](0, None
      , Some(_this => ictxt => octxt => (
          octxt.applyFunction((oc: DummyContext) =>
           !Atoms(oc.badUsers).has(ictxt)))))(this)
    x.update(alice, aliceContext(), 42)
    expectResult(0) { concretize(aliceContext(List(alice)), x.getValue()) }
    expectResult(42) { concretize(aliceContext(List()), x.getValue()) }
  }
 
  /* Make sure we can change the input channel type without things breaking. */
  test ("Different types for input channels") {
    val x = ProtectedIntRef[DummyContext, DummyContext](0
      , None, Some(_this => ictxt => _octxt => ictxt.viewer === alice))(this)
    x.update(aliceContext(List()), aliceContext(), 42)
    expectResult(42) { concretize(aliceContext(), x.getValue()) }
  }

  def id[T](x: T): T = x
  def inc(x: IntExpr): IntExpr = x + 1

  test ("Function facets--allowed to write.") {
    val x =
      ProtectedFunctionRef[IntExpr, IntExpr, DummyUser, DummyContext](
        FunctionVal(id[IntExpr]_)
        , None, Some(allowUserWrite(bob)))(this)
    expectResult (1) {
      concretize(aliceContext(), jfun[IntExpr, IntExpr](x.getValue(), 1)) }
    x.update(bob, bobContext(), FunctionVal(inc))
    expectResult (2) {
      concretize(aliceContext(), jfun[IntExpr, IntExpr](x.getValue(), 1)) }
  }

  test ("Function facets--not allowed to write.") {
    val x =
      ProtectedFunctionRef[IntExpr, IntExpr, DummyUser, DummyContext](
        FunctionVal(id[IntExpr]_)
        , None, Some(allowUserWrite(bob)))(this)
    expectResult (1) {
      concretize(aliceContext(), jfun[IntExpr, IntExpr](x.getValue(), 1)) }
    x.update(alice, aliceContext(), FunctionVal(inc))
    expectResult (1) {
      concretize(aliceContext(), jfun[IntExpr, IntExpr](x.getValue(), 1)) }
  }

  test ("Output write policies involving 'this'--cannot update") {
    val x =
      ProtectedIntRef[DummyUser, DummyContext](0
      , None, Some(v => ictxt => _ => !(v === 3) && ictxt === alice))(this)
    x.update(alice, aliceContext(), 1)
    expectResult(1) {
      concretize(aliceContext(), x.getValue())
    }
    x.update(alice, aliceContext(), 3)
    expectResult(3) {
      concretize(aliceContext(), x.getValue())
    }
    x.update(alice, aliceContext(), 5)
    println(x.getValue())
    DebugPrint.debugPrint(aliceContext(), x.getValue())(this)
    expectResult(3) {
      concretize(aliceContext(), x.getValue())
    }
  }

  // TODO: Test object refs...
}
