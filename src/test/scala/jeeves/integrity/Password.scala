package test.cap.jeeveslib.jeeves.integrity.password

import org.scalatest.FunSuite
import org.scalatest.Assertions
import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.jeeves._

/**
  With confidentiality policies only...
 */
case class CUser(id: BigInt, private val _pwd: String = "")
  (implicit jlib: JeevesLib[CUser]) extends Atom {
  // Only people who specify the right password can see the password...
  val a = jlib.mkLevel()
  jlib.restrict(a, (ctxt: ObjectExpr[CUser]) => ctxt === this)

  val pwd: ObjectExpr[S] = { jlib.mkSensitive(a, S(_pwd), S("")) }
}
class PasswordConfidentiality extends FunSuite with JeevesLib[CUser] {
  val alice = CUser(0, "alicePwd")(this)
  val bob = CUser(1, "bobPwd")(this)

  test ("user can see own password") {
    expectResult(S("alicePwd")) { concretize(alice, alice.pwd) }
    expectResult(S("bobPwd")) { concretize(bob, bob.pwd) }
  }
  test ("user cannot see other password") {
    expectResult(S("")) { concretize(alice, bob.pwd) }
    expectResult(S("")) { concretize(bob, alice.pwd) }
  }
}

/**
 * With integrity policies as well...
 */
case class IUser(id: BigInt, private val _pwd: String = "")
  (implicit jlib: JeevesLib[IUser]) extends Atom {
  val a = jlib.mkLevel()
  jlib.restrict(a, (ctxt: ObjectExpr[IUser]) => ctxt === this)

  private var pwdRef = ProtectedObjectRef[IUser, IUser](S(_pwd)
    , (ictxt, octxt) => ictxt === this
    , false)
  def getPassword() = {
    jlib.mkSensitive(a, pwdRef.v, S(""))
  }
  def setPassword(user: IUser, newPwd: String) = {
    pwdRef.update(user, newPwd)
  }
}
class PasswordIntegrity extends FunSuite with JeevesLib[IUser] {
  val alice = IUser(0, "alicePwd")(this)
  val bob = IUser(1, "bobPwd")(this)

  test ("user can see own password") {
    expectResult(S("alicePwd")) { concretize(alice, alice.getPassword()) }
    expectResult(S("bobPwd")) { concretize(bob, bob.getPassword()) }
  } 
  test ("user cannot see other password") {
    expectResult(S("")) { concretize(alice, bob.getPassword()) }
    expectResult(S("")) { concretize(bob, alice.getPassword()) }
  }
  test ("other user cannot update password") {
    alice.setPassword(bob, "bobPwd")
    expectResult(S("alicePwd")) { concretize(alice, alice.getPassword()) }
    expectResult(S("")) { concretize(bob, alice.getPassword()) }
  }
  test ("user can update own password") {
    alice.setPassword(alice, "alicePwd2")
    expectResult(S("alicePwd2")) { concretize(alice, alice.getPassword()) }
    expectResult(S("")) { concretize(bob, alice.getPassword()) }
  }
}
