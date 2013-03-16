package test.cap.jeeves.expressiveness

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expectResult}
import scala.collection.immutable.Map

import cap.jeeveslib.ast._
import cap.jeeveslib.jeeves._
import cap.jeeveslib.demo.authentication._

class TestAuthentication extends FunSuite {
  val alicePwd = "alicePwd"
  val bobPwd = "bobPwd"

  val aliceUser = User(1, "Alice", alicePwd)(Authentication)
  val bobUser = User(2, "Bob", bobPwd)(Authentication)

  val aliceCred = Authentication.login(aliceUser, "alicePwd")
  val bobCred = Authentication.login(bobUser, "bobPwd")

  val file = new Authentication.File(aliceUser, "hello world")(Authentication)

  test ("user can see own password") {
    expectResult(S(alicePwd)) {
      Authentication.concretize(Cred(aliceUser), aliceUser.password)
    }
    expectResult(S(alicePwd)) {
      Authentication.concretize(
        Cred(User(1, "Alice", alicePwd)(Authentication))
        , aliceUser.password)
    }
    expectResult(S(bobPwd)) {
      Authentication.concretize(Cred(bobUser), bobUser.password)
    }
  }
  
  test ("user cannot see other password") {
    expectResult(S("")) {
      Authentication.concretize(Cred(aliceUser), bobUser.password) }
    expectResult(S("")) {
      Authentication.concretize(Cred(bobUser), aliceUser.password) }
  }

  test ("user must supply correct password") {
    expectResult(S("")) {
      Authentication.concretize(
        Cred(User(1, "Alice", "alice!")(Authentication)), aliceUser.password)
    }
  }

  test ("other user cannot update password") {
    aliceUser.setPassword(Cred(bobUser), bobPwd)
    expectResult(S(alicePwd)) {
      Authentication.concretize(Cred(aliceUser), aliceUser.password)
    }
    expectResult(S("")) {
      Authentication.concretize(Cred(bobUser), aliceUser.password)
    }
  }
  test ("user can update own password") {
    aliceUser.setPassword(Cred(aliceUser), "alicePwd2")
    expectResult(S("alicePwd2")) {
      Authentication.concretize(Cred(aliceUser), aliceUser.password)
    }
    aliceUser.setPassword(Cred(aliceUser), alicePwd)
    expectResult(S("")) {
      Authentication.concretize(Cred(bobUser), aliceUser.password)
    }
  }

  test ("login") {
    expectResult(Cred(aliceUser)) {
      Authentication.concretize(
        Cred(aliceUser), Authentication.login(aliceUser, "alicePwd"));
    }
    expectResult(Cred(NullUser)) {
      Authentication.concretize(
        Cred(aliceUser), Authentication.login(aliceUser, "otherpwd"));
    }
    expectResult(Cred(Authentication.admin)) {
      Authentication.concretize(
        Cred(Authentication.admin)
        , Authentication.login(Authentication.admin, "secret"));
    }
    expectResult(Cred(NullUser)) {
      Authentication.concretize(
        Cred(Authentication.admin)
        , Authentication.login(Authentication.admin, "other"))
    }
  }

  test("everyone can see the file") {
    expectResult(S("hello world")) {
      Authentication.concretize(Cred(Authentication.admin), file.getContents())
    }
    expectResult(S("hello world")) {
      Authentication.concretize(Cred(aliceUser), file.getContents())
    }
    expectResult(S("hello world")) {
      Authentication.concretize(Cred(bobUser), file.getContents())
    }
  }

  test("using login credential to write file") {
    val aliceWrite = file.writeContents(aliceCred, "alice was here")

    expectResult(Unresolved) {
      Authentication.concretize(aliceCred, aliceWrite)
    }
    /*
    expectResult(S("alice was here")) {
      Authentication.concretize(Cred(Authentication.admin), file.getContents())
    }
    */
    expectResult(S("alice was here")) {
      Authentication.concretize(aliceCred, file.getContents())
    }
    expectResult(S("alice was here")) {
      Authentication.concretize(Cred(bobUser), file.getContents())
    }
    /*
    expectResult(Failure) {
      Authentication.concretize(bobCred
        , file.writeContents(bobCred, "bob was here"))
    }
    expectResult(S("alice was here")) {
      Authentication.concretize(aliceCred, file.getContents())
    }
    expectResult(S("alice was here")) {
      Authentication.concretize(bobCred, file.getContents())
    }
    */
  }
}