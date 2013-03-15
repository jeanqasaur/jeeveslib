package test.cap.jeeves.expressiveness

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expectResult}
import scala.collection.immutable.Map

import cap.jeeveslib.ast._
import cap.jeeveslib.jeeves._
import cap.jeeveslib.demo.authentication._

class TestAuthentication extends FunSuite {
  val aliceUser = User("Alice", "APassword")(Authentication)

  test ("login") {
    expectResult(Cred(aliceUser)) {
      Authentication.concretize(
        Cred(aliceUser), Authentication.login(aliceUser, "APassword"));
    }
    expectResult(Cred(Authentication.nullUser)) {
      Authentication.concretize(
        Cred(aliceUser), Authentication.login(aliceUser, "otherpwd"));
    }
    expectResult(Cred(Admin)) {
      Authentication.concretize(
        Cred(Admin), Authentication.login(Admin, "secret"));
    }
    expectResult(Cred(Authentication.nullUser)) {
      Authentication.concretize(
        Cred(Admin), Authentication.login(Admin, "other"))
    }
  }

  test ("password update") {
    
  }

  test("file write location") {
    val file = new Authentication.FileAC.File("file.txt")(Authentication);
    expectResult(Failure) {
      file.writeContents(Cred(aliceUser), "")
    }
    expectResult(Success) {
      file.writeContents(Cred(Admin), "")
    }
  }
}
