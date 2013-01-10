package test.cap.jeeves.expressiveness

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expectResult}
import scala.collection.immutable.Map

import cap.jeeveslib.ast.{Atom, ObjectExpr, S}
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.jeeves._

/**
 * Based on the Fine benchmarks from PLDI '10.
 * @author jeanyang
 */
  /**
   * Principals, users, and credentials.
   */
   object Authentication {
     class Principal extends Atom
     case class User(name: String) extends Principal
     object Admin extends Principal

     case class Cred(p: Principal) extends Atom
     case class AuthContext(prin: Principal, cred: Cred) extends Atom

     val aliceUser = User("Alice")

     def login (p: Principal, pw: String): Option[Cred] = {
       (p, pw) match {
         case (aliceUser, "APassword") => Some(Cred(p))
           case (Admin, "Secret") => Some(Cred(p))
           case _ => None
         }
       }
    }

class Authentication extends FunSuite
  with JeevesLib[Authentication.AuthContext] {
  /**
   * File authentication.
   */
  object FileAC {
    class File(private val _loc: String) extends Atom {
      // File read location.
      val canWrite = mkLevel ()
      restrict (canWrite
      , (ctxt: ObjectExpr[Authentication.AuthContext]) =>
        ((ctxt.prin === Authentication.Admin)
              && (ctxt.cred.p === ctxt.prin)))
      def getWriteLoc () = mkSensitive(canWrite, _loc, "")
      def showWriteLoc (ctxt: Authentication.AuthContext): String =
        concretize(ctxt, getWriteLoc ()).asInstanceOf[S].s
    }
  }

  def client (p: Authentication.Principal, c: Authentication.Cred
    , f: FileAC.File): String = {
    p match {
      case Authentication.Admin =>
        f.showWriteLoc(Authentication.AuthContext(p, c))
      case _ => ""
    }
  }

  import Authentication._;
  import FileAC._;
  test ("login") {
    expectResult(Some(Cred(aliceUser))) { login(aliceUser, "APassword"); }
    expectResult(None) { login(aliceUser, "otherpwd"); }
    expectResult(Some(Cred(Admin))) { login(Admin, "Secret"); }
    expectResult(None) { login(Admin, "other") }
  }

  test("file write location") {
    val file = new File("file.txt");
    expectResult("") {
      file.showWriteLoc(AuthContext(aliceUser, Cred(aliceUser)))
    }
    expectResult("") {
      file.showWriteLoc(AuthContext(aliceUser, Cred(Admin)))
    }
    expectResult("file.txt") {
      file.showWriteLoc(AuthContext(Admin, Cred(Admin)))
    }
    expectResult("") {
      file.showWriteLoc(AuthContext(Admin, Cred(aliceUser)))
    }
  }
}
