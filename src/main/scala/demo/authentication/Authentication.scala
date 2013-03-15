package cap.jeeveslib.demo.authentication

import cap.jeeveslib.ast.{Atom, Object, ObjectExpr, ProtectedObjectRef, S, UpdateResult}
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.jeeves._

/**
* Basic authentication.
* @author jeanyang
*/
/**
* Principals and users.
* Because of the way we represent 
*/
class Principal extends Atom
case class User(name: String, private val _pwd: String = "")
  (implicit jlib: JeevesLib[Cred])
  extends Principal {
  private var pwdRef = ProtectedObjectRef[Cred, Cred](S(_pwd)
    , (ictxt, octxt) =>
        (ictxt.name === name) && (ictxt.password === password)
        && (octxt.name === name) && (octxt.password === password)
    , true)(jlib)
  var password: ObjectExpr[S] = pwdRef.v.asInstanceOf[ObjectExpr[S]]
  def setPassword(c: Cred, newPwd: String) = {
    pwdRef.update(c, newPwd)
    password = pwdRef.v.asInstanceOf[ObjectExpr[S]]
  }
}
object Admin extends Principal
case class Cred(p: Principal) extends Atom

object Authentication extends JeevesLib[Cred] {
  val nullUser: User = User("default", "")(this)
  def login (p: Principal, pwd: String): ObjectExpr[Cred] = {
    (p, pwd) match {
      case (u@User(_, _), _) =>
        jif(u.password === S(pwd)
          , _ => Object(Cred(p)), _ => Object(Cred(nullUser)))
      case (Admin, "secret") => Object(Cred(p))
      case _ => Object(Cred(nullUser))
    }
  }

  /**
  * File authentication.
   */
  object FileAC {
    class File(private val _contents: String = "")
      (implicit jlib: JeevesLib[Cred]) extends Atom {
      private val contents = ProtectedObjectRef[Cred, Cred](S(_contents)
        , (ictxt, octxt) =>
          (ictxt.p === Admin), false)(jlib)
      def writeContents(c: Cred, body: String): UpdateResult =
        contents.update(c, S(body))
    }
  }

  /*
  def client (p: Principal, c: Cred, f: FileAC.File): String = {
    p match {
      case Admin => f.showWriteLoc(c)
      case _ => ""
    }
  }
  */
}
