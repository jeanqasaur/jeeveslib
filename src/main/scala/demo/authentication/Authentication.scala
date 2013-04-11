package cap.jeeveslib.demo.authentication

import cap.jeeveslib.ast.{Atom, BoolVal, Object, ObjectExpr, ProtectedObjectRef, S, UpdateResult}
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.jeeves._

/**
* Authentication example.
* @author jeanyang
*/
class Principal extends Atom
case class User(id: BigInt, name: String, private val _pwd: String = "")
  (implicit jlib: JeevesLib[Cred]) extends Principal {
  // Initial password: the initial password is a sensitive value that only
  // the current user can see.
  private val pwdLabel = jlib.mkLabel("userPwd")
  jlib.restrict(pwdLabel
    , (ctxt: ObjectExpr[Cred]) => ctxt.p === this)
  val password: ObjectExpr[S] = jlib.mkSensitive(pwdLabel, S(_pwd), S(""))

  // Password update: the password is protected by a write policy for integrity
  // that says only the current user can update the password value.
  // This policy also encapsulates the confidentiality that only the current
  // user can see the result of the update.
  val pwdRef = ProtectedObjectRef[S, Cred, Cred](password
    , None
    , Some(_this =>ictxt => octxt =>
          ictxt.p === this && octxt.p === this)
    , "pwdRef")(jlib)
  def setPassword(c: ObjectExpr[Cred], newPwd: String) = {
    pwdRef.update(c, c, newPwd)
  }
}
case class Admin()(implicit jlib: JeevesLib[Cred]) extends Principal {
  // Initial password.
  private val pwdLabel = jlib.mkLabel("adminPwd")
  jlib.restrict(pwdLabel
    , (ctxt: ObjectExpr[Cred]) =>
    ctxt.p === this && ctxt.p.password === password)
  val password: ObjectExpr[S] = jlib.mkSensitive(pwdLabel, S("secret"), S(""))
  }
case object NullUser extends Principal

case class Cred(p: Principal) extends Atom

object Authentication extends JeevesLib[Cred] {
  val admin = Admin()(this)

  // File class where the contents is a protected reference cell.
  class File(val owner: Principal, private val _contents: String = "")
  (implicit jlib: JeevesLib[Cred]) extends Atom {
    private val contentsRef = ProtectedObjectRef[S, Cred, Cred](S(_contents)
      , Some((_, ictxt) => (ictxt.p === owner) || (ictxt.p === admin))
      , None
      , "contentsRef")(jlib)
    def writeContents(c: ObjectExpr[Cred], body: String): UpdateResult = 
      contentsRef.update(c, c, S(body))
    def getContents(): ObjectExpr[S] = contentsRef.getValue()
  }

  // No declassication is needed for the login function!
  def login (p: Principal, pwd: String): ObjectExpr[Cred] = {
    p match {
      case u:User =>
      jif(u.pwdRef.getValue() === S(pwd)
        , _ => Object(Cred(p)), _ => Object(Cred(NullUser)))
      case a:Admin =>
      jif(a.password === S(pwd)
        , _ => Object(Cred(p)), _ => Object(Cred(NullUser)))
      case _ => Object(Cred(NullUser))
    }
  }
}
