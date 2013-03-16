package cap.jeeveslib.demo.authentication

import cap.jeeveslib.ast.{Atom, Object, ObjectExpr, ProtectedObjectRef, S, UpdateResult}
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.jeeves._

/**
* Basic authentication.
* @author jeanyang
*/
class Principal extends Atom
case class User(id: BigInt, name: String, private val _pwd: String = "")
  (implicit jlib: JeevesLib[Cred]) extends Principal {
  // Initial password.
  private val pwdLabel = jlib.mkLabel()
  jlib.restrict(pwdLabel
    , (ctxt: ObjectExpr[Cred]) =>
    ctxt.p~'id === id && ctxt.p.password === password)
  var password: ObjectExpr[S] = jlib.mkSensitive(pwdLabel, S(_pwd), S(""))

    // Password update.
  private var pwdRef = ProtectedObjectRef[Cred, Cred](password
    , ictxt => octxt =>
    (ictxt.p~'id === id) && (ictxt.p.password === password)
    && (octxt.p~'id === id) && (octxt.p.password === password)
    , true)(jlib)
  def setPassword(c: Cred, newPwd: String) = {
    pwdRef.update(c, newPwd)
    password = pwdRef.v.asInstanceOf[ObjectExpr[S]]
  }
}
case class Admin()(implicit jlib: JeevesLib[Cred]) extends Principal {
  // Initial password.
  private val pwdLabel = jlib.mkLabel()
  jlib.restrict(pwdLabel
    , (ctxt: ObjectExpr[Cred]) =>
    ctxt.p === this && ctxt.p.password === password)
  val password: ObjectExpr[S] = jlib.mkSensitive(pwdLabel, S("secret"), S(""))
  }
case object NullUser extends Principal

case class Cred(p: Principal) extends Atom

object Authentication extends JeevesLib[Cred] {
  val admin = Admin()(this)
  
  class File(val owner: Principal, private val _contents: String = "")
  (implicit jlib: JeevesLib[Cred]) extends Atom {
    private val contents = ProtectedObjectRef[Cred, Cred](S(_contents)
      , ictxt => octxt => (ictxt.p === owner) || (ictxt.p === admin)
      , false)(jlib)
    def writeContents(c: ObjectExpr[Cred], body: String): UpdateResult = 
      contents.update(c, S(body))
    def getContents(): ObjectExpr[S] = contents.v.asInstanceOf[ObjectExpr[S]]
  }

  def login (p: Principal, pwd: String): ObjectExpr[Cred] = {
    p match {
      case u:User =>
      jif(u.password === S(pwd)
        , _ => Object(Cred(p)), _ => Object(Cred(NullUser)))
      case a:Admin =>
      jif(a.password === S(pwd)
        , _ => Object(Cred(p)), _ => Object(Cred(NullUser)))
      case _ => Object(Cred(NullUser))
    }
  }
}