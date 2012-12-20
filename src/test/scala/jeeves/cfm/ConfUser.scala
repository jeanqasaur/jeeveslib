package test.cap.jeeveslib.jeeves.cfm

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._

import CfmBackend._

sealed trait UserStatus extends Atom
object PublicStatus extends UserStatus
object AuthorStatus extends UserStatus
object ReviewerStatus extends UserStatus
object PCStatus extends UserStatus

/* Conference User */
case class Name (name: String) extends Atom
case class Password (pwd: String) extends Atom
case class ConfUser( val name: Name, _password: String, val role: UserStatus )
  extends Atom {
    val password = {
      val level = mkLevel ();
      restrict (level
        , (ctxt: ObjectExpr[ConfContext]) => (ctxt.viewer === this));
      mkSensitive(level, Password(_password), Password(""))
    }
  }
