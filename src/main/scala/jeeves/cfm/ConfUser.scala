package cap.jeeves.cfm

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._

import CfmBackend._

sealed trait UserStatus extends JeevesRecord
object PublicStatus extends UserStatus
object AuthorStatus extends UserStatus
object ReviewerStatus extends UserStatus
object PCStatus extends UserStatus

/* Conference User */
case class Name (name: String) extends JeevesRecord
case class Password (pwd: String) extends JeevesRecord
case class ConfUser( val name: Name, _password: String, val role: UserStatus )
  extends JeevesRecord {
    val password = {
      val level = mkLevel ();
      policy (level, (CONTEXT: Symbolic) => !(CONTEXT.viewer === this));
      mkSensitive(level, Password(_password), Password(""))
    }
  }
