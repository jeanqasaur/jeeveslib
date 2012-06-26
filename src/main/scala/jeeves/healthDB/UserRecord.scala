package cap.jeeves.healthDB

/*
 * Patient records.
 * @author jeanyang
 */

import cap.scalasmt._
import HealthDBBackend._

sealed trait UserStatus extends JeevesRecord
case object Admin extends UserStatus
case object Doctor extends UserStatus
case object Pharmacist extends UserStatus
case object Other extends UserStatus

case class UserRecord(id: Int, name: S, status: UserStatus) extends JeevesRecord
