package test.cap.jeeveslib.jeeves.healthDB

/*
 * Patient records.
 * @author jeanyang
 */

import cap.jeeveslib.ast._
import HealthDBBackend._

sealed trait UserStatus extends Atom
case object Admin extends UserStatus
case object Patient extends UserStatus
case object Doctor extends UserStatus
case object Pharmacist extends UserStatus
case object Other extends UserStatus

case class UserRecord(id: Int, name: S, status: UserStatus) extends Atom
