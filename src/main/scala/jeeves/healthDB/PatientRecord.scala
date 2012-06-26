package cap.jeeves.healthDB

/*
 * Patient records.
 * @author jeanyang
 */

import cap.scalasmt._
import HealthDBBackend._

case class MedicationRecord(name: String) extends JeevesRecord

class PatientRecord(
    private val _identity: UserRecord
  , private val _doctor: UserRecord
  , private val _meds: List[MedicationRecord]) extends JeevesRecord {
  private val defaultUser = UserRecord(-1, S(""), Other)

  // Patient identity.
  val np = mkLevel ()
  var identity = mkSensitive(np, _identity, defaultUser)
  def getIdentity = {
    mkSensitive(np, _identity, defaultUser)
  }
  def showIdentity (ctxt: HealthContext): UserRecord  = {
    concretize(ctxt, identity).asInstanceOf[UserRecord]
  }

  // Doctor identity.
  val dp = mkLevel ()
  var doctor: Symbolic = mkSensitive(dp, _doctor, defaultUser)
  // TODO: Figure out why we need these explicit conversions...
  def setDoctor (newDoctor: UserRecord) (implicit ctxt: HealthContext) = {
    val canSet = mkLevel ()
    restrict (canSet, (ctxt: Symbolic) => ctxt.user.status === Admin)
    doctor = guardedAssign (
      ctxt.asInstanceOf[Symbolic], canSet
      , mkSensitive(dp, newDoctor, defaultUser), doctor).asInstanceOf[Symbolic]
  }
}

