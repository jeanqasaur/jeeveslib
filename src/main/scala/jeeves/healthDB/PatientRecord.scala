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
  private val defaultUser = UserRecord(-1, S(""))

  // Patient identity.
  val np = mkLevel ()
  def identity = mkSensitive(np, _identity, defaultUser)
  def getIdentity = {
    mkSensitive(np, _identity, defaultUser)
  }
  def setIdentity (u: UserRecord) = {
    
  }

  def showIdentity (ctxt: HealthContext) = {

  }

  // Doctor identity.
  val dp = mkLevel ()
  def doctor = mkSensitive(dp, _doctor, defaultUser)
}

