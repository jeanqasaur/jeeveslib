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
  private def isPatientOrDoctor (ctxt: Sensitive): Formula = {
    (ctxt.identity === this) || (ctxt.identity === doctor)
  }

  // Patient identity.
  private val np = mkLevel ()
  restrict (np, (ctxt: Sensitive) => isPatientOrDoctor(ctxt))
  var identity = mkSensitive(np, _identity, defaultUser)
  def getIdentity = {
    mkSensitive(np, _identity, defaultUser)
  }
  def showIdentity (ctxt: HealthContext): UserRecord  = {
    concretize(ctxt, identity).asInstanceOf[UserRecord]
  }

  // Doctor identity.
  private val dp = mkLevel ()
  restrict (dp, (ctxt: Sensitive) => isPatientOrDoctor(ctxt))
  var doctor: Sensitive = mkSensitive(dp, _doctor, defaultUser)
  // TODO: Figure out why we need these explicit conversions...
  def setDoctor (newDoctor: UserRecord) (implicit ctxt: HealthContext) = {
    val canSet = mkLevel ()
    restrict (canSet, (ctxt: Sensitive) => ctxt.user.status === Admin)
/*    doctor = guardedAssign (
      ctxt.asInstanceOf[Sensitive], canSet
      , mkSensitive(dp, newDoctor, defaultUser), doctor).asInstanceOf[Sensitive]
    */
  }

  // Medication list.
  private val mp = mkLevel ()
  restrict (dp, (ctxt: Sensitive) => isPatientOrDoctor(ctxt))

  var _actualMeds = _meds // Keep this in order to remove.
  var meds = _meds.map(m => mkSensitive(mp, m, NULL))
  def addMed (newMed: MedicationRecord) (implicit ctxt: HealthContext): Unit = {
    val canSet = mkLevel ()
    restrict (canSet, (ctxt: Sensitive) => ctxt.user.status === Admin)
  /*
    _actualMeds = guardedAssign (
      ctxt.asInstanceOf[Sensitive], canSet
      , newMed :: _actualMeds, _actualMeds ) 
    meds = guardedAssign (
      ctxt.asInstanceOf[Sensitive], canSet
      , (mkSensitive(mp, newMed, NULL)) :: meds, meds )
    */
  }
  // TODO
  def removeMed () (implicit ctxt: HealthContext): Unit = {
    
  }
}

