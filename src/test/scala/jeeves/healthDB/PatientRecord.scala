package cap.jeeves.healthDB

/*
 * Patient records.
 * @author jeanyang
 */

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import HealthDBBackend._

case class MedicationRecord(name: String) extends Atom

class PatientRecord(
    private val _identity: UserRecord
  , private val _doctor: UserRecord
  , private val _meds: List[MedicationRecord]) extends Atom {
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
  def setDoctor (newDoctor: UserRecord) (implicit ctxt: HealthContext) = {
  /*
    doctor = writeAs (
        ctxt
        , (ictxt, octxt) => ictxt.user.status === Admin
        , doctor // old value
        , mkSensitive(dp, newDoctor, defaultUser) // new value
      )
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
    _actualMeds = writeAs (
      ctxt
      , (ictxt: Sensitive, octxt: Sensitive) => ictxt.user.status === Admin
      , _actualMeds.asInstanceOf[Atom]
      , (newMed :: _actualMeds).asInstanceOf[Atom] )
    meds = writeAs (
      ctxt
      , (ictxt: Sensitive, octxt: Sensitive) => ictxt.user.status === Admin
      , meds
      , (mkSensitive(mp, newMed, NULL)) :: meds )
    */
  }
  // TODO
  def removeMed () (implicit ctxt: HealthContext): Unit = {
    
  }
}

