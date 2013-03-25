package test.cap.jeeveslib.jeeves.healthDB

/*
 * Patient records.
 * @author jeanyang
 */
import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import HealthDBBackend._

case class MedicationRecord(name: String) extends Atom

case class PatientRecord(
    private val _identity: UserRecord
  , private val _doctor: UserRecord
  , private val _meds: List[MedicationRecord]) extends Atom {
  private def isPatientOrDoctor (ctxt: ObjectExpr[HealthContext]): Formula = {
    (ctxt.user === _identity) || (ctxt.user === _doctor)
  }

  // Patient identity.
  private val np = mkLabel ()
  restrict (np, (ctxt: ObjectExpr[HealthContext]) => isPatientOrDoctor(ctxt))
  var identity = mkSensitive(np, _identity, defaultUser)
  def getIdentity = {
    mkSensitive(np, _identity, defaultUser)
  }
  def showIdentity (ctxt: HealthContext): UserRecord  = {
    concretize(ctxt, getIdentity()).asInstanceOf[UserRecord]
  }

  // Doctor identity.
  val doctorRef = ProtectedObjectRef[UserRecord, HealthContext](_doctor
    , (_, ictxt) => ictxt.status === Admin, None)(HealthDBBackend)
  def setDoctor (newDoctor: UserRecord) (implicit ctxt: HealthContext) = {
    doctorRef.update(ctxt.user, ctxt, newDoctor)
  }
  private val dp = mkLabel ()
  restrict (dp, (ctxt: ObjectExpr[HealthContext]) => isPatientOrDoctor(ctxt))
  def getDoctor() = { mkSensitive(dp, doctorRef.v, defaultUser) }

  // Medication list.
  private val mp = mkLabel ()
  restrict (dp, (ctxt: ObjectExpr[HealthContext]) => isPatientOrDoctor(ctxt))

  var _actualMeds = _meds // Keep this in order to remove.
  var meds = _meds.map(m => mkSensitive(mp, m, NULL))
  def addMed (newMed: MedicationRecord) (implicit ctxt: HealthContext): Unit = {
    val canSet = mkLabel ()
    restrict (canSet, (ctxt: ObjectExpr[HealthContext]) => ctxt.user.status === Admin)
   
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

