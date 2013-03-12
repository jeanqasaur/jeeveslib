package test.cap.jeeveslib.jeeves.healthDB

import scala.collection.mutable.HashMap;
import scala.collection.mutable.Set;

import cap.jeeveslib.ast._
import cap.jeeveslib.jeeves._

import FExpr._

/**
 * External interface to healthDB.
 * @author jean
 */
object HealthDBBackend extends JeevesLib[HealthContext] {
  val defaultUser = UserRecord(-1, S(""), Other)

//  private var doctors: List[UserRecord] = Nil;
  private var patients: List[PatientRecord] = Nil;

  private var _userCount = 0
  private def getNextUserId(): Int = {
    val id = _userCount;
    _userCount = _userCount + 1;
    id
  }
  def mkUser(name: String, status: UserStatus): UserRecord = {
    new UserRecord(getNextUserId(), S(name), status)
  }

  def sendMsg (p: PatientRecord) (msg: String): Unit = {
    // TODO: Send message to user
  }

  def countPatients (doctor: UserRecord): IntExpr = {
    var count: IntExpr = 0
    patients.foreach { p =>
      jif ((p.getDoctor() === doctor)
        , ((_: Unit) => count = count + 1)
        , ((_: Unit) => ()) )
    }
    count
  }
} 
