package cap.jeeves.healthDB

import scala.collection.mutable.HashMap;
import scala.collection.mutable.Set;

import cap.jeeveslib.ast._
import cap.jeeveslib.jeeves._

import Expr._

/**
 * External interface to healthDB.
 * @author jean
 */
object HealthDBBackend extends JeevesLib {
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
      jif ((p.doctor === doctor)
        , ((_: Unit) => count = count + 1)
        , ((_: Unit) => ()) )
    }
    count
  }
  /*
  def removeFriend(record1: UserRecord, record2: UserRecord) {
    record1.remove(record2);
    record2.remove(record1);
  }

  def getFriendNetworks(user: UserRecord) =
    user.getFriends().map(_.network)

  def getUsersByNetwork(network : Network) = 
    users.filter(_.network === network)

  def announceName(u: UserRecord) = 
    for (f <- u.getFriends())
      yield email(f, u.name)

  def email(f: Sensitive, b: Sensitive) = 
    Receipt(concretize(f, f.email), concretize(f, b))

  case class Receipt(email: Atom, body: Atom)

  // send email to multiple people at the same time
  */
} 
