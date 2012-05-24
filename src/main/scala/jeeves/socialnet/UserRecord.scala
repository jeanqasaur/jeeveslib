package cap.jeeves.socialnet

/*
 * User records for jeeves social net case study.
 * @author jeanyang, kuat
 */

import cap.scalasmt._
import collection.immutable.ListSet;
import SocialNetBackend._

import Expr._

case class Name(s: String) extends JeevesRecord
case class Email(s: String) extends JeevesRecord
case class Network(s: String) extends JeevesRecord

sealed trait UserLevel 
object Anyone extends UserLevel
object Self extends UserLevel
object Friends extends UserLevel

class UserRecord(
  nameV: Name, 
  nameL: UserLevel,
  emailV: Email,
  emailL: UserLevel, 
  networkV: Network, 
  networkL: UserLevel, 
  friendsL: UserLevel) 
extends JeevesRecord {
  private var friends: Set[UserRecord] = Set()
  var X: IntExpr = 1000
  var Y: IntExpr = 1000

  /** Mutators */
  def add(u: UserRecord) {friends = friends + u}
  def remove(u: UserRecord) {friends = friends - u}
  def setLocation(x: BigInt, y: BigInt) {
    val l = mkLevel();
    policy(l, DISTANCE(CONTEXT, this) >= 10);
    this.X = mkSensitiveInt(l, x, 1000);
    this.Y = mkSensitiveInt(l, y, 1000);
  }

  /** Observers */
  val name = mkSensitive(level(nameL), nameV)
  val email = mkSensitive(level(emailL), emailV)
  val network = mkSensitive(level(networkL), networkV);
  def getFriends() = {
    val l = level(friendsL);
    friends.map(mkSensitive(l, _))
  }
  def isFriends(u: UserRecord) = getFriends.has(u)
  def location() = (X, Y);

  /** Helpers */
  private def level(ul: UserLevel) = {
    val l = mkLevel();
    val me = CONTEXT === this;
    ul match {
      case Anyone => 
      case Self => policy(l, ! me)
      case Friends => policy(l, ! (me || friends.has(CONTEXT)));
    }
    l
  }

  private def DISTANCE(a: Symbolic, b: Symbolic) = 
    ABS(a.X - b.X) + ABS(a.Y - b.Y) 
}
