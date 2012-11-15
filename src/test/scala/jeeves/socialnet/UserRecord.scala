package cap.jeeves.socialnet

/*
 * User records for jeeves social net case study.
 * @author jeanyang, kuat
 */

import collection.immutable.ListSet;

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import Expr._
import SocialNetBackend._

case class Name(s: String) extends Atom
case class Email(s: String) extends Atom
case class Network(s: String) extends Atom

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
extends Atom {
  private var friends: Set[UserRecord] = Set()
  var X: IntExpr = 1000
  var Y: IntExpr = 1000

  /** Mutators */
  def add(u: UserRecord) {friends = friends + u}
  def remove(u: UserRecord) {friends = friends - u}
  def setLocation(x: BigInt, y: BigInt) {
    val l = mkLevel();
    restrict(l, (CONTEXT: Sensitive) => DISTANCE(CONTEXT, this) < 10);
    this.X = mkSensitiveInt(l, x, 1000);
    this.Y = mkSensitiveInt(l, y, 1000);
  }

  /** Observers */
  val name = mkSensitive(level (nameL), nameV)
  val email = mkSensitive(level (emailL), emailV)
  val network = mkSensitive(level (networkL), networkV);
  def getFriends () = {
    val l = level(friendsL);
    friends.map(mkSensitive(l, _))
  }
  def isFriends(u: UserRecord) = getFriends.has(u)
  def location() = (X, Y);

  /** Helpers */
  private def level (ul: UserLevel): LevelVar = {
    val l = mkLevel();
    def me (implicit CONTEXT: Sensitive) = CONTEXT === this;
    ul match {
      case Anyone => 
      case Self => restrict(l, (CONTEXT: Sensitive) => me (CONTEXT))
      case Friends => restrict(l,
        (CONTEXT: Sensitive) => (me (CONTEXT) || friends.has(CONTEXT)));
    }
    l
  }

  private def ABS(x: IntExpr): IntExpr = {
    jif (x >= 0, _ => x, _ => -x)
  }
  private def DISTANCE(a: Sensitive, b: Sensitive) = 
    ABS(a.X - b.X) + ABS(a.Y - b.Y) 
}
