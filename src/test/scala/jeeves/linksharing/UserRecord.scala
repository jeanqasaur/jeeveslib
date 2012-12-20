package test.cap.jeeveslib.jeeves.linksharing

/*
 * User records for jeeves social net case study.
 * @author jeanyang
 */

import collection.immutable.ListSet;

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import Expr._
import LinkSharingBackend._

case class Name(s: String) extends Atom
case class Email(s: String) extends Atom
case class Network(s: String) extends Atom
// TODO: Links

sealed trait UserLevel 
object Anyone extends UserLevel
object Self extends UserLevel
object Friends extends UserLevel

class UserRecord(
  private val nameV: Name, 
  private val nameL: UserLevel,
  private val emailV: Email,
  private val emailL: UserLevel, 
  private val networkV: Network, 
  private val networkL: UserLevel, 
  private val friendsL: UserLevel) extends Atom {
  private var friends: Set[UserRecord] = Set()
  var X: IntExpr = 1000
  var Y: IntExpr = 1000

  /** Mutators */
  def add(u: UserRecord) {friends = friends + u}
  def remove(u: UserRecord) {friends = friends - u}
  def setLocation(x: BigInt, y: BigInt) {
    val l = mkLevel();
    restrict(l, (ctxt: ObjectExpr[UserRecord]) => DISTANCE(ctxt, this) < 10);
    this.X = mkSensitiveInt(l, x, 1000);
    this.Y = mkSensitiveInt(l, y, 1000);
  }

  /** Observers */
  // Change to have getters and setters and all that...
  def getName(): ObjectExpr[Name] = mkSensitive(level (nameL), nameV)
  def getEmail(): ObjectExpr[Email] = mkSensitive(level (emailL), emailV)
  def getNetwork(): ObjectExpr[Network] =
    mkSensitive(level (networkL), networkV);
  def getFriends () = {
    val l = level(friendsL);
    friends.map(mkSensitive(l, _))
  }
  def isFriends(u: UserRecord) = getFriends.has(u)
  def location() = (X, Y);

  /** Helpers */
  private def level (ul: UserLevel): LevelVar = {
    val l = mkLevel();
    def me (implicit ctxt: ObjectExpr[UserRecord]) = ctxt === this;
    ul match {
      case Anyone => 
      case Self => restrict(l, (ctxt: ObjectExpr[UserRecord]) => me (ctxt))
      case Friends => restrict(l,
        (ctxt: ObjectExpr[UserRecord]) => (me (ctxt) || friends.has(ctxt)));
    }
    l
  }

  private def ABS(x: IntExpr): IntExpr = {
    jif (x >= 0, _ => x, _ => -x)
  }
  private def DISTANCE(a: ObjectExpr[Atom], b: ObjectExpr[Atom]) =
    ABS(a.X - b.X) + ABS(a.Y - b.Y) 
}
