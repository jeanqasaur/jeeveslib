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

sealed trait UserLevel 
object Anyone extends UserLevel
object Self extends UserLevel
object Friends extends UserLevel

class UserRecord(
  private val _name: S, private val nameL: UserLevel,
  private val _email: S, private val emailL: UserLevel, 
  private val _network: S, private val networkL: UserLevel, 
  private val friendsL: UserLevel) extends Atom {
  private var friends: Set[UserRecord] = Set()
  private var links: List[ObjectExpr[S]] = List()

  /** Mutators */
  def add(u: UserRecord) {friends = friends + u}
  def remove(u: UserRecord) {friends = friends - u}

  /** Observers */
  // Change to have getters and setters and all that...
  def getName(): ObjectExpr[S] = mkSensitive(level (nameL), _name)
  def getEmail(): ObjectExpr[S] = mkSensitive(level (emailL), _email)
  def getNetwork(): ObjectExpr[S] =
    mkSensitive(level (networkL), _network);
  def getFriends () = {
    val l = level(friendsL);
    friends.map(mkSensitive(l, _))
  }
  def isFriends(u: UserRecord) = getFriends.has(u)

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
}
