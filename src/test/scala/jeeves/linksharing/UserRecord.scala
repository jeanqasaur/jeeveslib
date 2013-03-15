package test.cap.jeeveslib.jeeves.linksharing

/*
 * User records for jeeves social net case study.
 * @author jeanyang
 */

import collection.immutable.ListSet;

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import FExpr._
import LinkSharingBackend._

sealed trait UserLabel 
object Anyone extends UserLabel
object Self extends UserLabel
object Friends extends UserLabel

class UserRecord(
  private val _name: S, private val nameL: UserLabel,
  private val _email: S, private val emailL: UserLabel, 
  private val _network: S, private val networkL: UserLabel, 
  private val friendsL: UserLabel) extends Atom {
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
  private def level (ul: UserLabel): LabelVar = {
    val l = mkLabel();
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
