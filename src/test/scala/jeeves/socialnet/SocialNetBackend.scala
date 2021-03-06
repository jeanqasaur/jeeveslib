package test.cap.jeeveslib.jeeves.socialnet

import scala.collection.mutable.HashMap;
import scala.collection.mutable.Set;

import cap.jeeveslib.ast._
import FExpr._
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.jeeves._

/**
 * External interface to social network.
 * @author kuat
 */
object SocialNetBackend extends JeevesLib[UserRecord] {
  private var users: List[UserRecord] = Nil;

  /* Database functions. */
  def addUser(u: UserRecord) {
    users = u :: users
  }
  
  def addFriend(record1: UserRecord, record2: UserRecord) {
    record1.add(record2);
    record2.add(record1);
  }

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

  def email(f: ObjectExpr[UserRecord], b: ObjectExpr[Name]) = 
    Receipt(concretize(f, f.email), concretize(f, b))

  case class Receipt(email: Atom, body: Atom)

  // send email to multiple people at the same time
} 
