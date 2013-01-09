package test.cap.jeeveslib.jeeves.linksharing

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map

import test.cap.jeeveslib.jeeves.linksharing._
import cap.jeeveslib.ast._
import cap.jeeveslib.jeeves._
import LinkSharingBackend._

class ExampleLinkSharingBackend extends FunSuite {
  val MIT = S("MIT");
  val jean = new UserRecord(
    S("Jean Yang"), Self, 
    S("jean@mit.edu"), Friends,
    MIT, Friends, 
    Friends)
  val kuat = new UserRecord(
    S("Kuat Yessenov"), Friends, 
    S("kuat@mit.edu"), Self,
    MIT, Self, 
    Friends)
  val joe = new UserRecord(
    S("Joe Near"), Self, 
    S("jnear@mit.edu"), Self,
    MIT, Friends, 
    Self)

  addUser(jean);
  addUser(kuat);
  addUser(joe);
  addFriend(jean, kuat);
  addFriend(joe, kuat);

  test ("name") {
    expect (null) { concretize(kuat, joe.getName()) }
    expect (null) { concretize(joe, jean.getName()) }
    expect (S("Kuat Yessenov")) { concretize(jean, kuat.getName()) }
  }

  test ("getFriends") {
    expect (kuat :: Nil) {concretize(kuat, jean.getFriends())}
    expect (Nil) {concretize(joe, jean.getFriends())}
    expect (Nil) {concretize(kuat, joe.getFriends())}
  }

  test ("isFriends") {
    expect (true) { concretize(jean, jean.isFriends(kuat)) }
    expect (true) { concretize(kuat, jean.isFriends(kuat)) }
    expect (true) { concretize(joe, joe.isFriends(kuat)) }
    expect (true) { concretize(jean, kuat.isFriends(joe)) }
    expect (false) { concretize(jean, joe.isFriends(kuat)) }
  }

  test ("getNetwork") {
    expect (MIT) {concretize(kuat, jean.getNetwork())}
    expect (null) {concretize(jean, kuat.getNetwork())}
    expect (jean :: Nil) {concretize(jean, getUsersByNetwork(MIT))}
  }

  test ("email") {
    expect (null) {concretize(kuat, joe.getEmail())}
    expect (null) {concretize(joe, jean.getEmail())}
    expect (S("kuat@mit.edu")) {concretize(kuat, kuat.getEmail())}
  }
  
  test ("state change") {
    val eunsuk = new UserRecord(
      S("Eunsuk Kang"), Anyone, 
      S("eskang@mit.edu"), Anyone,
      MIT, Anyone, 
      Anyone);
    expect (null) { concretize(eunsuk, joe.getNetwork())}
    addFriend(joe, eunsuk)
    expect (MIT) { concretize(eunsuk, joe.getNetwork())}
    removeFriend(joe, eunsuk)
    expect (null) { concretize(eunsuk, joe.getNetwork())} 
  }

  test("symbolic context") {
    expect(Set(Receipt(S("kuat@mit.edu"), null))) {
      announceName(jean)
    }

    expect(Set(Receipt(null, null))) {
      announceName(joe)
    }
  }
}
