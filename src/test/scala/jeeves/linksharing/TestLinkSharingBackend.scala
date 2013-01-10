package test.cap.jeeveslib.jeeves.linksharing

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expectResult}
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
    expectResult (null) { concretize(kuat, joe.getName()) }
    expectResult (null) { concretize(joe, jean.getName()) }
    expectResult (S("Kuat Yessenov")) { concretize(jean, kuat.getName()) }
  }

  test ("getFriends") {
    expectResult (kuat :: Nil) {concretize(kuat, jean.getFriends())}
    expectResult (Nil) {concretize(joe, jean.getFriends())}
    expectResult (Nil) {concretize(kuat, joe.getFriends())}
  }

  test ("isFriends") {
    expectResult (true) { concretize(jean, jean.isFriends(kuat)) }
    expectResult (true) { concretize(kuat, jean.isFriends(kuat)) }
    expectResult (true) { concretize(joe, joe.isFriends(kuat)) }
    expectResult (true) { concretize(jean, kuat.isFriends(joe)) }
    expectResult (false) { concretize(jean, joe.isFriends(kuat)) }
  }

  test ("getNetwork") {
    expectResult (MIT) {concretize(kuat, jean.getNetwork())}
    expectResult (null) {concretize(jean, kuat.getNetwork())}
    expectResult (jean :: Nil) {concretize(jean, getUsersByNetwork(MIT))}
  }

  test ("email") {
    expectResult (null) {concretize(kuat, joe.getEmail())}
    expectResult (null) {concretize(joe, jean.getEmail())}
    expectResult (S("kuat@mit.edu")) {concretize(kuat, kuat.getEmail())}
  }
  
  test ("state change") {
    val eunsuk = new UserRecord(
      S("Eunsuk Kang"), Anyone, 
      S("eskang@mit.edu"), Anyone,
      MIT, Anyone, 
      Anyone);
    expectResult (null) { concretize(eunsuk, joe.getNetwork())}
    addFriend(joe, eunsuk)
    expectResult (MIT) { concretize(eunsuk, joe.getNetwork())}
    removeFriend(joe, eunsuk)
    expectResult (null) { concretize(eunsuk, joe.getNetwork())} 
  }

  test("symbolic context") {
    expectResult(Set(Receipt(S("kuat@mit.edu"), null))) {
      announceName(jean)
    }

    expectResult(Set(Receipt(null, null))) {
      announceName(joe)
    }
  }
}
