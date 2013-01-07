package test.cap.jeeveslib.jeeves.linksharing

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map

import test.cap.jeeveslib.jeeves.linksharing._
import cap.jeeveslib.ast._
import cap.jeeveslib.jeeves._
import LinkSharingBackend._

class ExampleLinkSharingBackend extends FunSuite {
  val MIT = Network("MIT");
  val jean = new UserRecord(
    Name("Jean Yang"), Self, 
    Email("jean@mit.edu"), Friends,
    MIT, Friends, 
    Friends)
  val kuat = new UserRecord(
    Name("Kuat Yessenov"), Friends, 
    Email("kuat@mit.edu"), Self,
    MIT, Self, 
    Friends)
  val joe = new UserRecord(
    Name("Joe Near"), Self, 
    Email("jnear@mit.edu"), Self,
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
    expect (Name("Kuat Yessenov")) { concretize(jean, kuat.getName()) }
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

  test ("getNetwork()s") {
    expect (MIT) {concretize(kuat, jean.getNetwork())}
    expect (null) {concretize(jean, kuat.getNetwork())}
    expect (jean :: Nil) {concretize(jean, getUsersByNetwork(MIT))}
  }

  test ("email") {
    expect (null) {concretize(kuat, joe.getEmail())}
    expect (null) {concretize(joe, jean.getEmail())}
    expect (Email("kuat@mit.edu")) {concretize(kuat, kuat.getEmail())}
  }
  
  test ("state change") {
    val eunsuk = new UserRecord(
      Name("Eunsuk Kang"), Anyone, 
      Email("eskang@mit.edu"), Anyone,
      MIT, Anyone, 
      Anyone);
    expect (null) { concretize(eunsuk, joe.getNetwork())}
    addFriend(joe, eunsuk)
    expect (MIT) { concretize(eunsuk, joe.getNetwork())}
    removeFriend(joe, eunsuk)
    expect (null) { concretize(eunsuk, joe.getNetwork())} 
  }

  test("geo location") {
    jean.setLocation(8, 8) // top
    kuat.setLocation(4, 4)
    joe.setLocation(0, 0) // bottom
    expect((1000, 1000)) {concretize(jean, joe.location)}
    expect((1000, 1000)) {concretize(joe, jean.location)}
    expect((0, 0)) {concretize(kuat, joe.location)}
    expect((8, 8)) {concretize(kuat, jean.location)}
    expect((4, 4)) {concretize(joe, kuat.location)}
    expect((4, 4)) {concretize(jean, kuat.location)}
    expect((4, 4)) {concretize(kuat, kuat.location)}
  }

  test("symbolic context") {
    expect(Set(Receipt(Email("kuat@mit.edu"), null))) {
      announceName(jean)
    }

    expect(Set(Receipt(null, null))) {
      announceName(joe)
    }
  }
}
