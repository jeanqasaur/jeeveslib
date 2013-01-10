package test.cap.jeeveslib.jeeves.socialnet

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expectResult}
import scala.collection.immutable.Map

import test.cap.jeeveslib.jeeves.socialnet._
import cap.jeeveslib.ast._
import cap.jeeveslib.jeeves._
import SocialNetBackend._

class ExampleSocialNetBackend extends FunSuite {
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
    expectResult (null) { concretize(kuat, joe.name) }
    expectResult (null) { concretize(joe, jean.name) }
    expectResult (Name("Kuat Yessenov")) { concretize(jean, kuat.name) }
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

  test ("networks") {
    expectResult (MIT) {concretize(kuat, jean.network)}
    expectResult (null) {concretize(jean, kuat.network)}
    expectResult (jean :: Nil) {concretize(jean, getUsersByNetwork(MIT))}
  }

  test ("email") {
    expectResult (null) {concretize(kuat, joe.email)}
    expectResult (null) {concretize(joe, jean.email)}
    expectResult (Email("kuat@mit.edu")) {concretize(kuat, kuat.email)}
  }
  
  test ("state change") {
    val eunsuk = new UserRecord(
      Name("Eunsuk Kang"), Anyone, 
      Email("eskang@mit.edu"), Anyone,
      MIT, Anyone, 
      Anyone);
    expectResult (null) { concretize(eunsuk, joe.network)}
    addFriend(joe, eunsuk)
    expectResult (MIT) { concretize(eunsuk, joe.network)}
    removeFriend(joe, eunsuk)
    expectResult (null) { concretize(eunsuk, joe.network)} 
  }

  test("geo location") {
    jean.setLocation(8, 8) // top
    kuat.setLocation(4, 4)
    joe.setLocation(0, 0) // bottom
    expectResult((1000, 1000)) {concretize(jean, joe.location)}
    expectResult((1000, 1000)) {concretize(joe, jean.location)}
    expectResult((0, 0)) {concretize(kuat, joe.location)}
    expectResult((8, 8)) {concretize(kuat, jean.location)}
    expectResult((4, 4)) {concretize(joe, kuat.location)}
    expectResult((4, 4)) {concretize(jean, kuat.location)}
    expectResult((4, 4)) {concretize(kuat, kuat.location)}
  }

  test("symbolic context") {
    expectResult(Set(Receipt(Email("kuat@mit.edu"), null))) {
      announceName(jean)
    }

    expectResult(Set(Receipt(null, null))) {
      announceName(joe)
    }
  }
}
