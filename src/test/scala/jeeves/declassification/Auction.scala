package test.cap.jeeveslib.jeeves

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.jeeves._

class Auction extends FunSuite with JeevesLib {
  case class User(id: BigInt) extends Atom
  val alice = User(0)
  val bob = User(1)
  val claire = User(2)

  case class Timestamp(time: BigInt) extends Atom

  case class Bid( private val _value: BigInt
                , private val _owner: User
                , private val _policy: ObjectExpr[AuctionContext] => Formula)
              extends Atom {
    private val l = mkLevel()
    restrict(l
      , (context: ObjectExpr[AuctionContext]) =>
          (context.user === _owner) || _policy(context))
    val value = mkSensitiveInt(l, _value, -1)
    val owner = _owner
  }
  case class AuctionContext(user: User, time: Timestamp, bids: List[Bid])
    extends Atom

  test ("owner can see") {
    val policy = (context: ObjectExpr[AuctionContext]) => BoolVal(false)
    val aliceBid = Bid(3, alice, policy)

    expect(3) {
      concretize(AuctionContext(alice, Timestamp(0), List()), aliceBid.value)
    }
    expect(-1) {
      concretize(AuctionContext(bob, Timestamp(0), List()), aliceBid.value)
    }
  }

  test ("time-sensitive release") {
    val auctionEndTime = Timestamp(10)
    val policy =
      (context: ObjectExpr[AuctionContext]) =>
        context.time.time > auctionEndTime.time
    val aliceBid = Bid(3, alice, policy)

    expect(3) {
      concretize(
        AuctionContext(bob, Timestamp(11), List()), aliceBid.value)
    };
    expect(-1) {
      concretize(
        AuctionContext(bob, Timestamp(10), List()), aliceBid.value)
    }
  }

  /*
  test("all users submit bids") {
    val allUsers: List[User] = List(alice, bob, claire)
    val policy =
      (context: Sensitive) =>
        allUsers.foldLeft(BoolVal(true))((acc, c) => BoolVal(true)) //(context.bids.contains(c) && acc))
    val aliceBid = Bid(3, alice, policy)
    expect(-1) {
      concretize(
        AuctionContext(bob, Timestamp(11), List(aliceBid)), aliceBid.value)
    }
  }
  */
}
