package test.cap.jeeveslib.jeeves

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expectResult}

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
    // The policy allows the owner to always be able to see their own bid.
    // Otherwise it defers to the policy passed in.
    restrict(l
      , (context: ObjectExpr[AuctionContext]) =>
          (context.user === _owner) || _policy(context))
    val value = mkSensitiveInt(l, _value, -1)
    val owner = _owner
  }
  case class AuctionContext(user: User, time: Timestamp, bids: List[Bid])
    extends Atom

  /* This test makes sure the owner can always see their own bid. */
  test ("owner can see") {
    val policy = (context: ObjectExpr[AuctionContext]) => BoolVal(false)
    val aliceBid = Bid(3, alice, policy)

    expectResult(3) {
      concretize(AuctionContext(alice, Timestamp(0), List()), aliceBid.value)
    }
    expectResult(-1) {
      concretize(AuctionContext(bob, Timestamp(0), List()), aliceBid.value)
    }
  }

  /* This test shows how we can encode time-sensitive release. */
  test ("time-sensitive release") {
    val auctionEndTime = Timestamp(10)
    val policy =
      (context: ObjectExpr[AuctionContext]) =>
        context.time.time > auctionEndTime.time
    val aliceBid = Bid(3, alice, policy)

    expectResult(3) {
      concretize(
        AuctionContext(bob, Timestamp(11), List()), aliceBid.value)
    };
    expectResult(-1) {
      concretize(
        AuctionContext(bob, Timestamp(10), List()), aliceBid.value)
    }
  }

  /* This test shows how we can encode "sealed auction" policies: bids are
     only made public once everyone has submitted their bids. */
  /* NOTE(JY): This function might be a bit more complex than it needs to be
     right now.  I wrestled with the Scala typechecker for a bit and then
     took a break.  Might clean this up later.
     
     The issue is with getting fields of sensitive objects.  I might do a
     cleanup of that whole thing at some point.  */
  test("all users submit bids") {
    def hasBidFromUser(ctxt: ObjectExpr[AuctionContext], u: User): Formula = {
      val bids: ObjectExpr[Atoms[Bid]] =
        ctxt.applyFunction[Atoms[Bid]](ac => Atoms(ac.bids))
        bids.applyFunction((bidList: Atoms[Bid]) =>
          bidList.hasFormula(b => b.owner === u))
    }

    val allUsers: List[User] = List(alice, bob, claire)
    val policy =
      (ctxt: ObjectExpr[AuctionContext]) =>
      allUsers.foldLeft[Formula](BoolVal(true))(
        (acc: Formula, c) => hasBidFromUser(ctxt, c) && acc)
    
    val aliceBid = Bid(3, alice, policy)
    val bobBid = Bid(4, bob, policy)
    val claireBid = Bid(5, claire, policy)

    expectResult(-1) {
      concretize(
        AuctionContext(bob, Timestamp(11), List(aliceBid)), aliceBid.value)
    }
    expectResult(-1) {
      concretize(
        AuctionContext(bob, Timestamp(11), List(aliceBid, bobBid))
        , aliceBid.value)
    }
    expectResult(3) {
      concretize(
        AuctionContext(bob, Timestamp(11), List(aliceBid, bobBid, claireBid))
        , aliceBid.value)
    }
  }
}
