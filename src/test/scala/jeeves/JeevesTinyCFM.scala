package test.cap.jeeves

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import cap.jeeveslib.jeeves._

class JeevesTinyCFM extends FunSuite with JeevesLib {
  /**
   * Jeeves equivalent of "Hello world."
   */
  case class User (id: Int) extends Atom

  private var _id = 0
  private def getNextUid (): Int = {
    val id = _id
    _id = _id + 1
    id
  }

  // Some users.
  val alice = User (getNextUid ())
  val bob = User (getNextUid ())
  val claire = User (getNextUid ())

  test ("Jeeves hello world") {
    val aliceName = S("Alice")
    val anonymousName = S("Anonymous")

    val name: ObjectExpr[S] = {
      val a = mkLevel ();
      restrict(a, (ctxt: ObjectExpr[User]) => ctxt === alice)
      mkSensitive(a // Level variable
        , aliceName     // High-confidentiality value
        , anonymousName // Low-confidentiality value
      )
    }

    expect(aliceName) { concretize(alice, name) }
    expect(anonymousName) { concretize(bob, name) }
    expect(anonymousName) { concretize(claire, name) }
  }
  

  /**
   * Simple conference management example.
   */
  sealed trait UserRole extends Atom
  case object AuthorRole extends UserRole
  case object ReviewerRole extends UserRole
  case object PCRole extends UserRole
  case object PublicRole extends UserRole

  case class ConfUser (id: Int, role: UserRole) extends Atom
  val defaultUser = ConfUser(getNextUid (), PublicRole)
  val aliceUser = ConfUser(getNextUid (), AuthorRole)
  val bobUser = ConfUser(getNextUid (), ReviewerRole)
  val claireUser = ConfUser(getNextUid (), PCRole)

  private def _isInternalF (implicit ctxt: ObjectExpr[User]): Formula = {
    ((ctxt.viewer.role === ReviewerRole)
    || (ctxt.viewer.role === PCRole))
  }

  case class Paper(
      private val title: String
    , private val author: ConfUser
    , private var reviews: List[Review]
    , private var isAccepted: Boolean ) extends Atom {
    // Level variables.
    private val _titleL = mkLevel()
    private val _acceptedL = mkLevel()

    restrict(_titleL
      , (ctxt: ObjectExpr[User]) =>
        ((ctxt.viewer === author) || (_isInternalF (ctxt))
            || ((ctxt.stage === Public) && (getIsAccepted ()))) )
    def getTitle() = {
      mkSensitive(_titleL, S(title), S(""))
    }
    def showTitle(ctxt: ConfContext): String = {
      concretize(ctxt, getTitle()).asInstanceOf[S].s
    }

    // TODO: Make this sensitive.
    def setIsAccepted(accepted: Boolean): Unit = {
      isAccepted = accepted
    }
    def getIsAccepted(): Formula = {
      isAccepted
    }
    def showIsAccepted(ctxt: ConfContext): Boolean = {
      concretize(ctxt, getIsAccepted()).asInstanceOf[Boolean]
    }
  }

  case class Review(
      private val reviewer: User
    , private var score: BigInt
    , private var body: S)
    extends Atom {
 
    private val _reviewerL = mkLevel()
    restrict( _reviewerL
      , (ctxt: ObjectExpr[User]) =>
        ((ctxt.viewer === reviewer)
          || (ctxt.viewer.role === PCRole)) )
    def getReviewer() = {
      mkSensitive(_reviewerL, reviewer, defaultUser)
    }

    private val _scoreL = mkLevel()
    restrict ( _reviewerL, (ctxt: ObjectExpr[User]) => _isInternalF (ctxt))
    def getScore() = {
      mkSensitiveInt(_scoreL, score, -1)
    }
  }

  sealed trait ConfStage extends Atom
  case object Submission extends ConfStage
  case object Review extends ConfStage
  case object Decision extends ConfStage
  case object Public extends ConfStage

  case class ConfContext(viewer: ConfUser, stage: ConfStage)
    extends Atom

  // TODO: Make some papers and reviews.
  val paper0 = new Paper("Paper", aliceUser, Nil, false)

  test ("title restrict") {
    expect("Paper") { paper0.showTitle(ConfContext(aliceUser, Submission)) }
    expect("") { paper0.showTitle(ConfContext(defaultUser, Submission)) }
    expect("Paper") { paper0.showTitle(ConfContext(bobUser, Submission)) }
    expect("Paper") { paper0.showTitle(ConfContext(claireUser, Submission)) }
  }

  // TODO: Demonstrate circular dependencies.
}
