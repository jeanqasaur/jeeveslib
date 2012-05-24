package test.cap.jeeves

import cap.scalasmt._
import cap.jeeves._
import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map

class JeevesTutorial extends FunSuite with JeevesLib {
  case class StringVal (v: String) extends JeevesRecord

  /**
   * Jeeves equivalent of "Hello world."
   */
  case class User (id: Int) extends JeevesRecord

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
    val aliceName = StringVal("Alice")
    val anonymousName = StringVal("Anonymous")

    val name: Symbolic = {
      val a = mkLevel ();
      policy(a, !(CONTEXT === alice))
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
  sealed trait UserRole extends JeevesRecord
  case object AuthorRole extends UserRole
  case object ReviewerRole extends UserRole
  case object PCRole extends UserRole
  case object PublicRole extends UserRole

  case class ConfUser (id: Int, role: UserRole) extends JeevesRecord
  val defaultUser = ConfUser(getNextUid (), PublicRole)
  val aliceUser = ConfUser(getNextUid (), AuthorRole)
  val bobUser = ConfUser(getNextUid (), ReviewerRole)
  val claireUser = ConfUser(getNextUid (), PCRole)

  private val _isInternalF: Formula = {
    ((CONTEXT.viewer.role === ReviewerRole)
    || (CONTEXT.viewer.role === PCRole))
  }

  case class Paper(
      private val title: String
    , private val author: ConfUser
    , private var reviews: List[Review]
    , private var isAccepted: Boolean ) extends JeevesRecord {
    // Level variables.
    private val _titleL = mkLevel()
    private val _acceptedL = mkLevel()

    policy(_titleL
      , !((CONTEXT.viewer === author) || _isInternalF
          || ((CONTEXT.stage === Public) && (getIsAccepted ()))) )
    def getTitle() = {
      mkSensitive(_titleL, StringVal(title), StringVal(""))
    }
    def showTitle(ctxt: ConfContext): String = {
      concretize(ctxt, getTitle()).asInstanceOf[StringVal].v
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
    , private var body: StringVal)
    extends JeevesRecord {
 
    private val _reviewerL = mkLevel()
    policy( _reviewerL
      , !((CONTEXT.viewer === reviewer)
        || (CONTEXT.viewer.role === PCRole)) )
    def getReviewer() = {
      mkSensitive(_reviewerL, reviewer, defaultUser)
    }

    private val _scoreL = mkLevel()
    policy ( _reviewerL, !_isInternalF )
    def getScore() = {
      mkSensitiveInt(_scoreL, score, -1)
    }
  }

  sealed trait ConfStage extends JeevesRecord
  case object Submission extends ConfStage
  case object Review extends ConfStage
  case object Decision extends ConfStage
  case object Public extends ConfStage

  case class ConfContext(viewer: ConfUser, stage: ConfStage)
    extends JeevesRecord

  // TODO: Make some papers and reviews.
  val paper0 = new Paper("Paper", aliceUser, Nil, false)

  test ("title policy") {
    expect("Paper") { paper0.showTitle(ConfContext(aliceUser, Submission)) }
    expect("") { paper0.showTitle(ConfContext(defaultUser, Submission)) }
    expect("Paper") { paper0.showTitle(ConfContext(bobUser, Submission)) }
    expect("Paper") { paper0.showTitle(ConfContext(claireUser, Submission)) }
  }
}
