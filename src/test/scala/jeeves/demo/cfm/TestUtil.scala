package test.cap.jeeveslib.jeeves.demo.cfm

import cap.jeeveslib.demo.cfm._
import JConfBackend._

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expectResult}

import scala.collection.immutable.Map
import scala.collection.mutable.Set
import scala.util.Random

object TestUtil {
  def mkUser( userName : String, name: String, affiliation: String
            , pwd: String, isGrad: Boolean = false, acmNum: String = "-1"
            , userStatus : UserStatus): ConfUser = {
    addUser(userName, name, affiliation, pwd, isGrad, acmNum, userStatus)
  }

  def getContext(user: ConfUser, stage: PaperStage = Submission): ConfContext =
    ConfContext(user, stage)

  // jconf users.
  val author0 = mkUser("author0", "Author0", "", "a0p", userStatus=AuthorStatus)
  def getAuthorCtxt0 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(author0, stage);
  val author1 = mkUser("author1", "Author1", "", "", userStatus=AuthorStatus);
  def getAuthorCtxt1 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(author1, stage);
  val author2 = mkUser("author2", "Author2", "", "", userStatus=AuthorStatus)
  def getAuthorCtxt2 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(author2, stage);

  val reviewer0 =
    mkUser("reviewer0", "Reviewer0", "", "", userStatus=ReviewerStatus);
  def getReviewerCtxt0 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(reviewer0, stage);
  val reviewer1 =
    mkUser("reviewer1", "Reviewer1", "", "", userStatus=ReviewerStatus);
  def getReviewerCtxt1 (stage : PaperStage = Submission)
    : ConfContext = new ConfContext(reviewer1, stage);

  val pc0 = mkUser("pc0", "PC0", "", "", userStatus=PCStatus);
  def getPcCtxt0 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(pc0, stage);

  val public0 = mkUser("public0", "Public0", "", "", userStatus=PublicStatus);
  def getPublicCtxt0 (stage: PaperStage = Submission)
    : ConfContext = new ConfContext(public0, stage);

  // papers.
  val emptyName = ""

  val paper0Name = "my paper"
  val paper0 = addPaper(paper0Name, List(author0, author1));
  assignReview(paper0, reviewer1);
  assignReview(paper0, reviewer0);
  val paper0Review = paper0.addReview(reviewer0, "very nice", 3);

  val paper1Name = "hello world"
  val paper1 = addPaper(paper1Name, List(author2), tags=List(Accepted));
  val paper1Review = paper1.addReview(reviewer1, "eh", 2);
}
