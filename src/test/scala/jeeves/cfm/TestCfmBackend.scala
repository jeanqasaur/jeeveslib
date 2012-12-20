package test.cap.jeeveslib.jeeves.cfm

import cap.jeeveslib.jeeves._
import test.cap.jeeveslib.jeeves.cfm._
import CfmBackend._

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expect}
import scala.collection.immutable.Map
import scala.collection.mutable.Set
import scala.util.Random

class TestCfmBackend extends FunSuite {
  def mkUser(userName : String, pwd: String, userStatus : UserStatus)
    : ConfUser =
    new ConfUser(Name (userName), pwd, userStatus);

  // jconf users.
  val author0 = mkUser("author0", "", AuthorStatus)
  private def getAuthorCtxt0 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(author0, stage);
  val author1 = mkUser("author1", "", AuthorStatus);
  private def getAuthorCtxt1 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(author1, stage);
  val author2 = mkUser("author2", "", AuthorStatus)
  private def getAuthorCtxt2 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(author2, stage);

  val reviewer0 = mkUser("reviewer0", "", ReviewerStatus);
  private def getReviewerCtxt0 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(reviewer0, stage);
  val reviewer1 = mkUser("reviewer1", "", ReviewerStatus);

  val pc0 = mkUser("pc0", "", PCStatus);
  private def getPcCtxt0 (stage : PaperStage = Submission)
  : ConfContext = new ConfContext(pc0, stage);

  val public0 = mkUser("public0", "", PublicStatus);
  private def getPublicCtxt0 (stage: PaperStage = Submission)
  : ConfContext = new ConfContext(public0, stage);

  // papers.
  val emptyName = Title("")

  val paper0Name = Title("my paper")
  val paper0 = addPaper(paper0Name, List(author0, author1), Nil);
  assignReview(paper0, reviewer1);
  assignReview(paper0, reviewer0);
  val paper0Review = paper0.addReview(reviewer0, "very nice", 3);

  val paper1Name = Title("hello world")
  val paper1 = addPaper(paper1Name, List(author2), List(Accepted));

  // Name visibility
  test ("name visibility") {
    expect(paper0Name) { concretize(getAuthorCtxt0(), paper0.name); }
    expect(emptyName) { concretize(getAuthorCtxt2(), paper0.name); }

    val viewMap =
      Map((Submission, paper0Name), (Review, paper0Name), (Decision, paper0Name));
    viewMap.foreach {
      case (stage, r) =>
        expect (r) {
          concretize(getReviewerCtxt0(stage), paper0.name)
        };
        expect (r) {
          concretize(getPcCtxt0(stage), paper0.name);
        }
    }

    expect(emptyName) { concretize(getPublicCtxt0(Submission), paper1.name); }
    expect(paper1Name) { concretize(getPublicCtxt0(Public), paper1.name); }
  }

  // Author list visibility
  test ("author list") {
    expect (true) {
      concretize(getAuthorCtxt0(), paper0.authors.has(author0))
    };
    expect (true) {
      concretize(getAuthorCtxt1(), paper0.authors.has(author0))
    }
    expect (false) {
      concretize( getReviewerCtxt0(Submission)
                , paper0.authors.has(author0));
    }
    expect (true) {
      concretize(getReviewerCtxt0(Decision), paper0.authors.has(author0));
    }
    expect (true) {
      concretize(getPcCtxt0(Decision), paper0.authors.has(author0));
    }
  }

  test ("tag visibility") {
    expect (false) {
      concretize(getAuthorCtxt0(Decision), paper1.hasTag(Accepted)) }
    expect (true) {
      concretize(getAuthorCtxt0(Public), paper1.hasTag(Accepted));
    }
  }

  test ("tag state change") {
    expect (false) {
      concretize(getAuthorCtxt0(Public), paper0.hasTag(Accepted));
    }
    paper0.addTag(Accepted);
    expect (true) {
      concretize(getAuthorCtxt0(Public), paper0.hasTag(Accepted));
    }
    paper0.removeTag(Accepted);
    expect (false) {
      concretize(getAuthorCtxt0(Public), paper0.hasTag(Accepted));
    }

  }

  test ("review assignment") {
    expect (true) { isAssigned(paper0, reviewer0); }
    expect (true) { isAssigned(paper0, reviewer1); }
    expect (false) { isAssigned(paper1, reviewer0); }
    expect (false) { isAssigned(paper1, reviewer1); }

    assignReview(paper1, author0);
    expect (false) { isAssigned(paper1, author0); }
  }

  test ("review tag visibility") {
    expect (false) {
      concretize(getAuthorCtxt0(Review), paper0.hasTag(ReviewedBy(reviewer0)));
    }
    expect (true) {
      concretize(getReviewerCtxt0(Review), paper0.hasTag(ReviewedBy(reviewer0)));
    }
    expect (true) {
      concretize(getPcCtxt0(Review), paper0.hasTag(ReviewedBy(reviewer0)));
    }
  }

  /*
  test ("review visibility") {
    expect(null) {
      concretize(getAuthorCtxt1(Review), paper0Review);
    }
    expect(null) {
      concretize(getAuthorCtxt0(Review), paper0Review.reviewer);
    }
    expect(reviewer0) {
      concretize(getReviewerCtxt0(Review), paper0Review.reviewer);
    }
  }

  test ("back end functionality") {
    expect(Some(paper0)) { getById(paper0.id); }
    expect(false) {
      concretize( getPublicCtxt0(Public)
                , searchByName("my paper").has(paper0) );
    }
    expect(true) {
      concretize( getPublicCtxt0(Public)
                , searchByName("hello world").has(paper1) );
    }
    expect(true) {
      concretize( getPublicCtxt0(Public)
                , searchByAuthor(author2).has(paper1) );
    }

  }
  */
}
