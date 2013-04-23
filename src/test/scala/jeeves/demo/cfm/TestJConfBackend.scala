package test.cap.jeeveslib.jeeves.demo.cfm

import cap.jeeveslib.ast.{S}
import cap.jeeveslib.jeeves._
import cap.jeeveslib.debug.DebugPrint._
import cap.jeeveslib.demo.cfm._
import JConfBackend._

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expectResult}

import TestUtil._

class TestJConfBackend extends FunSuite {
  // Name visibility
  test ("name visibility") {
    expectResult(S(paper0Name)) { concretize(getAuthorCtxt0(), paper0.title); }
    expectResult(S(emptyName)) { concretize(getAuthorCtxt2(), paper0.title); }

    val viewMap =
      Map( (Submission, S(paper0Name))
         , (Review, S(paper0Name))
         , (Public, S(paper0Name)));
    viewMap.foreach {
      case (stage, r) =>
      expectResult (r) {
          // debugPrint(getReviewerCtxt0(stage), paper0.title)(JConfBackend)
          concretize(getReviewerCtxt0(stage), paper0.title)
        };
        expectResult (r) {
          concretize(getPcCtxt0(stage), paper0.title);
        }
    }

    expectResult(S(emptyName)) {
      concretize(getPublicCtxt0(Submission), paper1.title);
    }
    expectResult(S(paper1Name)) {
      concretize(getPublicCtxt0(Public), paper1.title);
    }
  }

  // Author list visibility
  test ("author list") {
    expectResult (true) {
      concretize(getAuthorCtxt0(), (paper0.authors).has(author0.uid))
    };
    expectResult (true) {
      concretize(getAuthorCtxt1(), (paper0.authors).has(author0.uid))
    }
    expectResult (false) {
      concretize( getReviewerCtxt0(Submission)
                , (paper0.authors).has(author0.uid));
    }
    // TODO: Test that accepted paper author list is visible...
  }

  test ("tag visibility") {
    expectResult (false) {
      concretize(getAuthorCtxt0(Review), paper1.hasTag(Accepted)) }
    expectResult (true) {
      concretize(getAuthorCtxt0(Public), paper1.hasTag(Accepted));
    }
  }

  test ("tag state change") {
    expectResult (false) {
      concretize(getAuthorCtxt0(Public), paper0.hasTag(Accepted));
    }
    paper0.addTag(Accepted);
    expectResult (true) {
      concretize(getAuthorCtxt0(Public), paper0.hasTag(Accepted));
    }
    paper0.removeTag(Accepted);
    expectResult (false) {
      concretize(getAuthorCtxt0(Public), paper0.hasTag(Accepted));
    }

  }
  
  test ("review assignment") {
    expectResult (true) {
      concretize(getReviewerCtxt0(), isAssigned(paper0, reviewer0));
    }
    expectResult (true) { paper0.showNeedsReviewBy(getReviewerCtxt0()) }

    expectResult (true) {
      concretize(getReviewerCtxt1(), isAssigned(paper0, reviewer1));
    }
    expectResult (false) {
      concretize(getReviewerCtxt0(), isAssigned(paper1, reviewer0));
    }
    expectResult (false) {
      concretize(getReviewerCtxt0(), isAssigned(paper1, reviewer1));
    }

    assignReview(paper1, author0);
    expectResult (false) {
      concretize(getReviewerCtxt0(), isAssigned(paper1, author0)); }
  }

  test ("review tag visibility") {
    expectResult (false) {
      concretize(getAuthorCtxt0(Review)
        , paper0.hasTag(ReviewedBy(reviewer0.uid)));
    }
    expectResult (true) {
      concretize(getReviewerCtxt0(Review)
        , paper0.hasTag(ReviewedBy(reviewer0.uid)));
    }
    expectResult (true) {
      concretize(getPcCtxt0(Review)
        , paper0.hasTag(ReviewedBy(reviewer0.uid)));
    }
  }

  test ("review visibility") {
    expectResult(reviewer0.uid) {
      concretize(getAuthorCtxt0(Review), paper0Review.reviewer);
    }
  }
  
  test("paper visibility of rejected paper") {
    expectResult(false) {
      concretize( getPublicCtxt0(Public)
                , searchByTitle("my paper").has(paper0) );
    }
  }

  test("paper visibility of accepted paper") {
    expectResult(true) {
      concretize( getPublicCtxt0(Public)
                , searchByTitle("hello world").has(paper1) );
    }
  }
  test ("author visibility") {
    expectResult(true) {
      concretize( getPublicCtxt0(Public)
                , searchByAuthor(author2).has(paper1) );
    }

  }
  
  test ("login") {
    expectResult(Some(author0)) {
      loginUser("author0", "a0p");
    }
    expectResult(None) {
      loginUser("author0", "xxx");
    }
  }
}
