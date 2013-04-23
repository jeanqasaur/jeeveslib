package test.cap.jeeveslib.jeeves.demo.cfm

import cap.jeeveslib.demo.cfm._
import JConfBackend._

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expectResult}

import TestUtil._

class TestConfUser extends FunSuite {
  test ("submitted papers in list") {
    expectResult(true) {
      concretize(
        getContext(author0), author0.submittedPapers.has(paper0.uid)) }
    expectResult(false) {
      concretize(
        getContext(author0), author0.submittedPapers.has(paper1.uid)) }
  }

  test ("password visibility") {
    expectResult("a0p") { author0.showPassword(getContext(author0)) }

    expectResult("default") { author0.showPassword(getContext(author1)) }
  } 

  test ("submitted paper visibility - author") {
    expectResult(true) {
      concretize(
        getContext(author0)
        , (author0.submittedPapers).has(paper0.uid));
    }
  }

  test ("submitted paper visibility - nonauthor") {
    expectResult(false) {
      concretize(
        getContext(author1), (author0.submittedPapers).has(paper0.uid));
    }
  }

  test ("submitted paper exists in list") {
    expectResult(paper0.uid) {
      concretize(getContext(author0), (author0.submittedPapers).head);
    }
  }

  test ("submitted paper title") {
    expectResult(paper0Name) {
      author0.showSubmittedPapers (getContext(author0)).head.showTitle(getContext(author0));
    }
  }

  test ("showTitle with login user") {
    val u = loginUser("author0", "a0p");
    expectResult("my paper") {
      u match {
        case Some(user) =>
        val ctxt = getContext(user);
        ((author0.showSubmittedPapers (ctxt)).head).showTitle (ctxt);
        case None => ""
      }
    }
  }
}
