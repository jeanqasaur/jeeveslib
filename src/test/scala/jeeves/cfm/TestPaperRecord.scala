package test.cap.jeeveslib.demo.cfm

import cap.jeeveslib.ast._
import cap.jeeveslib.demo.cfm._
import JConfBackend._

import org.scalatest.FunSuite
import org.scalatest.Assertions.{expectResult}

import TestUtil._

class TestPaperRecord extends FunSuite {
  test ("title policy") {
    expectResult(paper0Name) { paper0.showTitle(getAuthorCtxt0(Submission)) }
  }

  test ("isAuthor") {
    def isPaper0Author(ctxt: ConfContext) =
      paper0.authors.hasFormula((aid: IntExpr) =>
      aid === ctxt.viewer.uid);

    // Sanity check.
    expectResult(false) {
      concretize( getAuthorCtxt0(Submission)
        , paper0.authors.hasFormula((aid: IntExpr) =>
          aid === IntVal(-1)) );
    }
    expectResult(false) {
      concretize( getAuthorCtxt0(Submission)
        , paper0.authors.hasFormula((aid: IntExpr) =>
          aid === IntVal(author2.uid)) );
    }
    expectResult(true) {
      concretize(getAuthorCtxt0(Submission)
        , isPaper0Author(getAuthorCtxt0(Submission)))
    }
    expectResult(author2.uid) {
      concretize[BigInt](getAuthorCtxt2(Submission), author2.uid)
    }
    expectResult(false) {
      concretize(getAuthorCtxt2(Submission)
        , isPaper0Author(getAuthorCtxt2(Submission)))
    }
  }

  test ("getAuthors list length") {
    expectResult(2) { paper0.authors.length }
  }

  test ("getAuthors") {
    expectResult(true) {
      concretize(
        getAuthorCtxt0 (Submission)
        , paper0.authors.hasFormula(
          (aid: IntExpr) => aid === IntVal(author0.uid)))
    }
  }

  test ("title visibility - author can see title") {
    expectResult(S(paper0Name)) {
      concretize(getAuthorCtxt0 (Submission), paper0.title)
    }
  }

  test ("title visibility - nonauthor cannot see title") {
    expectResult(S("")) {
      concretize(getAuthorCtxt2 (Submission), paper0.title)
    }
  }

  test ("get by ID") {
    expectResult(paper0.uid) {
      getPaperById(paper0.uid.toInt) match {
        case Some(p)  => p.uid
        case None     => -1
      }
    }
  }
}
