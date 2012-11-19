package test.cap.jeeveslib.jeeves.cfm

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import CfmBackend._

class PaperReview(id: Int, reviewerV: ConfUser, var body: String, var score: Int) {
  private val reviewer = reviewerV;
  def getReviewer (ctxt: ConfContext): ConfUser = {
    reviewer
    // TODO: Use Jeeves policies
    /*
    val level = mkLevel();
    val vrole = CONTEXT.viewer.role;
    if ((vrole == ReviewerStatus) || (vrole == PCStatus))
      reviewer
    else null
    */
  }

  def updateBody (newbody: String) = body = newbody
  def updateScore (newscore: Int) = score = newscore
}
