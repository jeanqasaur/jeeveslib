package cap.jeeveslib.demo.cfm

/*
 * User records for jconf case study.
 * @author jeanyang
 */
import JConfBackend._
import cap.jeeveslib.ast.{Atom, Formula, IntExpr, ObjectExpr, S}

class PaperReview(
            val  uid: BigInt
  ,         val  key: String
  ,         var  paperId: BigInt = -1
  , private val _reviewerId: BigInt = -1
  , private var _body: String = ""
  , private var _problemScore: Int = 3
  , private var _backgroundScore: Int = 3
  , private var _approachScore: Int = 3
  , private var _resultScore: Int = 3) extends Atom {
  /*************/
  /* Policies. */
  /*************/
  private val _reviewerL = mkLabel ();
  private val _scoreL = mkLabel ();
  private def _isInternalF (ctxt: ObjectExpr[ConfContext]): Formula = {
    val vrole = ctxt.viewer.role;
    (vrole === ReviewerStatus) || (vrole === PCStatus);
  }
  restrict(_reviewerL,
    (ctxt: ObjectExpr[ConfContext]) => true) //_isInternalF (ctxt))

  val reviewer: IntExpr = mkSensitiveInt(_reviewerL, _reviewerId, -1)
  def showReviewer(ctxt: ConfContext): ConfUser = {
    val reviewerId: BigInt =
      concretize(ctxt, reviewer).asInstanceOf[BigInt]
    getUserById(reviewerId.toInt) match {
      case Some(u) => u
      case None => defaultUser
    }
  }
  def getReviewerTag(): ObjectExpr[PaperTag] =
    mkSensitive(_reviewerL, ReviewedBy(_reviewerId), EmptyTag)
  def showReviewerTag(ctxt: ConfContext): PaperTag = {
    println("showing reviewer tag")
    concretize(ctxt, getReviewerTag()).asInstanceOf[PaperTag]
  }

  def setBody (newbody: String) = _body = newbody
  def getBody (): String = _body
  def showBody(ctxt: ConfContext): String = _body

  /* Score. */
  def setProblemScore (newscore: Int) = _problemScore = newscore
  def getProblemScore (): Int = _problemScore
  def showProblemScore (ctxt: ConfContext): Int = _problemScore

  def setBackgroundScore (newscore: Int) = _backgroundScore = newscore
  def getBackgroundScore (): Int = _backgroundScore
  def showBackgroundScore (ctxt: ConfContext): Int = _backgroundScore

  def setApproachScore (newscore: Int) = _approachScore = newscore
  def getApproachScore (): Int = _approachScore
  def showApproachScore (ctxt: ConfContext): Int = _approachScore

  def setResultScore (newscore: Int) = _resultScore = newscore
  def getResultScore (): Int = _resultScore
  def showResultScore (ctxt: ConfContext): Int = _resultScore

  /* URL links. */
  private val _reviewL = mkLabel()
  restrict ( _reviewL
    , (ctxt: ObjectExpr[ConfContext]) =>
        !((ctxt.viewer.role === ReviewerStatus) ||
        (ctxt.viewer.role === PCStatus) ||
        ((ctxt.viewer.role === AuthorStatus) &&
        ctxt.stage === Public)) )
  private val _editL = mkLabel()
  restrict( _editL
    , (ctxt: ObjectExpr[ConfContext]) => !((ctxt.viewer~'uid === reviewer)
      && (ctxt.stage === Review)) )

  private val _reviewLink: String = "review?id=" + uid + "&key=" + key
  val reviewLink: ObjectExpr[S] =
    mkSensitive(_reviewL, S(_reviewLink), S(""))
  def showReviewLink(ctxt: ConfContext): String = {
    (concretize(ctxt, reviewLink).asInstanceOf[S]).s
  }
  private val _editReviewLink = "edit_review?id=" + uid + "&key=" + key
  val editReviewLink: ObjectExpr[S] =
    mkSensitive(_editL, S(_editReviewLink), S(""))
  def showEditReviewLink(ctxt: ConfContext): String = {
    (concretize(ctxt, editReviewLink).asInstanceOf[S]).s
  }
  private val _postReviewLink = "review?id=" + uid + "&key=" + key
  val postReviewLink: ObjectExpr[S] =
    mkSensitive(_editL, S(_postReviewLink), S(""))
  def showPostReviewLink(ctxt: ConfContext): String = {
    println("showing post review link")
    (concretize(ctxt, postReviewLink).asInstanceOf[S]).s
  }
}
