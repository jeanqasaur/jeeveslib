package cap.jeeveslib.demo.cfm

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.jeeveslib.ast.{Atom, Formula, ObjectExpr, S}
import JConfBackend._

sealed trait UserStatus extends Atom
case object PublicStatus extends UserStatus
case object AuthorStatus extends UserStatus
case object ReviewerStatus extends UserStatus
case object PCStatus extends UserStatus

/* Conference User */
case class ConfUser(
            val uid: BigInt
  ,         val secretId: String  // Used in the link
  ,         val email: String
  , private var _name: String
  , private var _affiliation: String
  , private var _password: String
  , private var _isGrad: Boolean
  , private var _acmNum: String = ""
  ,         val role: UserStatus
  , private var _conflicts: List[BigInt] )
  extends Atom {
    /*************/
    /* Policies. */
    /*************/
    private def isSelf (ctxt: ObjectExpr[ConfContext]) : Formula =
      ctxt.viewer~'uid === uid

    private def isReviewer (ctxt: ObjectExpr[ConfContext]): Formula =
      ctxt.viewer.status === ReviewerStatus
    private def isPC (ctxt: ObjectExpr[ConfContext]): Formula =
      ctxt.viewer.status === PCStatus

    private val selfL = mkLevel ();
    restrict (selfL, (ctxt: ObjectExpr[ConfContext]) => isSelf (ctxt));
    logConfUserPolicy();
    def showIsSelf(ctxt: ConfContext): Boolean = {
      concretize(ctxt, selfL)
    }

    // No policies on name--should always be high...
    def setName (n: String): Unit = { _name = n; }
    def showName (ctxt: ConfContext): String = {
      _name
    }
    def setAffiliation (a: String): Unit = {
      _affiliation = a;
    }
    def showAffiliation (ctxt: ConfContext): String = {
      _affiliation
    }

    def setIsGrad (isGrad: Boolean): Unit = {
      _isGrad = isGrad
    }
    def showIsGrad(ctxt: ConfContext): Boolean = {
      _isGrad
    }

    private val numL = mkLevel()
    restrict (numL, (ctxt: ObjectExpr[ConfContext]) =>
      (isSelf (ctxt) || isPC (ctxt)))
    var acmNum = mkSensitive(numL, S(_acmNum), S(""))
    def setAcmNum (newNum: String): Unit = {
      _acmNum = newNum
      acmNum = mkSensitive(numL, S(_acmNum), S(""))
    }
    def showAcmNum (ctxt: ConfContext): String = {
      (concretize(ctxt, acmNum).asInstanceOf[S]).s
    }

    // Submitted papers.
    var _submittedPapers: List[BigInt] = Nil
    var submittedPapers =
      _submittedPapers.map(p => mkSensitiveInt(selfL, p, -1))
    def showSubmittedPapers (ctxt: ConfContext): List[PaperRecord] = {
      val paperIds: List[BigInt] =
        submittedPapers.map(p => concretize(ctxt, p));
        paperIds.map(pid => getPaperById(pid.toInt) match {
            case Some(paper) => paper
            case None => defaultPaper
          })
    }
    def addSubmittedPaper (p: PaperRecord): Unit = {
      _submittedPapers = p.uid :: _submittedPapers
      submittedPapers = (mkSensitiveInt(selfL, p.uid, -1)) :: submittedPapers
    }
    // TODO: Withdraw submitted paper
    def withdrawSubmittedPaper (p: PaperRecord): Unit = {
      // Remove from list and create a new sensitive list. 
      _submittedPapers = _submittedPapers.filterNot(_ == p.uid)
      submittedPapers =
        _submittedPapers.map(p => mkSensitiveInt(selfL, p, -1))
    }

    // Papers to review.
    def getReviewPapers (): List[ObjectExpr[PaperRecord]] = {
      val papers: List[ObjectExpr[PaperRecord]] =
        getPapersByReviewer(uid.toInt);
      papers.map(p => mkSensitive(selfL, p, defaultPaper))
    }
    def showReviewPapers (ctxt: ConfContext): List[PaperRecord] = {
      (getReviewPapers ()).map(p =>
        concretize(ctxt, p).asInstanceOf[PaperRecord])
    }

    // Reviews submitted.
    def getReviews (): List[ObjectExpr[PaperReview]] = {
      val reviews: List[ObjectExpr[PaperReview]] =
        getReviewsByReviewer(uid.toInt);
      reviews.map(r => mkSensitive(selfL, r, defaultReview))
    }
    def showReviews (ctxt: ConfContext): List[PaperReview] = {
      (getReviews ()).map(r =>
        concretize(ctxt, r).asInstanceOf[PaperReview])
    }

    // Password.
    def setPassword (p: String) = {
      _password = p
      password =
        mkSensitive(selfL, S(_password), S("default"))
    }
    var password =
      mkSensitive(selfL, S(_password), S("default"))
    def showPassword (ctxt: ConfContext): String = {
      concretize(ctxt, password).asInstanceOf[S].s
    }

    def addConflict(c: BigInt): Unit = {
      _conflicts = c::_conflicts
    }
    def setConflicts(cs: List[BigInt]): Unit = { _conflicts = cs }
    // Conflicts are public right now...
    def getConflicts(): List[BigInt] = {
      _conflicts
    }
    def hasConflict(userId: BigInt): Boolean = {
      (getConflicts ()).exists(_ == userId)
    }
    def showHasConflict(ctxt: ConfContext, userId: BigInt): Boolean = {
      hasConflict(userId)
    }

    def update(params: Map[String, String]
      , conflicts: List[BigInt])
    : Unit = {
      val name: String = params("name")
      val affiliation: String = params("affiliation")
      val isGrad: Boolean = {
        try { params("isGrad") == "yes" }
        catch { case e: Exception => false }
      }
      var acmNum: String = {
        try { params("acmNum") } catch { case e: Exception => "" }
      }

      setName(name)
      setAffiliation(affiliation)
      setIsGrad(isGrad)
      setAcmNum(acmNum)
      setConflicts(conflicts)
    }

    def debugPrint(): Unit = {
      println("ConfUser(id=" + uid + ",email=" + email + ")")
    }
  }
