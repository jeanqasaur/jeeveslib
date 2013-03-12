package cap.jeeveslib.demo.cfm

/*
* User records for jconf case study.
* @author jeanyang
*/

import JConfBackend._
import cap.jeeveslib.ast.{Atom, Formula, IntExpr, Object, ObjectExpr, S}

import scala.collection.immutable.List;

sealed trait PaperStage extends Atom
case object Submission extends PaperStage
case object Review extends PaperStage
case object Public extends PaperStage

sealed trait PaperTag extends Atom {
  def showTag(ctxt: ConfContext): String
}
case class NeedsReview (reviewer: BigInt) extends PaperTag {
  def showTag(ctxt: ConfContext): String = { "Needs review " + reviewer }
}
case class ReviewedBy (reviewer: BigInt) extends PaperTag {
  def showTag(ctxt: ConfContext): String = { "Reviewed by " + reviewer }
}
case object Accepted extends PaperTag {
  def showTag(ctxt: ConfContext): String = { "Accepted" }
}
case object EmptyTag extends PaperTag {
  def showTag(ctxt: ConfContext): String = { "--" }
}

class PaperRecord(         val uid: BigInt
                 ,         val key: String
                 , private var _title: String
                 , private val _authors: List[BigInt] = Nil
                 , private var _file: String = ""
                 , private var _tags: List[PaperTag] = Nil
                 , private val _conflicts: List[BigInt] )
               extends Atom {
  /**************/
  /* Variables. */
  /**************/
  private var _authorL = mkLevel()
  private var _reviewerL = mkLevel()
  private val titleL = mkLevel();

  /*************/
  /* Policies. */
  /*************/
  private def isAuthor (ctxt: ObjectExpr[ConfContext]): Formula =
    authors.has(ctxt.viewer~'uid);
  private def isInternal (ctxt: ObjectExpr[ConfContext]): Formula =
    (ctxt.viewer.role === ReviewerStatus) ||
    (ctxt.viewer.role === PCStatus)
  private def isPC (ctxt: ObjectExpr[ConfContext]): Formula =
    (ctxt.viewer.role === PCStatus)
  private def authorCanSeeReview (ctxt: ObjectExpr[ConfContext]): Formula =
    (ctxt.stage === Public)
  private def isPublic (ctxt: ObjectExpr[ConfContext]): Formula =
    (ctxt.stage === Public) && (getTags ()).has(Accepted)

  restrict ( _authorL
         , (ctxt: ObjectExpr[ConfContext]) =>
            (isAuthor (ctxt) || isPC (ctxt) || isPublic (ctxt)) );
  logPaperRecordPolicy();
  restrict (titleL
    , (ctxt: ObjectExpr[ConfContext]) => (isAuthor (ctxt)
      || isInternal (ctxt) || isPublic (ctxt)));
  logPaperRecordPolicy();

  /************************/
  /* Getters and setters. */
  /************************/
  def setTitle(name: String) = {
    _title = name
    title =
      mkSensitive(titleL, S(_title), emptyStringVal)
  }
  var title: ObjectExpr[S] =
    mkSensitive(titleL, S(_title), emptyStringVal)
  def showTitle(ctxt: ConfContext): String = {
    (concretize(ctxt, title).asInstanceOf[S]).s
  }

  def authors : List[IntExpr] = {
    _authors.map(author => mkSensitiveInt(_authorL, author, -1))
  }
  def showAuthors(ctxt: ConfContext): List[BigInt] = {
    authors.map(a => concretize(ctxt, a).asInstanceOf[BigInt])
  }

  def getFile(): String = _file
  def setFile(file: String): Unit = _file = file
  def showFile(ctxt: ConfContext): String = _file

  /* Managing tags. */
  private def addTagPermission (tag : PaperTag) : ObjectExpr[PaperTag] = {
    val level = mkLevel ();
    tag match {
      case NeedsReview(reviewerId) =>
        restrict (level
          , (ctxt: ObjectExpr[ConfContext]) =>
            (isPC (ctxt) || (ctxt.viewer~'uid === reviewerId)) );
        logPaperRecordPolicy();
      case ReviewedBy (reviewerId) =>
      restrict (level, (ctxt: ObjectExpr[ConfContext]) =>
        isPC (ctxt) || (ctxt.viewer~'uid === reviewerId));
        logPaperRecordPolicy();
      // Can see the "Accepted(b)" tag if is an internal user at the decision
      // stage or if all information is visible.
      case Accepted =>
        restrict (level
          , (ctxt: ObjectExpr[ConfContext]) =>
              (isInternal (ctxt) || ctxt.stage === Public) );
        logPaperRecordPolicy();
      case EmptyTag => ()
    }
    mkSensitive(level, tag, EmptyTag)
  }

  def addTag (newtag: PaperTag) = { _tags = newtag::_tags }
  def getTags (): List[ObjectExpr[PaperTag]] = {
    _tags.map(t => addTagPermission(t))
  }
  def removeTag (tag : PaperTag) : Unit = { _tags = _tags.filterNot(_ == tag) }
  def hasTag (tag : ObjectExpr[PaperTag]) : Formula = (getTags ()).has(tag)
  def showTags (ctxt: ConfContext): List[PaperTag] = {
    (getTags ()).map(t => concretize(ctxt, t).asInstanceOf[PaperTag])
  }

  def hasConflict(reviewer: BigInt): Boolean = {
    _conflicts.exists(_ == reviewer)
  }

  // We only add to reviews and don't take away.
  var reviews: List[ObjectExpr[PaperReview]] = Nil
  def addReview (reviewer: ConfUser, body: String = ""
    , problemScore: Int = 3, backgroundScore: Int = 3
    , approachScore: Int = 3, resultScore: Int =3)
  : PaperReview = {
   val (reviewUid, reviewKey) =
     getReviewUid(uid.toInt, reviewer.uid.toInt, body
     , problemScore, backgroundScore, approachScore, resultScore);
   val r =
     new PaperReview(reviewUid, reviewKey
     , uid, reviewer.uid, body
     , problemScore, backgroundScore, approachScore, resultScore);
   addTag(ReviewedBy(reviewer.uid))
   reviews = addReviewPolicy(r) :: reviews
   r
  }
  def isReviewedBy(reviewer: ConfUser): Formula = {
    hasTag(ReviewedBy(reviewer.uid))
  }
  def showNeedsReviewBy(ctxt: ConfContext): Boolean = {
    concretize(ctxt, hasTag(NeedsReview(ctxt.viewer.uid)))
  }
  def showIsReviewedBy(ctxt: ConfContext, reviewer: ConfUser): Boolean = {
    concretize(ctxt, isReviewedBy(reviewer))
  }
  def addReviewPolicy (r: ObjectExpr[PaperReview]): ObjectExpr[PaperReview] = {
    val level = mkLevel();
    restrict( level
      , (ctxt: ObjectExpr[ConfContext]) =>
          ( (isInternal (ctxt) && (!isAuthor (ctxt))) ||
                (isAuthor (ctxt) && authorCanSeeReview (ctxt)) ) );
    logPaperRecordPolicy();
    mkSensitive(level, r, Object(defaultReview))
  }
  def showReviews (ctxt: ConfContext): List[PaperReview] = {
    reviews.map(r => concretize(ctxt, r).asInstanceOf[PaperReview])
  }
  def getReviewByReviewer (reviewerId: BigInt)
    : List[ObjectExpr[PaperReview]] = {
    val reviews = getReviewsByPaperReviewer(uid.toInt, reviewerId.toInt)
    reviews.map(r => addReviewPolicy(r))
  }
  def showReviewByReviewer(ctxt: ConfContext, reviewerId: BigInt)
    : PaperReview = {
    concretize(ctxt, getReviewByReviewer(reviewerId)).asInstanceOf[PaperReview]
  }

  def showIsAuthor (ctxt: ConfContext): Boolean =
    concretize(ctxt, isAuthor (ctxt))

  def debugPrint(): Unit = {
    _authors.foreach(a => println(a))
  }
  
  private val _editL = mkLevel()
  restrict(_editL
    , (ctxt: ObjectExpr[ConfContext]) =>
        (isAuthor (ctxt) && (ctxt.stage === Submission)) )

  def showLink(ctxt: ConfContext): String = {
    "paper?id=" + uid + "&key=" + key
  }

  private val path: String = new java.io.File("").getAbsolutePath()
  def getBackupLoc(): ObjectExpr[S] = {
    val backupLoc = path + "/papers/" + "jcp" + key + "_" + _file
    mkSensitive(_editL, S(backupLoc), emptyStringVal)
  }
  def getTomcatLoc(): ObjectExpr[S] = {
    val tomcatLoc = path + "/webapps/src2012/papers/" + "jcp" + key + "_" + _file
    mkSensitive(_editL, S(tomcatLoc), emptyStringVal)
  }
  // Permanent storage location for file.
  def showFileLocations(ctxt: ConfContext): (String, String) = {
    val backupLoc = {
      concretize(ctxt, getBackupLoc ()).asInstanceOf[S]
    }
    val tomcatLoc = concretize(ctxt, getTomcatLoc ()).asInstanceOf[S]
    (backupLoc.s, tomcatLoc.s)
  }
  // Where the file is stored for display online.
  def getFileStorageLocation(paperSecretId: String, filename: String): String = {
    path + "/webapps/src2012/papers/" + "jcp" + paperSecretId + "_" + filename
  }

  // The public link for this directory.
  def showFileDisplayLocation(ctxt: ConfContext): String = {
    "papers/" + "jcp" + key + "_" + showFile(ctxt)
  }

  private val _assignL = mkLevel ()
  restrict (_assignL, (ctxt: ObjectExpr[ConfContext]) => isPC (ctxt))
  private val _assignLink = "assign_paper?id=" + uid + "&key=" + key
  def getAssignLink(userId: BigInt): ObjectExpr[S] = {
    mkSensitive(_assignL
      , S(_assignLink + "&userId=" + userId)
      , emptyStringVal)
  }
  def showAssignLink(ctxt: ConfContext, userId: BigInt): String = {
    concretize(
      ctxt, getAssignLink(userId)).asInstanceOf[S].s
  }

  private val _editLink = "edit_paper?id=" + uid + "&key=" + key
  val editLink: ObjectExpr[S] = {
    mkSensitive(_editL, S(_editLink), emptyStringVal)
  }
  def showEditLink(ctxt: ConfContext): String = {
    concretize(ctxt, editLink).asInstanceOf[S].s
  }

  private val _withdrawLink = "withdraw_paper?id=" + uid + "&key=" + key
  val withdrawLink: ObjectExpr[S] = {
    mkSensitive(_editL, S(_withdrawLink), emptyStringVal)
  }
  def showWithdrawLink(ctxt: ConfContext): String = {
    concretize(ctxt, withdrawLink).asInstanceOf[S].s
  }

  private val _postLink = "paper?id=" + uid + "&key=" + key
  def postLink: ObjectExpr[S] = {
    mkSensitive(_editL, S(_postLink), emptyStringVal)
  }
  def showPostLink(ctxt: ConfContext): String = {
    concretize(ctxt, postLink).asInstanceOf[S].s
  }

  def getUploadLink(file: String): String = {
    if (!file.isEmpty()) {
      "uploadedPaper?id=" + uid + "&key=" + key + "&filename=" + file
    } else { "" }
  }
  def showUploadLink(ctxt: ConfContext): String = {
    getUploadLink(showFile(ctxt))
  }
}
