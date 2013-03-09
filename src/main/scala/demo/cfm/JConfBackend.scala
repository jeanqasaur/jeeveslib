package cap.jeeveslib.demo.cfm

import cap.jeeveslib._
import cap.jeeveslib.ast.{Formula, ObjectExpr, S}
import cap.jeeveslib.jeeves._

import scala.collection.mutable.Map
import scala.collection.mutable.Set

object JConfBackend extends JeevesLib[ConfContext] {
  class AccessError extends Exception
  class PermissionsError extends Exception
  class NoSuchUserError(uid: Int) extends Exception
  class NoSuchPaperError(uid: Int) extends Exception
  class TypeError extends Exception

  /* Database initialization. */
  private val jconfUsers: Map[Int, ConfUser] = Map[Int, ConfUser]()
  private val jconfPapers: Map[Int, PaperRecord] =  Map[Int, PaperRecord]()
  private val jconfReviews: Map[Int, PaperReview] = Map[Int, PaperReview]()

  // A list of conflicts.
  private var conflicts: List[ConfUser] = Nil

  // A list of the PC members.
  private var pcMembers: List[ConfUser] = conflicts.filter(_.role == PCStatus)

  /* Debugging variables. */
  private var numConfUserPs = 0;
  def logConfUserPolicy() = numConfUserPs = numConfUserPs + 1
  private var numPaperRecordPs = 0;
  def logPaperRecordPolicy() = numPaperRecordPs = numPaperRecordPs + 1
  private var numPaperReviewPs = 0;
  def logPaperReviewPolicy() = numPaperReviewPs = numPaperReviewPs + 1
  def printPolicyCount() = {
    println("***")
    println("# ConfUser policies: " + numConfUserPs)
    println("# PaperRecord policies: " + numPaperRecordPs)
    println("# PaperReview policies: " + numPaperReviewPs)
    println("***")
  }

  val emptyStringVal = S("")

  val defaultUser = {
    getUserById(1) match {
      // If the default user exists already...
      case Some(u) => u
      // Otherwise create a new one.
      case None =>
        val defaultEmail = "defaultUser";
        val defaultName = "Default User";
        val defaultAffiliation = "";
        val defaultPwd = "";
        val defaultGrad = false;
        val defaultAcm = ""
        val defaultStatus = PublicStatus;
        val defaultConflicts = Nil;
        val (id, secretId) = getUserUid()
        new ConfUser ( id, secretId
          , defaultEmail, defaultName, defaultAffiliation
          , defaultPwd, defaultGrad, defaultAcm
          , defaultStatus
          , defaultConflicts )
    }
  }
  val defaultPaper = {
    getPaperById(1) match {
      case Some(p) => p
      case None =>
        val defaultTitle = "---";
        val (id, secretId) = getPaperUid(defaultTitle, "")
        new PaperRecord (id, secretId, defaultTitle, _conflicts=Nil)
    }
  }
  val defaultReview: PaperReview = new PaperReview (uid=0, key="defaultKey")

  /* Making papers. */
  private var _usercount = 1;
  private def getUserUid (): (Int, String) = {
    // Generate a secretId.
    val secretId = RandomGenerator.generateSecretId();
    val userId = _usercount
    _usercount = _usercount + 1
    (userId, secretId)
  }
  private var _papercount = 1;
  private def getPaperUid (title: String, file: String) : (Int, String) = {
    val secretId = RandomGenerator.generateSecretId();
    val paperId = _papercount
    _papercount = _papercount + 1
    (paperId, secretId)
  }
  private var _reviewcount = 1;
  def getReviewUid (
    paperId: Int = -1, reviewerId: Int = -1, body: String = ""
    , problemScore: Int = 3, backgroundScore: Int = 3
    , approachScore: Int = 3, resultScore: Int = 3): (Int, String) = {
    val key = RandomGenerator.generateSecretId();
    val reviewId = _reviewcount
    _reviewcount = _reviewcount + 1
    (reviewId, key)
  }

  def addUser(email: String
    , name: String, affiliation: String
    , password: String, isGrad: Boolean, acmNum: String
    , role: UserStatus, userConflicts: List[BigInt] = Nil): ConfUser = {
    // For this conference, only authors should be allowed to list conflicts.
    if (role != AuthorStatus) {
      assert(userConflicts == Nil);
    }

    val (id, secretId) = getUserUid ()
    val user =
      new ConfUser(id, secretId, email, name, affiliation
      , password, isGrad, acmNum, role, userConflicts);

    // Add conflict if necessary.
    if ((role == ReviewerStatus) || (role == PCStatus)) {
      conflicts = user::conflicts
    }
    if (role == PCStatus) {
      pcMembers = user::pcMembers;
    }

    cacheUser(user)
    user
  }

  /* This function should only be called for brand-new papers. */
  def addPaper(name : String, authors : List[ConfUser]
    , file: String = "", tags : List[PaperTag] = Nil)
      : PaperRecord = {
    // TODO: Make sure UID is unique...
    val (uid, secretId) = getPaperUid (name, file);

    var conflicts: List[BigInt] = List()
    authors.foreach { a => a.getConflicts() ::: conflicts
    }
    val paper =
      new PaperRecord(uid, secretId, name, authors.map(a => a.uid)
      , file, tags, conflicts)
    authors.foreach(a => { a.addSubmittedPaper(paper) })

    // Assign the PC member to review the paper.
    pcMembers.foreach { u => assignReview(paper, u) }

    // Add paper to in-memory cache.
    cachePaper(paper)
    paper
  }
 
  /* Reviews. */
  def assignReview (p: PaperRecord, reviewer: ConfUser): Unit = {
    // TODO: Where do we want to have integrity policies about this manifest?
    if (!((reviewer.role == ReviewerStatus) || (reviewer.role == PCStatus)))
      return;

    // Write persistently.
    val review = p.addReview(reviewer)
    cacheReview(review)
    p.addTag(NeedsReview(reviewer.uid))
  }
  def isAssigned (p: PaperRecord, reviewer: ConfUser): Formula = {
    p.hasTag(NeedsReview(reviewer.uid))
  }
  
  /* Searching. */
  def lookupCachedPaper(uid: Int): Option[PaperRecord] = { jconfPapers.get(uid) }
  def cachePaper(paper: PaperRecord) = jconfPapers += (paper.uid.toInt -> paper)
  def getPaperById(uid: Int): Option[PaperRecord] = {
    lookupCachedPaper(uid) match {
      case Some(paper) => Some(paper)
      case None => None
    }
  }
  def getPapersByIds(ids: List[Int]): List[Option[PaperRecord]] =
    ids.map(id => getPaperById(id))
  def getPapersByReviewer(reviewerId: Int)
    : List[ObjectExpr[PaperRecord]] =
    jfilter[PaperRecord](getAllPapers(), defaultPaper
      , (p: PaperRecord) =>
          p.reviews.hasFormula(r => r~'reviewer === reviewerId))
  
  def getReviewsByReviewer(reviewerId: Int)
    : List[ObjectExpr[PaperReview]] =
    jfilter[PaperReview](getAllReviews(), defaultReview
      , (r: PaperReview) => r.reviewer === reviewerId)
  
  def getReviewsByPaperReviewer(paperId: Int, reviewerId: Int)
    : List[ObjectExpr[PaperReview]] = {
    jfilter[PaperReview](getAllReviews(), defaultReview
      , (r: PaperReview) =>
          (r.paperId == paperId) && (r.reviewer === reviewerId))
  }

  def getAllPapers(): List[PaperRecord] = jconfPapers.values.toList
  def getAllReviews(): List[PaperReview] = jconfReviews.values.toList
  
  /*
  def searchByTitle(title: String) = 
    getAllPapers().filter(_.title === StringVal(this, title))
  */
  /*
  def searchByAuthor(author: ConfUser) = 
    JConfTables.getAllDBPapers().filter(_.getAuthors().has(author))
  */
  /*
  def searchByTag(tag: PaperTag) = {
    getAllPapers().filter(_.getTags().has(tag))
  }
  */
 
  def lookupCachedUser(uid: Int): Option[ConfUser] = { jconfUsers.get(uid) }
  def cacheUser(user: ConfUser) = jconfUsers += (user.uid.toInt -> user)
  def getUserById(uid: Int): Option[ConfUser] = {
    if (uid < 0) { Some(defaultUser)
    } else {
      lookupCachedUser(uid) match {
        case Some(user) => Some(user)
        case None => None
      }
    }
  }
  def getUserByEmail(username: String): Option[ConfUser] = {
    jconfUsers.find(v => v._2.email == username) match {
      case Some(v) => Some(v._2)
      case None => None
    }
  }

  def loginUser(uname: String, password: String): Option[ConfUser] = {
   getUserByEmail(uname) match {
      case Some(user) =>
        cacheUser(user);
        val userCtxt = new ConfContext(user, Public);
        val pwd : String = user.showPassword(userCtxt);
        if (pwd.equals(password)) Some(user) else None
      case None => None
    }
  }

  def lookupCachedReview(uid: Int): Option[PaperReview] = {
    jconfReviews.get(uid)
  }
  def cacheReview(review: PaperReview): Unit =
    jconfReviews += (review.uid.toInt -> review)
  def getReviewById(uid: Int): Option[PaperReview] = {
    if (uid < 0) { Some(defaultReview)
    } else { lookupCachedReview(uid) }
  }

  def showAuthorsByPaper(ctxt: ConfContext, p: PaperRecord): List[ConfUser] = {
    val authorIds: List[BigInt] = p.showAuthors(ctxt)
    authorIds.map(aid =>
      getUserById(aid.toInt) match {
        case Some(a) => a
        case None => throw new NoSuchUserError(aid.toInt)
      }
    )
  }
}
