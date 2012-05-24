package cap.jeeves.cfm

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import cap.scalasmt._
import scala.collection.immutable.List;
import scala.collection.mutable.Map;

import Expr._
import CfmBackend._

sealed trait PaperStage extends JeevesRecord
object Submission extends PaperStage
object Review extends PaperStage
object Rebuttal extends PaperStage
object Decision extends PaperStage
object Public extends PaperStage

sealed trait PaperTag extends JeevesRecord
object NeedsReview extends PaperTag
case class ReviewedBy (reviewer: ConfUser) extends PaperTag
object Accepted extends PaperTag

case class Title (name : String) extends JeevesRecord

class PaperRecord( val id : Int
                 , _name : Title, _authors : List[ConfUser]
                 , _papertags : List[PaperTag] ) extends JeevesRecord {
  private def isPublic (curtags : List[Symbolic]) : Formula =
    (CONTEXT.stage === Public) && curtags.has(Accepted)

  // Some predicates...
  private val isAuthor: Formula = _authors.has(CONTEXT.viewer);
  private val isInternal: Formula =
    (CONTEXT.viewer.role === ReviewerStatus) ||
    (CONTEXT.viewer.role === PCStatus)

  // The name of the paper is always visible to the authors.
  def updateName(_name: Title): Symbolic = {
    val level = mkLevel ();
    policy (level, !(isAuthor || isInternal || isPublic(getTags ())));
    mkSensitive(level, _name, Title(""))
  }
  var name = updateName(_name);

  val authors: List[Symbolic] = {
    val level = mkLevel ();
    policy ( level
           , !(isAuthor || (isInternal && (CONTEXT.stage === Decision)) ||
              isPublic(getTags ())) );
    _authors.map(a => mkSensitive(level, a, NULL))
  }

  /* Managing tags. */
  private def addTagPermission (tag : PaperTag) : Symbolic = {
    val level = mkLevel ();
    tag match {
      case NeedsReview =>
        val canSee : Formula = isInternal && CONTEXT.stage === Review;
        policy (level, !canSee);
      case ReviewedBy (reviewer) =>
        policy (level, !isInternal)
      // Can see the "Accepted" tag if is an internal user at the decision
      // stage or if all information is visible.
      case Accepted =>
        val stage = CONTEXT.stage;
        val canSee : Formula =
          (isInternal && (stage == Decision)) || (stage === Public);
        policy (level, canSee);
        policy (level, !canSee);
    }
    mkSensitive(level, tag, NULL)
  }

  private var actualTags : Map[PaperTag, Symbolic] = {
    val m = Map[PaperTag, Symbolic]();
    _papertags foreach { tag => m += (tag -> addTagPermission(tag)) };
    m
  }
  def getTags () : List[Symbolic] =
    (actualTags.toList).map(x => x._2)
  def addTag (newtag : PaperTag) : Unit = {
    actualTags += (newtag -> addTagPermission(newtag))
  }
  def removeTag (oldtag : PaperTag) : Unit = actualTags -= oldtag
  def hasTag (tag : PaperTag) : Formula = (getTags ()).has(tag)

  /* Managing reviews. */
  private var reviewIds = 0;
  private def getReviewId () : Int = {
    val id = reviewIds;
    reviewIds = reviewIds + 1;
    id
  }
  
  private var reviews : List[PaperReview] = Nil
  def addReview (reviewer: ConfUser, rtext: String, score: Int)
  : PaperReview = {
    val reviewId = getReviewId ();
    val r = new PaperReview(reviewId, reviewer, rtext, score);
      /*
      val level = mkLevel();
      val s = 
        new PaperReview(reviewId, reviewer, rtext, score);
      policy( level
            , !((CONTEXT.stage === Review && (hasTag (ReviewedBy (reviewer)))) ||
                ((CONTEXT.stage === Decision) && isInternal) ||
                (isAuthor &&
                  ((CONTEXT.stage === Rebuttal) ||
                    (CONTEXT.stage === Decision))))

            , LOW);
      mkSensitive(level, s, NULL)
    } */
    reviews = r::reviews;
    addTag (ReviewedBy(reviewer))
    r
  }
}
