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
import cap.jeeves.JeevesTypes._

sealed trait PaperStage extends Atom
object Submission extends PaperStage
object Review extends PaperStage
object Rebuttal extends PaperStage
object Decision extends PaperStage
object Public extends PaperStage

sealed trait PaperTag extends Atom
object NeedsReview extends PaperTag
case class ReviewedBy (reviewer: ConfUser) extends PaperTag
object Accepted extends PaperTag

case class Title (name : String) extends Atom

class PaperRecord( val id : Int
                 , _name : Title, _authors : List[ConfUser]
                 , _papertags : List[PaperTag] ) extends Atom {
  private def isPublic (curtags : List[Sensitive]) (implicit CONTEXT: Sensitive)
  : Formula =
    (CONTEXT.stage === Public) && curtags.has(Accepted)

  // Some predicates...
  private def isAuthor (implicit CONTEXT: Sensitive): Formula =
    _authors.has(CONTEXT.viewer);
  private def isInternal (implicit CONTEXT: Sensitive): Formula =
    (CONTEXT.viewer.role === ReviewerStatus) ||
    (CONTEXT.viewer.role === PCStatus)

  // The name of the paper is always visible to the authors.
  def updateName(_name: Title): Sensitive = {
    val level = mkLevel ();
    restrict (level
      , (CONTEXT: Sensitive) =>
        (isAuthor (CONTEXT) || isInternal (CONTEXT)
          || isPublic (getTags ()) (CONTEXT)));
    mkSensitive(level, _name, Title(""))
  }
  var name = updateName(_name);

  val authors: List[Sensitive] = {
    val level = mkLevel ();
    restrict ( level
           , (CONTEXT: Sensitive) =>
              (isAuthor (CONTEXT)
                || (isInternal (CONTEXT) && (CONTEXT.stage === Decision)) ||
                isPublic (getTags ()) (CONTEXT)) );
    _authors.map(a => mkSensitive(level, a, NULL))
  }

  /* Managing tags. */
  private def addTagPermission (tag : PaperTag) : Sensitive = {
    val level = mkLevel ();
    tag match {
      case NeedsReview =>
        restrict (level,
          (CONTEXT: Sensitive) =>
            (isInternal (CONTEXT) && (CONTEXT.stage === Review)));
      case ReviewedBy (reviewer) =>
        restrict (level, (CONTEXT: Sensitive) => isInternal (CONTEXT))
      // Can see the "Accepted" tag if is an internal user at the decision
      // stage or if all information is visible.
      case Accepted =>
        restrict (level
          , (CONTEXT: Sensitive) =>
            ((isInternal (CONTEXT) && (CONTEXT.stage === Decision))
              || (CONTEXT.stage === Public)));
    }
    mkSensitive(level, tag, NULL)
  }

  private var actualTags : Map[PaperTag, Sensitive] = {
    val m = Map[PaperTag, Sensitive]();
    _papertags foreach { tag => m += (tag -> addTagPermission(tag)) };
    m
  }
  def getTags () : List[Sensitive] =
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
      restrict( level
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
