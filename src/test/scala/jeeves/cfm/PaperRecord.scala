package test.cap.jeeveslib.jeeves.cfm

/*
 * User records for jconf case study.
 * @author jeanyang
 */

import scala.collection.immutable.List;
import scala.collection.mutable.Map;

import cap.jeeveslib.ast._
import cap.jeeveslib.ast.JeevesTypes._
import Expr._
import CfmBackend._

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
  private def isPublic (curtags : List[ObjectExpr[PaperTag]])
    (implicit ctxt: ObjectExpr[ConfContext])
  : Formula =
    (ctxt.stage === Public) && curtags.has(Accepted)

  // Some predicates...
  private def isAuthor (implicit ctxt: ObjectExpr[ConfContext]): Formula =
    _authors.has(ctxt.viewer);
  private def isInternal (implicit ctxt: ObjectExpr[ConfContext]): Formula =
    (ctxt.viewer.role === ReviewerStatus) || (ctxt.viewer.role === PCStatus)

  // The name of the paper is always visible to the authors.
  def updateName(_name: Title): ObjectExpr[Title] = {
    val level = mkLevel ();
    restrict (level
      , (ctxt: ObjectExpr[ConfContext]) =>
        (isAuthor(ctxt) || isInternal(ctxt)
          || isPublic(getTags())(ctxt)));
    mkSensitive(level, _name, Title(""))
  }
  var name = updateName(_name);

  val authors: List[ObjectExpr[ConfUser]] = {
    val level = mkLevel ();
    restrict ( level
           , (ctxt: ObjectExpr[ConfContext]) =>
              (isAuthor(ctxt)
                || (isInternal(ctxt) && (ctxt.stage === Decision)) ||
                isPublic(getTags ())(ctxt)) );
    _authors.map(a => mkSensitive(level, a, NULL))
  }

  /* Managing tags. */
  private def addTagPermission (tag : PaperTag) : ObjectExpr[PaperTag] = {
    val level = mkLevel ();
    tag match {
      case NeedsReview =>
        restrict (level,
          (ctxt: ObjectExpr[ConfContext]) =>
            (isInternal(ctxt) && (ctxt.stage === Review)));
      case ReviewedBy (reviewer) =>
        restrict (level
          , (ctxt: ObjectExpr[ConfContext]) => isInternal(ctxt))
      // Can see the "Accepted" tag if is an internal user at the decision
      // stage or if all information is visible.
      case Accepted =>
        restrict (level
          , (ctxt: ObjectExpr[ConfContext]) =>
            ((isInternal(ctxt) && (ctxt.stage === Decision))
              || (ctxt.stage === Public)));
    }
    mkSensitive(level, tag, NULL)
  }

  private var actualTags : Map[PaperTag, ObjectExpr[PaperTag]] = {
    val m = Map[PaperTag, ObjectExpr[PaperTag]]();
    _papertags foreach { tag => m += (tag -> addTagPermission(tag)) };
    m
  }
  def getTags () : List[ObjectExpr[PaperTag]] =
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
