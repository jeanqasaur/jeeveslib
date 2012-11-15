package cap.jeeves.cfm

import cap.scalasmt._
import cap.jeeves._

import scala.collection.mutable.Map
import scala.collection.mutable.Set

import Expr._

object CfmBackend extends JeevesLib {
  // We do not delegate integrity checking to Jeeves.
  private var users : List[ConfUser] = Nil
  private val assignments : Map[Int, Set[ConfUser]] = Map[Int, Set[ConfUser]]()
  private var papers : List[PaperRecord] = Nil

  /* Making papers. */
  private var _papercount = 0;
  private def getPaperUid () : Int = {
    val count = _papercount;
    _papercount = _papercount + 1;
    count
  }

  def addUser(newUser: ConfUser) {
    users = newUser::users 
  }

  def addPaper(name : Title, authors : List[ConfUser], tags : List[PaperTag])
      : PaperRecord = {
    val paper = new PaperRecord(getPaperUid(), name, authors, tags);
    papers = paper::papers;
    paper
  }
 
  /* Reviews. */
  def assignReview (p: PaperRecord, reviewer: ConfUser): Unit = {
    if (!((reviewer.role == ReviewerStatus) || (reviewer.role == PCStatus)))
      return;
    assignments.get(p.id) match {
      case Some(reviewers) => reviewers += reviewer
      case None =>
        val reviewers = Set[ConfUser]();
        reviewers += reviewer;
        assignments += (p.id -> reviewers)
    };
  }
  def isAssigned (p: PaperRecord, reviewer: ConfUser): Boolean = {
    assignments.get(p.id) match {
      case Some(reviewers) => reviewers.contains(reviewer)
      case None => false
    }
  }
  def addReview
    (p: PaperRecord, reviewer: ConfUser, rtext: String, score: Int)
    : Unit = {
      if (isAssigned (p, reviewer))
          p.addReview(reviewer, rtext, score)
  }

  /* Searching. */
  def getById(id: Int) = 
    papers.find ((p: PaperRecord) => p.id == id)
  
  def searchByName(name: String) = 
    papers.filter(_.name === Title(name))
  
  def searchByAuthor(author: ConfUser) = 
    papers.filter(_.authors.has(author))
  
  def searchByTag(tag: PaperTag) = papers.filter(_.getTags().has(tag))
  
}
