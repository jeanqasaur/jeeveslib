package cap.jeeveslib.demo.cfm

import JConfBackend._

object Init {
  def initUsers (): Unit = {
    // Add some dummy users.
    val pcArmando =
      addUser(
        "asolar@mit.edu", "Armando Solar-Lezama", "MIT"
        , "armando", true, "", PCStatus);
      
    val studentJean =
      addUser(
        "jeanyang@csail.mit.edu", "Jean Yang", "MIT CSAIL"
        , "jean", true, "", AuthorStatus, List(pcArmando.uid))

    val authorJean =
      addUser(
        "jeanyang@mit.edu", "Jean Yang", "MIT"
        , "jean", true, "", ReviewerStatus);
    val reviewerKuat =
      addUser(
        "kuat@mit.edu", "Kuat Yessenov", "MIT"
        , "kuat", true, "", ReviewerStatus);

      val paper0Name = "A Language for Automatically Enforcing Privacy";
      val paper0 = addPaper(paper0Name, List(authorJean));
      assignReview(paper0, reviewerKuat);

      val paper1Name = "Matchmaker";
      val paper1 = addPaper(paper1Name, List(reviewerKuat));
      assignReview(paper1, authorJean);
  }
}
