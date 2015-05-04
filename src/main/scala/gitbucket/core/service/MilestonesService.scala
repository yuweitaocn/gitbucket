package gitbucket.core.service

import gitbucket.core.model.Milestone
import gitbucket.core.model.Profile._
import profile.api._
// TODO Why is direct import required?
import gitbucket.core.model.Profile.dateColumnType

trait MilestonesService {

  def createMilestone(owner: String, repository: String, title: String, description: Option[String],
                      dueDate: Option[java.util.Date])(implicit s: Database): Unit = run {
    Milestones += Milestone(
      userName       = owner,
      repositoryName = repository,
      title          = title,
      description    = description,
      dueDate        = dueDate,
      closedDate     = None
    )
  }

  def updateMilestone(milestone: Milestone)(implicit db: Database): Unit = run {
    Milestones
      .filter (t =>  t.byPrimaryKey(milestone.userName, milestone.repositoryName, milestone.milestoneId))
      .map    (t => (t.title, t.description.?, t.dueDate.?, t.closedDate.?))
      .update (milestone.title, milestone.description, milestone.dueDate, milestone.closedDate)
  }

  def openMilestone(milestone: Milestone)(implicit db: Database): Unit =
    updateMilestone(milestone.copy(closedDate = None))

  def closeMilestone(milestone: Milestone)(implicit db: Database): Unit =
    updateMilestone(milestone.copy(closedDate = Some(currentDate)))

  def deleteMilestone(owner: String, repository: String, milestoneId: Int)(implicit db: Database): Unit = run {
    DBIO.seq(
      Issues.filter(_.byMilestone(owner, repository, milestoneId)).map(_.milestoneId.?).update(None),
      Milestones.filter(_.byPrimaryKey(owner, repository, milestoneId)).delete
    )
  }

  def getMilestone(owner: String, repository: String, milestoneId: Int)(implicit db: Database): Option[Milestone] = run {
    Milestones.filter(_.byPrimaryKey(owner, repository, milestoneId)).result.headOption
  }

  def getMilestonesWithIssueCount(owner: String, repository: String)(implicit db: Database): List[(Milestone, Int, Int)] = {
    val counts = run { Issues
      .filter  { t => (t.byRepository(owner, repository)) && (t.milestoneId.? isDefined) }
      .groupBy { t => t.milestoneId -> t.closed }
      .map     { case (t1, t2) => t1._1 -> t1._2 -> t2.length }
      .result
    }.toMap

    getMilestones(owner, repository).map { milestone =>
      (milestone, counts.getOrElse((milestone.milestoneId, false), 0), counts.getOrElse((milestone.milestoneId, true), 0))
    }
  }

  def getMilestones(owner: String, repository: String)(implicit db: Database): List[Milestone] = run {
    Milestones.filter(_.byRepository(owner, repository)).sortBy(_.milestoneId asc).result
  }.toList

}
