package gitbucket.core.service

import gitbucket.core.model.Label
import gitbucket.core.model.Profile._
import profile.api._

trait LabelsService {

  def getLabels(owner: String, repository: String)(implicit db: Database): List[Label] = run {
    Labels.filter(_.byRepository(owner, repository)).sortBy(_.labelName asc).result
  }.toList

  def getLabel(owner: String, repository: String, labelId: Int)(implicit db: Database): Option[Label] = run {
    Labels.filter(_.byPrimaryKey(owner, repository, labelId)).result.headOption
  }

  def createLabel(owner: String, repository: String, labelName: String, color: String)(implicit db: Database): Int = run {
    Labels returning Labels.map(_.labelId) += Label(
      userName       = owner,
      repositoryName = repository,
      labelName      = labelName,
      color          = color
    )
  }

  def updateLabel(owner: String, repository: String, labelId: Int, labelName: String, color: String)
                 (implicit db: Database): Unit = run {
    Labels.filter(_.byPrimaryKey(owner, repository, labelId))
          .map(t => t.labelName -> t.color)
          .update(labelName, color)
  }

  def deleteLabel(owner: String, repository: String, labelId: Int)(implicit db: Database): Unit = run {
    DBIO.seq(
      IssueLabels.filter(_.byLabel(owner, repository, labelId)).delete,
      Labels.filter(_.byPrimaryKey(owner, repository, labelId)).delete
    )
  }

}
