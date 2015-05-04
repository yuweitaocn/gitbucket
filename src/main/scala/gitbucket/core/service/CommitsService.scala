package gitbucket.core.service

import gitbucket.core.model.CommitComment
import gitbucket.core.model.Profile._
import profile.api._

// TODO Why is direct import required?
import gitbucket.core.model.Profile.dateColumnType

trait CommitsService {

  def getCommitComments(owner: String, repository: String, commitId: String, pullRequest: Boolean)
                       (implicit db: Database) = run {
    CommitComments.filter {
      t => t.byCommit(owner, repository, commitId) && (t.pullRequest === pullRequest || pullRequest)
    }.result
  }.toList

  def getCommitComment(owner: String, repository: String, commentId: String)(implicit db: Database) = {
    if (commentId forall (_.isDigit)){
      run {
        CommitComments.filter { t =>
          t.byPrimaryKey(commentId.toInt) && t.byRepository(owner, repository)
        }.result.headOption
      }
    } else None
  }

  def createCommitComment(owner: String, repository: String, commitId: String, loginUser: String,
      content: String, fileName: Option[String], oldLine: Option[Int], newLine: Option[Int], pullRequest: Boolean)
     (implicit s: Session): Int = {
    // TODO Move to DBIOAction
    CommitComments.autoInc insert CommitComment(
      userName          = owner,
      repositoryName    = repository,
      commitId          = commitId,
      commentedUserName = loginUser,
      content           = content,
      fileName          = fileName,
      oldLine           = oldLine,
      newLine           = newLine,
      registeredDate    = currentDate,
      updatedDate       = currentDate,
      pullRequest       = pullRequest)
  }

  def updateCommitComment(commentId: Int, content: String)(implicit db: Database) = run {
    CommitComments
      .filter (_.byPrimaryKey(commentId))
      .map { t =>
      t.content -> t.updatedDate
    }.update (content, currentDate)
  }

  def deleteCommitComment(commentId: Int)(implicit db: Database) = run {
    CommitComments filter (_.byPrimaryKey(commentId)) delete
  }
}
