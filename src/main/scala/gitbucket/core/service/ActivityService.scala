package gitbucket.core.service

import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.source.ObjectSource
import org.elasticsearch.search.sort.SortOrder
import gitbucket.core.model.Activity
import gitbucket.core.model.Profile._
import gitbucket.core.servlet.ElasticsearchServer.client
import gitbucket.core.util.JGitUtil
import profile.simple._
import gitbucket.core.util.Elastic4sSupport._

trait ActivityService {

  def deleteOldActivities(limit: Int)(implicit s: Session): Int = {
    Activities.map(_.activityId).sortBy(_ desc).drop(limit).firstOption.map { id =>
      Activities.filter(_.activityId <= id.bind).delete
    } getOrElse 0
  }

  def getActivitiesByUser(activityUserName: String, isPublic: Boolean)(implicit s: Session): List[Activity] = client.search {
    search in "gitbucket" / "activity" postFilter {
      if(isPublic){
        must(
          termFilter("activityUserName", activityUserName),
          hasParentFilter("repository") filter {
            termFilter("isPrivate", false)
          }
        )
      } else {
        termFilter("activityUserName", activityUserName)
      }
    } sort {
      by field "activityDate" order SortOrder.DESC
    } limit 30
  }.docs[Activity].toList

  def getRecentActivities()(implicit s: Session): List[Activity] = client.search {
    search in "gitbucket" / "activity" postFilter {
      hasParentFilter("repository") filter {
        termFilter("isPrivate", false)
      }
    } sort {
      by field "activityDate" order SortOrder.DESC
    } limit 30
  }.docs[Activity].toList

  def getRecentActivitiesByOwners(owners : Set[String])(implicit s: Session): List[Activity] = client.search {
    search in "gitbucket" / "activity" postFilter {
      should (
        hasParentFilter("repository") filter {
          termFilter("isPrivate", false)
        },
        termsFilter("userName", owners.toSeq: _*)
      )
    } sort {
      by field "activityDate" order SortOrder.DESC
    } limit 30
  }.docs[Activity].toList

  private def repositoryId(userName: String, repositoryName: String): String = client.search {
    search in "gitbucket" / "repository" query {
      must(termQuery("userName", userName), termQuery("repositoryName", repositoryName))
    } fields("_id")
  }.getHits.getHits.head.getId

  private def recordActivity(userName: String, repositoryName: String, activityUserName: String,
                              activityType: String, message: String, additionalInfo: Option[String] = None) = client.update {
    index into "gitbucket" / "activity" doc ObjectSource(
      Activity(userName, repositoryName, activityUserName, activityType, message, additionalInfo, currentDate)
    ) parent repositoryId(userName, repositoryName)
  }

  def recordCreateRepositoryActivity(userName: String, repositoryName: String, activityUserName: String)
                                    (implicit s: Session): Unit =
    recordActivity(userName, repositoryName, activityUserName,
      "create_repository",
      s"[user:${activityUserName}] created [repo:${userName}/${repositoryName}]")

  def recordCreateIssueActivity(userName: String, repositoryName: String, activityUserName: String, issueId: Int, title: String)
                               (implicit s: Session): Unit =
    recordActivity(userName, repositoryName, activityUserName,
      "open_issue",
      s"[user:${activityUserName}] opened issue [issue:${userName}/${repositoryName}#${issueId}]",
      Some(title))

  def recordCloseIssueActivity(userName: String, repositoryName: String, activityUserName: String, issueId: Int, title: String)
                              (implicit s: Session): Unit =
    recordActivity(userName, repositoryName, activityUserName,
      "close_issue",
      s"[user:${activityUserName}] closed issue [issue:${userName}/${repositoryName}#${issueId}]",
      Some(title))

  def recordClosePullRequestActivity(userName: String, repositoryName: String, activityUserName: String, issueId: Int, title: String)
                                    (implicit s: Session): Unit =
    recordActivity(userName, repositoryName, activityUserName,
      "close_issue",
      s"[user:${activityUserName}] closed pull request [pullreq:${userName}/${repositoryName}#${issueId}]",
      Some(title))

  def recordReopenIssueActivity(userName: String, repositoryName: String, activityUserName: String, issueId: Int, title: String)
                               (implicit s: Session): Unit =
    recordActivity(userName, repositoryName, activityUserName,
      "reopen_issue",
      s"[user:${activityUserName}] reopened issue [issue:${userName}/${repositoryName}#${issueId}]",
      Some(title))

  def recordCommentIssueActivity(userName: String, repositoryName: String, activityUserName: String, issueId: Int, comment: String)
                                (implicit s: Session): Unit =
    recordActivity(userName, repositoryName, activityUserName,
      "comment_issue",
      s"[user:${activityUserName}] commented on issue [issue:${userName}/${repositoryName}#${issueId}]",
      Some(cut(comment, 200)))

  def recordCommentPullRequestActivity(userName: String, repositoryName: String, activityUserName: String, issueId: Int, comment: String)
                                      (implicit s: Session): Unit =
    recordActivity(userName, repositoryName, activityUserName,
      "comment_issue",
      s"[user:${activityUserName}] commented on pull request [pullreq:${userName}/${repositoryName}#${issueId}]",
      Some(cut(comment, 200)))

  def recordCommentCommitActivity(userName: String, repositoryName: String, activityUserName: String, commitId: String, comment: String)
                                 (implicit s: Session): Unit =
    recordActivity(userName, repositoryName, activityUserName,
      "comment_commit",
      s"[user:${activityUserName}] commented on commit [commit:${userName}/${repositoryName}@${commitId}]",
      Some(cut(comment, 200)))

  def recordCreateWikiPageActivity(userName: String, repositoryName: String, activityUserName: String, pageName: String)
                                  (implicit s: Session): Unit =
    recordActivity(userName, repositoryName, activityUserName,
      "create_wiki",
      s"[user:${activityUserName}] created the [repo:${userName}/${repositoryName}] wiki",
      Some(pageName))

  def recordEditWikiPageActivity(userName: String, repositoryName: String, activityUserName: String, pageName: String, commitId: String)
                                (implicit s: Session): Unit =
    recordActivity(userName, repositoryName, activityUserName,
      "edit_wiki",
      s"[user:${activityUserName}] edited the [repo:${userName}/${repositoryName}] wiki",
      Some(pageName + ":" + commitId))

  def recordPushActivity(userName: String, repositoryName: String, activityUserName: String,
      branchName: String, commits: List[JGitUtil.CommitInfo])(implicit s: Session): Unit =
    recordActivity(userName, repositoryName, activityUserName,
      "push",
      s"[user:${activityUserName}] pushed to [branch:${userName}/${repositoryName}#${branchName}] at [repo:${userName}/${repositoryName}]",
      Some(commits.map { commit => commit.id + ":" + commit.shortMessage }.mkString("\n")))

  def recordCreateTagActivity(userName: String, repositoryName: String, activityUserName: String, 
      tagName: String, commits: List[JGitUtil.CommitInfo])(implicit s: Session): Unit =
    recordActivity(userName, repositoryName, activityUserName,
      "create_tag",
      s"[user:${activityUserName}] created tag [tag:${userName}/${repositoryName}#${tagName}] at [repo:${userName}/${repositoryName}]")

  def recordDeleteTagActivity(userName: String, repositoryName: String, activityUserName: String,
                              tagName: String, commits: List[JGitUtil.CommitInfo])(implicit s: Session): Unit =
    recordActivity(userName, repositoryName, activityUserName,
      "delete_tag",
      s"[user:${activityUserName}] deleted tag ${tagName} at [repo:${userName}/${repositoryName}]")

  def recordCreateBranchActivity(userName: String, repositoryName: String, activityUserName: String, branchName: String)
                                (implicit s: Session): Unit =
    recordActivity(userName, repositoryName, activityUserName,
      "create_branch",
      s"[user:${activityUserName}] created branch [branch:${userName}/${repositoryName}#${branchName}] at [repo:${userName}/${repositoryName}]")

  def recordDeleteBranchActivity(userName: String, repositoryName: String, activityUserName: String, branchName: String)
                                (implicit s: Session): Unit =
    recordActivity(userName, repositoryName, activityUserName,
      "delete_branch",
      s"[user:${activityUserName}] deleted branch ${branchName} at [repo:${userName}/${repositoryName}]")

  def recordForkActivity(userName: String, repositoryName: String, activityUserName: String, forkedUserName: String)(implicit s: Session): Unit =
    recordActivity(userName, repositoryName, activityUserName,
      "fork",
      s"[user:${activityUserName}] forked [repo:${userName}/${repositoryName}] to [repo:${forkedUserName}/${repositoryName}]")

  def recordPullRequestActivity(userName: String, repositoryName: String, activityUserName: String, issueId: Int, title: String)
                               (implicit s: Session): Unit =
    recordActivity(userName, repositoryName, activityUserName,
      "open_pullreq",
      s"[user:${activityUserName}] opened pull request [pullreq:${userName}/${repositoryName}#${issueId}]",
      Some(title))

  def recordMergeActivity(userName: String, repositoryName: String, activityUserName: String, issueId: Int, message: String)
                         (implicit s: Session): Unit =
    recordActivity(userName, repositoryName, activityUserName,
      "merge_pullreq",
      s"[user:${activityUserName}] merged pull request [pullreq:${userName}/${repositoryName}#${issueId}]",
      Some(message))

  private def cut(value: String, length: Int): String =
    if(value.length > length) value.substring(0, length) + "..." else value
}
