package gitbucket.core.service

import gitbucket.core.model._
import gitbucket.core.util.Implicits
import gitbucket.core.controller.Context
import Implicits.request2Session
import Implicits.request2database

/**
 * This service is used for a view helper mainly.
 *
 * It may be called many times in one request, so each method stores
 * its result into the cache which available during a request.
 */
trait RequestCache extends SystemSettingsService with AccountService with IssuesService {

  private implicit def context2Session(implicit context: Context): Session =
    request2Session(context.request)

  private implicit def context2Database(implicit context: Context): Database =
    request2database(context.request)

  def getIssue(userName: String, repositoryName: String, issueId: String)(implicit context: Context): Option[Issue] = {
    context.cache(s"issue.${userName}/${repositoryName}#${issueId}"){
      super.getIssue(userName, repositoryName, issueId)
    }
  }

  def getAccountByUserName(userName: String)(implicit context: Context): Option[Account] = {
    context.cache(s"account.${userName}"){
      super.getAccountByUserName(userName)
    }
  }

  def getAccountByMailAddress(mailAddress: String)(implicit context: Context): Option[Account] = {
    context.cache(s"account.${mailAddress}"){
      super.getAccountByMailAddress(mailAddress)
    }
  }
}
