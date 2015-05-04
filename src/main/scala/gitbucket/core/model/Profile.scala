package gitbucket.core.model

import slick.dbio.{NoStream, DBIOAction}
import slick.jdbc.JdbcBackend

import scala.concurrent.Await
import scala.concurrent.duration.Duration


trait Profile {
  val profile: slick.driver.JdbcProfile
  import profile.simple._

  /**
   * java.util.Date Mapped Column Types
   */
  implicit val dateColumnType = MappedColumnType.base[java.util.Date, java.sql.Timestamp](
    d => new java.sql.Timestamp(d.getTime),
    t => new java.util.Date(t.getTime)
  )

  /**
   * Extends Column to add conditional condition
   */
  implicit class RichColumn(c1: Column[Boolean]){
    def &&(c2: => Column[Boolean], guard: => Boolean): Column[Boolean] = if(guard) c1 && c2 else c1
  }

  /**
   * Returns system date.
   */
  def currentDate = new java.util.Date()

  def run[R](a: profile.api.DBIOAction[R, NoStream, Nothing])(implicit db: JdbcBackend#Database): R = {
    val f = db.run(a)
    Await.result(f, Duration.Inf)
  }

}

trait ProfileProvider { self: Profile =>
  val profile = slick.driver.H2Driver
}

trait CoreProfile extends ProfileProvider with Profile
  with AccessTokenComponent
  with AccountComponent
  with ActivityComponent
  with CollaboratorComponent
  with CommitCommentComponent
  with CommitStatusComponent
  with GroupMemberComponent
  with IssueComponent
  with IssueCommentComponent
  with IssueLabelComponent
  with LabelComponent
  with MilestoneComponent
  with PullRequestComponent
  with RepositoryComponent
  with SshKeyComponent
  with WebHookComponent
  with PluginComponent

object Profile extends CoreProfile