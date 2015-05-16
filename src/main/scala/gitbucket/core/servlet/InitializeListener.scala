package gitbucket.core.servlet

import akka.event.Logging
import com.sksamuel.elastic4s.ElasticClient
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.mappings.FieldType._
import com.sksamuel.elastic4s.source.ObjectSource
import com.typesafe.config.ConfigFactory
import gitbucket.core.model.{Repository, Activity}
import gitbucket.core.plugin.PluginRegistry
import gitbucket.core.service.{ActivityService, SystemSettingsService}
import org.apache.commons.io.FileUtils
import javax.servlet.{ServletContextListener, ServletContextEvent}
import org.elasticsearch.action.admin.indices.create.CreateIndexRequestBuilder
import org.slf4j.LoggerFactory
import gitbucket.core.util.Versions
import gitbucket.core.util.JDBCUtil._
import akka.actor.{Actor, Props, ActorSystem}
import com.typesafe.akka.extension.quartz.QuartzSchedulerExtension
import AutoUpdate._

/**
 * Initialize GitBucket system.
 * Update database schema and load plug-ins automatically in the context initializing.
 */
class InitializeListener extends ServletContextListener with SystemSettingsService {

  private val logger = LoggerFactory.getLogger(classOf[InitializeListener])

  override def contextInitialized(event: ServletContextEvent): Unit = {
    val dataDir = event.getServletContext.getInitParameter("gitbucket.home")
    if(dataDir != null){
      System.setProperty("gitbucket.home", dataDir)
    }
    org.h2.Driver.load()

    // Start Elasticsearch server
    ElasticsearchServer.start()

    Database() withTransaction { session =>
      implicit val conn = session.conn

      if(ElasticsearchServer.initialize){
        val client = ElasticsearchServer.client

        client.execute {
          create index "gitbucket" mappings (
            "repository" as (
              "userName"             typed StringType,
              "repositoryName"       typed StringType,
              "isPrivate"            typed BooleanType,
              "description"          typed StringType,
              "defaultBranch"        typed StringType,
              "registeredDate"       typed DateType,
              "updatedDate"          typed DateType,
              "lastActivityDate"     typed DateType,
              "originUserName"       typed StringType,
              "originRepositoryName" typed StringType,
              "parentUserName"       typed StringType,
              "parentRepositoryName" typed StringType
              ),
            "activity" parent "repository" as (
              "activityUserName"     typed StringType,
              "activityType"         typed StringType,
              "message"              typed StringType,
              "additionalInfo"       typed StringType,
              "activityDate"         typed DateType
              )
            )
        }.await

        // Insert repository data into Elasticsearch
        val repositoryId = conn.select("SELECT * FROM REPOSITORY"){ rs =>
          val repository = Repository(
            userName             = rs.getString("USER_NAME"),
            repositoryName       = rs.getString("REPOSITORY_NAME"),
            isPrivate            = rs.getBoolean("PRIVATE"),
            description          = Option(rs.getString("DESCRIPTION")),
            defaultBranch        = rs.getString("DEFAULT_BRANCH"),
            registeredDate       = rs.getTimestamp("REGISTERED_DATE"),
            updatedDate          = rs.getTimestamp("UPDATED_DATE"),
            lastActivityDate     = rs.getTimestamp("LAST_ACTIVITY_DATE"),
            originUserName       = Option(rs.getString("ORIGIN_USER_NAME")),
            originRepositoryName = Option(rs.getString("ORIGIN_REPOSITORY_NAME")),
            parentUserName       = Option(rs.getString("PARENT_USER_NAME")),
            parentRepositoryName = Option(rs.getString("PARENT_REPOSITORY_NAME"))
          )
          val resp = client.execute {
            // the band object will be implicitly converted into a DocumentSource
            index into "gitbucket" / "repository" doc ObjectSource(repository)
          }.await

          (repository.userName, repository.repositoryName) -> resp.getId
        }.toMap

        // Insert activity data into Elasticsearch
        conn.select("SELECT * FROM ACTIVITY"){ rs =>
          val activity = Activity(
            userName         = rs.getString("USER_NAME"),
            repositoryName   = rs.getString("REPOSITORY_NAME"),
            activityUserName = rs.getString("ACTIVITY_USER_NAME"),
            activityType     = rs.getString("ACTIVITY_TYPE"),
            message          = rs.getString("MESSAGE"),
            additionalInfo   = Option(rs.getString("ADDITIONAL_INFO")),
            activityDate     = rs.getTimestamp("ACTIVITY_DATE"),
            activityId       = rs.getInt("ACTIVITY_ID")
          )
          client.execute {
            // the band object will be implicitly converted into a DocumentSource
            val id = repositoryId(activity.userName, activity.repositoryName)
            index into "gitbucket" / "activity" doc ObjectSource(activity) parent id
          }.await
        }

        client.execute {
          flush index "gitbucket"
        }.await
      }

      // Migration
      logger.debug("Start schema update")
      Versions.update(conn, headVersion, getCurrentVersion(), versions, Thread.currentThread.getContextClassLoader){ conn =>
        FileUtils.writeStringToFile(versionFile, headVersion.versionString, "UTF-8")
      }

      // Load plugins
      logger.debug("Initialize plugins")
      PluginRegistry.initialize(event.getServletContext, loadSystemSettings(), conn)
    }

    // Start Quartz scheduler
    val system = ActorSystem("job", ConfigFactory.parseString(
      """
        |akka {
        |  quartz {
        |    schedules {
        |      Daily {
        |        expression = "0 0 0 * * ?"
        |      }
        |    }
        |  }
        |}
      """.stripMargin))

    val scheduler = QuartzSchedulerExtension(system)

    scheduler.schedule("Daily", system.actorOf(Props[DeleteOldActivityActor]), "DeleteOldActivity")
  }

  override def contextDestroyed(event: ServletContextEvent): Unit = {
    // Shutdown plugins
    PluginRegistry.shutdown(event.getServletContext, loadSystemSettings())
    // Close datasource
    Database.closeDataSource()
    // Shutdown Elasticsearch server
    ElasticsearchServer.shutdown()
  }

}

class DeleteOldActivityActor extends Actor with SystemSettingsService with ActivityService {

  private val logger = Logging(context.system, this)

  def receive = {
    case s: String => {
      loadSystemSettings().activityLogLimit.foreach { limit =>
        if(limit > 0){
          Database() withTransaction { implicit session =>
            val rows = deleteOldActivities(limit)
            logger.info(s"Deleted ${rows} activity logs")
          }
        }
      }
    }
  }
}

object ElasticsearchServer {
  import java.io.File
  import org.elasticsearch.common.settings.ImmutableSettings
  import org.elasticsearch.node.Node
  import org.elasticsearch.node.NodeBuilder._
  import gitbucket.core.util.Directory

  private var node: Node = null
  private var _client: ElasticClient = null

  var initialize = false

  def client = _client

  def start(): Unit = {
    initialize = !(new File(Directory.GitBucketHome, "elasticsearch").exists)

    val settings = ImmutableSettings.settingsBuilder
      .put("path.data", Directory.GitBucketHome)
      .put("cluster.name", "elasticsearch")
      .put("node.name", "gitbucket1")
//      .put("http.enabled", false)
//      .put("node.http.enabled", false)
      .build

    node = nodeBuilder().local(false).settings(settings).build
    node.start()

    _client = ElasticClient.fromNode(node)
  }

  def shutdown(): Unit = {
    _client.close()
    node.close()
  }

}