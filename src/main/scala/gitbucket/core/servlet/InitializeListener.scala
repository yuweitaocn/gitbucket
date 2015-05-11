package gitbucket.core.servlet

import akka.event.Logging
import com.sksamuel.elastic4s.ElasticClient
import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.source.ObjectSource
import com.typesafe.config.ConfigFactory
import gitbucket.core.model.Activity
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
      val client = ElasticClient.remote("localhost", 9300)

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
          index into "gitbucket" / "activity" doc ObjectSource(activity)
        }
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
  import org.elasticsearch.client.Client
  import org.elasticsearch.common.settings.ImmutableSettings
  import org.elasticsearch.node.Node
  import org.elasticsearch.node.NodeBuilder._
  import gitbucket.core.util.Directory

  private var node: Node = null
  def client: Client = node.client

  def start(): Unit = {
    // Delete index data at first during experimental
    FileUtils.deleteDirectory(new File(Directory.GitBucketHome, "elasticsearch"))

    val settings = ImmutableSettings.settingsBuilder
      .put("path.data", Directory.GitBucketHome)
      .put("cluster.name", "elasticsearch")
      .put("node.name", "gitbucket1")
//      .put("http.enabled", false)
//      .put("node.http.enabled", false)
      .build
    node = nodeBuilder().local(false).settings(settings).build
    node.start()

    // Create index
    val request = new CreateIndexRequestBuilder(client.admin.indices)
      .setIndex("gitbucket")
      .setSource(
        """
          |{
          |  "activity" : {
          |    "mappings" : {
          |      "properties" : {
          |        "activityUserName" : {
          |          "type" : "string"
          |        },
          |        "activityType" : {
          |          "type" : "string"
          |        },
          |        "message" : {
          |          "type" : "string"
          |        },
          |        "additionalInfo" : {
          |          "type" : "string"
          |        },
          |        "activityDate" : {
          |          "type" : "date",
          |          "format" : "dateOptionalTime"
          |        }
          |      }
          |    }
          |  }
          |}
        """.stripMargin)

    val response = request.get()
    println("*************************************")
    println(response)
    println("*************************************")
  }

  def shutdown(): Unit = {
    node.close()
  }

}