package gitbucket.core.servlet

import akka.event.Logging
import com.typesafe.config.ConfigFactory
import gitbucket.core.plugin.PluginRegistry
import gitbucket.core.service.{ActivityService, SystemSettingsService}
import org.apache.commons.io.FileUtils
import javax.servlet.{ServletContextListener, ServletContextEvent}
import org.slf4j.LoggerFactory
import gitbucket.core.util.Versions
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
      val conn = session.conn

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
//  import java.nio.file.Files
//  import org.apache.commons.io.FileUtils
//  import org.elasticsearch.client.Client
  import org.elasticsearch.common.settings.ImmutableSettings
  import org.elasticsearch.node.Node
  import org.elasticsearch.node.NodeBuilder._
  import gitbucket.core.util.Directory

  private var node: Node = null
  //def client: Client = node.client

  def start(): Unit = {
    val settings = ImmutableSettings.settingsBuilder
      .put("path.data", Directory.GitBucketHome)
      .put("cluster.name", "elasticsearch")
      .build
    node = nodeBuilder().local(true).settings(settings).build
    node.start()
  }

  def shutdown(): Unit = {
    node.close()
  }

}