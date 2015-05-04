package gitbucket.core.service

import gitbucket.core.model.Plugin
import gitbucket.core.model.Profile._
import profile.api._

trait PluginService {

  def getPlugins()(implicit db: Database): List[Plugin] = run {
    Plugins.sortBy(_.pluginId).result
  }.toList

  def registerPlugin(plugin: Plugin)(implicit db: Database): Unit = run {
    Plugins += plugin
  }

  def updatePlugin(plugin: Plugin)(implicit db: Database): Unit = run {
    Plugins.filter(_.pluginId === plugin.pluginId.bind).map(_.version).update(plugin.version)
  }

  def deletePlugin(pluginId: String)(implicit db: Database): Unit = run {
    Plugins.filter(_.pluginId === pluginId.bind).delete
  }

  def getPlugin(pluginId: String)(implicit db: Database): Option[Plugin] = run {
    Plugins.filter(_.pluginId === pluginId.bind).result.headOption
  }

}
