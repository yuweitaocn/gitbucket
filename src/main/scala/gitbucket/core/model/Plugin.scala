package gitbucket.core.model

trait PluginComponent extends TemplateComponent { self: Profile =>
  import profile.api._

  lazy val Plugins = TableQuery[Plugins]

  class Plugins(tag: Tag) extends Table[Plugin](tag, "PLUGIN"){
    val pluginId = column[String]("PLUGIN_ID", O PrimaryKey)
    val version = column[String]("VERSION")
    def * = (pluginId, version) <> (Plugin.tupled, Plugin.unapply)
  }
}

case class Plugin(
  pluginId: String,
  version: String
)
