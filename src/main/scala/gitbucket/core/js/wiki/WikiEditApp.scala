package gitbucket.core.js.wiki

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.jquery.jQuery
import org.scalajs.dom

object WikiEditApp extends JSApp {

  def main(): Unit = {
    jQuery(setupUI _)
  }

  def setupUI(): Unit = {
    jQuery("#delete").click { () =>
      dom.confirm("Are you sure you want to delete this page?")
    }
  }
}
