package edu.umassmed.dmetag.display

import com.raquo.laminar.api.L
import com.raquo.laminar.nodes.ReactiveElement
import org.scalajs.dom

/**
 * Methods to initially render laminar elements
 */
object Render {
  /**
   * Render the laminar element in a page, assumed to already be loaded.
   * @param elementID id of dom element to be parent of laminar element
   * @param rootNode  root laminar element to load
   */
  def render(elementID: String, rootNode: ReactiveElement.Base): Unit = {
    val appContainer = dom.document.getElementById(elementID)
    L.render(appContainer, rootNode)
  }

  /**
   * Render the laminar element in a page when the page is loaded.
   * @param selector id of dom element to be parent of laminar element
   * @param rootNode root laminar element to load
   */
  def renderOnLoad(selector: String, rootNode: ReactiveElement.Base): Unit = {
    // Whenever a DOM document is loaded render the application defined in the root node
    L.documentEvents.onDomContentLoaded.foreach { _ =>
      val appContainer = dom.document.getElementById(selector) // .querySelector(s"#$selector")
      L.render(appContainer, rootNode)
    }(L.unsafeWindowOwner)
  }
}
