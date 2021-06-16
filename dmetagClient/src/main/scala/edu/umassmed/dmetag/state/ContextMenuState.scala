package edu.umassmed.dmetag.state

import com.raquo.laminar.api._

/**
 * States of context menu for entry in workflow graph.
 */
object ContextMenuState {
  sealed trait ContextMenuState

  /**
   * Init context menu
   */
  case object InitMenu extends ContextMenuState

  /**
   * Display context menu
   * @param menu div in which to display menu
   */
  case class DisplayMenu(menu: L.Div) extends ContextMenuState
}
