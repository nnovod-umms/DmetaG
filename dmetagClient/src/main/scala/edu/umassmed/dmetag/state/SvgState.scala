package edu.umassmed.dmetag.state

import com.raquo.laminar.nodes.ReactiveSvgElement

/**
 * States for stream to update svg display
 */
object SvgState {
  /**
   * Base for svg states
   */
  sealed trait SvgState

  /**
   * Init SVG display
   */
  case object SvgInit extends SvgState

  /**
   * Display svg elements
   * @param svg svg representation
   */
  case class SvgDisplay(svg: ReactiveSvgElement.Base) extends SvgState
}
