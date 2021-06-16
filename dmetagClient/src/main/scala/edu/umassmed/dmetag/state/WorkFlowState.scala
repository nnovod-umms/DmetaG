package edu.umassmed.dmetag.state

import com.raquo.laminar.nodes.ReactiveSvgElement
import edu.umassmed.dmetag.dmeta.DMetaDefs
import edu.umassmed.dmetag.js.JsDagre
import edu.umassmed.dmetag.utils.Types.DBID
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.immutable.Graph

/**
 * State of workflow display
 */
object WorkFlowState {
  sealed trait WorkFlowState

  /**
   * Initialize display
   */
  case object WorkFlowDisplayInit extends WorkFlowState

  /**
   * Display html svg elements
   * @param svg svg representation
   */
  case class WorkFlowSvg(svg: ReactiveSvgElement.Base) extends WorkFlowState

  /**
   * Display workflow
   * @param entryID workflow entry database ID
   * @param collName collection name (projectName_collectionName)
   * @param graph graph set with nodes/edges and layout
   * @param mdDefs metadata definitions
   * @param highlight nodes/edges to highlight
   */
  case class WorkFlowDisplay(entryID: DBID, collName: String, graph: JsDagre.Graph, mdDefs: DMetaDefs,
                             highlight: Graph[DBID, DiEdge])
    extends WorkFlowState
}
