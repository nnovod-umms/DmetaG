package edu.umassmed.dmetag.main

import edu.umassmed.dmetag.dmeta.{DMetaDefs, WFGraph}
import edu.umassmed.dmetag.js.JsDagre
import JsDagre.{Attrs, Dagre}
import edu.umassmed.dmetag.utils.Types.DBID

import scala.concurrent.Future
// Import execution context for futures
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

object FindDisplay {
  /**
   * Display result of find command.  Make a graph with parents and children of found entry, and then make a dagre
   * graph with layout for display.
   * @param mdDefs metadata definitions
   * @param collName collection name (projectName_collectionName)
   * @param entryDBID entry ID
   * @param entryValues entry data found
   * @return Either Left (error string) or Right((graphData, displayGraph))
   */
  private[main] def findDisplay(mdDefs: DMetaDefs, collName: String, entryDBID: DBID, entryValues: Map[String, Any])
  : Future[Either[String, (WFGraph, JsDagre.Graph)]] = {
    WFGraph.buildGraphWithPath(
      mdDefs = mdDefs, collName = collName,
      nodeDBID = entryDBID, nodeValues = entryValues
    ).map {
      case Left(err) => Left(err)
      case Right(wfGraph) =>
        Right(wfGraph, makeDisplayGraph(wfGraph))
    }
  }

  /**
   * Make a Dagre graph, with layout set, for a graph set around a found entry.
   * @param graphData workflow graph and entry data
   * @return graph with entries and layout (x/y coordinates set for nodes and edges)
   */
  private def makeDisplayGraph(graphData: WFGraph)
  : JsDagre.Graph = {
    // Make graph to display
    val graph = JsDagre.newGraph
    // set nodes into graph
    graphData.wfGraph.nodes.map(node => {
      val nodeID = node.toOuter
      val (entryCollName, entryData) = graphData.entryData(nodeID)
      // Get labels for collection and entry
      val (collLabel, entryLabel) =
        graphData.dmMetaDefs.getLabel(entryCollName, entryData) match {
          case Left(_) => ("UNKNOWN", "UNKNOWN") // Should never get here
          case Right(labels) => labels
        }
      // Get maximum label length
      val maxLabelLength = if (collLabel.length > entryLabel.length) collLabel.length else entryLabel.length
      // Set node in graph
      graph.setNode(nodeID,
        Attrs(label = collLabel, collName = entryCollName, width = maxLabelLength * 16, height = 85, entryID = entryLabel))
    })
    // Set edges into graph
    graphData.wfGraph.edges.map(edge => {
      graph.setEdge(sourceId = edge.from.toOuter, targetId = edge.to.toOuter)
    })
    // Do layout for graph and return graph with nodes/edges and layout done
    Dagre.layout(graph)
    graph
  }
}
