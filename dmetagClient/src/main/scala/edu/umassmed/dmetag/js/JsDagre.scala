package edu.umassmed.dmetag.js

import scala.scalajs.js
import js.annotation._

/**
 * Interface to dagre, a javascript library for doing layouts of graphs.
 * See https://github.com/dagrejs/dagre/wiki for documentation.
 */
object JsDagre {

  /**
   * Create a new directed graph.
   * @return new instance of a directed graph to be set with nodes and edges
   */
  def newGraph: Graph =
  // Create new instance of DAGRE graph (you'd think there would be an easier way but I couldn't figure one out)
  // and initialize it.
    js.Dynamic.newInstance(Dagre.graphlib.Graph.asInstanceOf[js.Dynamic])().asInstanceOf[Graph]
      .setGraph(js.Dynamic.literal(directed = true))
      .setDefaultEdgeLabel(() => blankLabel)
      .setDefaultNodeLabel(() => blankLabel)

  /**
   * Blank node/edge label
   */
  private def blankLabel = Attrs(label = "", collName = "", width = 0, height = 0, entryID = "")

  /**
   * Root of Dagre imported from NPM
   */
  @JSImport("dagre", JSImport.Namespace)
  @js.native
  object Dagre extends js.Object {
    // graphlib sublibrary
    val graphlib: GraphLib = js.native

    /**
     * Do the layout for the graph.  Nodes and edges must have been previously set (setNode and setEdge) in the graph.
     * This method sets the x/y node and edge coordinates in the graph. To retrieve the coordinates the Graph methods
     * nodes and edges must be called to get the array of nodes/edges and then the methods node and edge are called to
     * retrieve the information, including the coordinates, for individual nodes/edges.
     * @param graph graph setup with node/edges
     */
    def layout(graph: Graph): Unit = js.native
  }

  /**
   * GraphLib interface within Dagre - just needed to create graphs
   */
  @js.native
  private[JsDagre] trait GraphLib extends js.Object {
    def Graph: Graph = js.native
  }

  /**
   * Graph interface.
   */
  @js.native
  trait Graph extends js.Object {
    /**
     * Get nodes in graph
     * @return graph nodes
     */
    def nodes(): js.Array[String] = js.native

    /**
     * Get information for an individual graph node
     * @param id node id
     * @return node data, including layout location
     */
    def node(id: String): Node = js.native

    /**
     * Get edges in graph
     * @return graph edges
     */
    def edges(): js.Array[EdgeID] = js.native

    /**
     * Get information for an individual edge
     * @param id edge id (source/target)
     * @return edge data, including layout points
     */
    def edge(id: EdgeID): Edge = js.native

    /**
     * Set default node label
     * @param callback method to set default node label
     * @return updated graph
     */
    def setDefaultNodeLabel(callback: js.Function0[js.Object]): Graph = js.native

    /**
     * Set default edge label
     * @param callback method to set default edge label
     * @return updated graph
     */
    def setDefaultEdgeLabel(callback: js.Function0[js.Object]): Graph = js.native

    /**
     * Set node in graph.  The label, width and height are set via attrs.
     * @param id node id
     * @param label label attributes
     * @return updated graph
     */
    def setNode(id: String, label: Attrs): Graph = js.native

    /**
     * Set edge in graph. If the edge has a label it is set via attrs along with the label width and height.
     * @param sourceId id of edge source node
     * @param targetId id of edge target node
     * @param label optional label attributes
     * @return updated graph
     */
    def setEdge(sourceId: String, targetId: String,
                label: Attrs = JsDagre.blankLabel): Graph = js.native

    /**
     * Set options in graph
     * @param options graph options
     * @return updated graph
     */
    def setGraph(options: js.Object): Graph = js.native

    /**
     * Label for graph that includes attributes such as width and height
     */
    val _label: GraphDimensions
  }

  /**
   * Graph dimensions: height and width for entire graph.  However, the width/height seemed
   * to be based on just the nodes, ignoring edges, which is inaccurate if the edges are laid out beyond
   * the perimeter of the outside nodes.
   */
  @js.native
  trait GraphDimensions extends js.Object {
    val height: Float = js.native
    val width: Float = js.native
  }

  /**
   * Edge identifier - source and target IDs
   */
  @js.native
  trait EdgeID extends js.Object {
    val v: String = js.native
    val w: String = js.native
  }

  /**
   * Label and name for node or edge along with height/width.
   * For a node width/height is the node's height/width, for an edge it's the label's height/width.
   */
  @js.native
  trait Attrs extends js.Object {
    val label: String = js.native
    val collName: String = js.native
    val width: Float = js.native
    val height: Float = js.native
    val entryID: String = js.native
  }

  /**
   * Object to allow typesafe creation of Attrs
   * Attrs(label, name, width, height, entryID)
   */
  object Attrs {
    def apply(label: String, collName: String, width: Int, height: Int, entryID: String): Attrs =
      js.Dynamic.literal(label = label, collName = collName, width = width, height = height, entryID = entryID).asInstanceOf[Attrs]
  }

  /**
   * Coordinates for a position.
   */
  @js.native
  trait Coords extends js.Object {
    val x: Float = js.native
    val y: Float = js.native
  }

  /**
   * Edge layout information. Note that label and associated attributes (width, height, x and y) are set if and only
   * if a label has been set for the edge. The x/y coordinates for a label as set as the center position of the label.
   */
  @js.native
  trait Edge extends js.Object with Attrs with Coords {
    // layout points for edge
    val points: js.Array[Coords] = js.native
  }

  /**
   * Node data
   * The x and y coordinates are for the middle of the node, unlike coordinates for svg which are for the upper
   * left hand corner, so svg x,y = node.x - node.width/2, node.y - node.height/2
   */
  @js.native
  trait Node extends js.Object with Attrs with Coords
}
