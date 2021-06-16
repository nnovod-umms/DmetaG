package edu.umassmed.dmetag.display

import com.raquo.laminar.api._
import com.raquo.laminar.nodes.ReactiveSvgElement
import edu.umassmed.dmetag.js.JsDagre
import org.scalajs.dom
import org.scalajs.dom.svg.{G, Polyline, RectElement, SVG, Text}
import edu.umassmed.dmetag.utils.Types.DBID
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.immutable.Graph

import java.lang.{Float => JFloat}

/**
 * Methods to make SVG displays
 */
object SvgElements {
  /**
   * Make graph display for workflow. The layout for the graph has been determined by dagre.
   * @param graph graph of elements, including node and edge layout
   * @param isBlankIfEmpty make empty box if nothing to display?
   * @param nodeOnClick on click callback for node (nodeID, collectionName, title, subtitle) => T
   * @param contextMenu on right click callback for node (nodeID, collectionName, x-position, y-position) => T1
   * @param nodeOnDblClick method to call if one of entries is double-clicked (nodeID, collectionName, title, subtitle) => T2
   * @param highlight list of nodes and edges to highlight (color crimson instead of black)
   * @param title workflow title
   * @return svg elements containing nodes and connecting edges
   */
  def workflow[T, T1, T2](graph: JsDagre.Graph, isBlankIfEmpty: Boolean,
                          nodeOnClick: (String, String, String, String) => T,
                          contextMenu: (String, String, Double, Double) => T1,
                          nodeOnDblClick: Option[(String, String, String, String) => T2],
                          highlight: Graph[DBID, DiEdge], title: String)
  : ReactiveSvgElement[SVG] = {
    // Get nodes and edges to be highlighted
    val highlightNodes = highlight.nodes.map(_.toOuter).toSet
    val highlightEdges = highlight.edges.map(_.toOuter).toSet
    // Get hightlight color
    def getNodeColor(node: DBID) = getColor(node, highlightNodes)
    def getEdgeColor(edge: DiEdge[DBID]) = getColor(edge, highlightEdges)
    def getColor[CT](entry: CT, highlights: Set[CT]) = if (highlights.contains(entry)) "hotpink" else "black"

    // Some constants (make settings someday)
    // Padding around display, font size, minimum height for display graph
    val padding = 40
    val fontSize = 25
    val minHeight = 125

    // Get nodes
    val gNodes = graph.nodes()
    // If nothing and that's what we want return an empty display
    if (gNodes.isEmpty && isBlankIfEmpty)
      L.svg.svg()
    else {
      // Convert nodes into svg rectangle elements
      val nodeSvgs =
        gNodes.map(nodeID => {
          val node = graph.node(nodeID)
          val collName = node.collName
          // Make rectangle, remembering that the node's x/y in the graph is the middle of the rectangle
          makeInnerRect(title = node.label, subtitle = node.entryID,
            height = node.height.toInt, width = node.width.toInt, maxText = None,
            x = node.x - (node.width/2), y = node.y - (node.height/2),
            stroke = getNodeColor(nodeID),
            onClick = nodeOnClick(nodeID, collName, _, _),
            onDblClick = nodeOnDblClick.map(f => f(nodeID, collName, _, _)),
            onContextMenu = Some(contextMenu(nodeID, collName, _, _))
          )
        })
      // Convert edges into svg polylines with points set in graph layout. Need complex foldLefts vs. simple maps
      // because folds are needed to get max width/height from points since dagre appears to not include points going
      // around boxes in its graph width/height, and we don't want to loop through the edges multiple times.
      val ((graphWidth, graphHeight), edgeSvgs) = {
        graph.edges().foldLeft(((graph._label.width, graph._label.height), List.empty[ReactiveSvgElement[Polyline]])) {
          case ((edgesMax, polyList), next) =>
            val edge = graph.edge(next)
            // Go through edge's points to check for new max width/height and make "x,y" point position strings
            val (polyMax, points) =
              edge.points.foldLeft((edgesMax, List.empty[String])) {
                case (((maxX, maxY), polyPoints), next) =>
                  // Reset max width/height seen and add to list of points in polyline
                  ((JFloat.max(maxX, next.x), JFloat.max(maxY, next.y)), s"${next.x},${next.y}" :: polyPoints)
              }
            // Return new max width/height and add new polyline created from points to list
            (polyMax,
              L.svg.polyline(
                L.svg.fill("none"),
                L.svg.stroke(getEdgeColor(DiEdge(next.v, next.w))),
                // Make points into string x0,y0 x1,y1 ... xx,yy
                L.svg.points(points.mkString(" "))
              ) :: polyList
            )
        }
      }
      // Now make the big box
      val (bigBox, bigText, _) = makeRectWithText(title = title, subtitle = None,
        height = graphHeight.toInt+padding, width = graphWidth.toInt+padding,
        x = -padding/2, y = -padding/2, opacity = 0f, stroke = "Silver",
        isTitleOnTop = true, fontSize = fontSize, None)
      // Width/height on svg is absolute and viewbox gives coordinates to use for what's in svg. We use negative
      // low points for the viewbox for a bit of padding since dagre coordinates used from graph layout start at 0,0
      val svgHeight = if (graphHeight < minHeight) minHeight else graphHeight
      L.svg.svg(
        // Let it be as wide as parent section with height adjusted based on # of rows
        L.svg.height(s"$svgHeight"),
        L.svg.viewBox(s"${-padding} ${-padding}, ${graphWidth + padding*2}, ${graphHeight + padding*2}"),
        L.svg.preserveAspectRatio("xMinYMin meet"), // Align mins of viewbox with smallest x/y of viewport
        L.svg.style("overflow-x:scroll; overflow-y:scroll;"),
        L.svg.g(
          bigBox, bigText
        ),
        L.svg.g(
          nodeSvgs,
          edgeSvgs
        ),
      )
    }
  }

  /**
   * Create a svg rectangle with a title and subtitle placed in the middle.
   * @param title rectangle mid-placed title
   * @param subtitle rectangle mid-placed subtitle
   * @param height rectangle height
   * @param width rectangle width
   * @param maxText max length text to display
   * @param x x-position of rectangle
   * @param y y-position of rectangle
   * @param onClick method to call if one of workbench entries is clicked (title, subtitle) => T
   * @param onContextMenu method to call to display a context menu (x-pos, y-pos) => T1
   * @param onDblClick method to call if one of entries is double-clicked (title, subtitle) => T2
   * @return svg G element that includes rectangle and centered text title
   */
  private def makeInnerRect[T, T1, T2](title: String, subtitle: String, height: Int, width: Int, maxText: Option[Int],
                                       x: Float, y: Float, stroke: String,
                                       onClick: (String, String) => T,
                                       onContextMenu:Option[(Double, Double) => T1],
                                       onDblClick:Option[(String, String) => T2])
  : ReactiveSvgElement[G] = {
    val (rect, text, text1) =
      makeRectWithText(title = title, subtitle = Some(subtitle),
        height = height, width = width, x = x, y = y,
        opacity = 0.1f, stroke = stroke, isTitleOnTop = false, fontSize = height / 4, maxTextLen = maxText
      )
    // Setup initial graphic area with rectangle and title
    val initG =
      L.svg.g(
        rect,
        text
      )
    // Add subtitle if it's there
    val withSubtitle =
      if (text1.isDefined)
        initG.amend(text1.get)
      else
        initG
    // Can't use L.onDblClick and L.onClick together (onClick goes off for first click of onDblClick) so we need
    // this variable to contain timer to see if single click is not double click
    var timer: Option[Int] = None
    val withClick = withSubtitle.amend(
      L.onClick --> (e => {
        onDblClick match {
          // No double click callback so just treat all clicks as single clicks
          case None => onClick(title, subtitle)
          case Some(dblClick) =>
            // If double click (detail == 2) then process it and shut down timer that was used to check for single click
            if (e.detail == 2) {
              dblClick(title, subtitle)
              timer.map(dom.window.clearTimeout)
              timer = None
            } else if (e.detail == 1) {
              // Single click (so far):
              // If timer is active (normally shouldn't be - could only happen if a second click happens
              // after the system 2-click time but before the timeout for the timer we have) then we just
              // treat the first click as a single click and ignore the second.
              // Otherwise setup timer now to treat click as a single click if there are no additional clicks
              // before timeout period.
              timer match {
                case Some(t) =>
                  dom.window.clearTimeout(t)
                  timer = None
                  onClick(title, subtitle)
                case None =>
                  // Setup timer to do onclick if second click doesn't come soon enough
                  timer = Some(dom.window.setTimeout(() => {timer = None; onClick(title, subtitle)}, 400))
              }
            }
        }
      }))
    onContextMenu match {
      case Some(func) => withClick.amend(
        L.onContextMenu.preventDefault --> (e => {
          val docElement = dom.document.documentElement
          val docBody = dom.document.body
          // Make sure position isn't scrolled off of page and leave room for menu - we should really be using the menu
          // width and height instead of the rectangle's but the menu hasn't been created yet
          val maxLeft = List(dom.window.innerWidth.toInt, docElement.clientWidth, docBody.clientWidth)
            .find(_ != 0).getOrElse(0)-width-20
          val maxTop = List(dom.window.innerHeight.toInt, docElement.clientHeight, docBody.clientHeight)
            .find(_ != 0).getOrElse(0)-height-20
          func(
            if (e.clientX > maxLeft) maxLeft.toDouble else e.clientX,
            if (e.clientY > maxTop) maxTop.toDouble else e.clientY
          )
        }))
      case None => withClick
    }
  }

  /**
   * Create a svg rectangle, with a title and optionally a subtitle.
   * @param title        rectangle title
   * @param subtitle     optional subtitle (ignored if title on top is true)
   * @param height       rectangle height
   * @param width        rectangle width
   * @param x            x-position of rectangle
   * @param y            y-position of rectangle
   * @param opacity      inside opacity - 0.0 (totally clear) to 1.0 (black)
   * @param stroke       rectangle outside line color
   * @param isTitleOnTop true if title on top of box, otherwise inside
   * @param fontSize     text font size
   * @param maxTextLen   max length text to display
   * @return (svg rectangle, svg text with title, optional svg text with subtitle)
   */
  private def makeRectWithText(title: String, subtitle: Option[String], height: Int, width: Int, x: Float, y: Float,
                               opacity: Float, stroke: String, isTitleOnTop: Boolean, fontSize: Int,
                               maxTextLen: Option[Int])
  : (ReactiveSvgElement[RectElement], ReactiveSvgElement[Text], Option[ReactiveSvgElement[Text]]) = {

    // Helper method to make a svg title
    def getTitle(text: String, textX: Float, textY: Float) = {
      L.svg.text(
        L.svg.style := "cursor: pointer;",
        L.svg.textAnchor("middle"),
        L.svg.x(textX.toString),
        L.svg.y(textY.toString),
        L.svg.fontFamily("Times,serif"),
        L.svg.fontSize(fontSize.toString),
        L.svg.fill("#000000"),
        maxText(text, maxTextLen)
      )
    }

    // Helper method to make sure a string isn't over a given length - if it is we get replace it with
    // the first x characters, followed by "...", and ending with the last 2 characters of the input
    def maxText(text: String, maxLen: Option[Int]) = {
      maxLen match {
        case Some(len) =>
          if (text.length <= len)
            text
          else
            text.substring(0, len-5) + "..." + text.substring(text.length - 2, text.length)
        case _ => text
      }
    }

    // Decide if there's a subtitle
    val hasSubtitle = subtitle.isDefined && subtitle.get.nonEmpty && !isTitleOnTop
    // Make sure 1st character of title is upper case
    val capTitle = if (title.length < 1) title else title.substring(0, 1).toUpperCase + title.substring(1)
    // Get title line - in middle of box horizontally, height depends of whether it's on top or if there's a subtitle
    val firstTitleLine =
      if (isTitleOnTop)
        getTitle(capTitle, textX = x + width/2, textY = y - 2)
      else if (hasSubtitle)
        getTitle(capTitle, textX = x + width/2, textY = y + height*5/16)
      else
        getTitle(capTitle, textX = x + width/2, textY = y + height/2)
    // Get subtitle if it exists - in middle of box horizontally and below title
    val secondTitleLine =
      if (!hasSubtitle)
        None
      else
        Some(getTitle(subtitle.get, textX = x + width/2, textY = y + height*11/16))
    // Return (rectangle, title, optional subtitle)
    (
      L.svg.rect(
        L.svg.x := x.toString,
        L.svg.y := y.toString,
        L.svg.width := width.toString,
        L.svg.height := height.toString,
        L.svg.strokeWidth := "2",
        L.svg.fillOpacity := opacity.toString,
        L.svg.stroke := stroke
      ),
      firstTitleLine,
      secondTitleLine
    )
  }
}
