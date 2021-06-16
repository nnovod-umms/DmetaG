package edu.umassmed.dmetag.display

import com.raquo.laminar.api._
import org.scalajs.dom.html

import scala.scalajs.js.Object

/**
 * Method to make a html table.
 */
object HtmlTable {
  /**
   * Make a html table.
   * @param title heading to put above list
   * @param headings headings to put in first row above columns
   * @param entries table entries - List of (idForRow, entries)
   * @param striped make table rows striped?
   * @param cursorPointer make cursor over rows a pointer?
   * @param onClick called when row is clicked - called with (rowID, map of columnHeadingName (or index) to columnValue)
   * @return HTML div containing table
   */
  def makeEntryTable[T](title: Option[String], headings: Option[List[String]],
                        entries: List[(String, List[String])], striped: Boolean, cursorPointer: Boolean,
                        onClick: Option[(String, Map[String, String]) => T])
  : L.Div = {
    // Setup initial div with heading if wanted
    val titleDiv =
      title match {
        case Some(titleStr) => L.div(L.h3(titleStr))
        case None => L.div()
      }
    // Init table
    val table =  L.table(L.cls := "pure-table")
    // Setup heading row if wanted
    if (headings.isDefined)
      table.amend(
        L.thead(
          L.tr(
            headings.get.map(hStr => L.th(hStr)),
          )
        )
      )
    // Setup table entries
    val entryRows = {
      var odd: Boolean = false
      entries.map{
        case (rowID, rowContents) =>
          odd = !odd
          val tr = L.tr(L.idAttr(rowID))
          if (cursorPointer) tr.amend(L.styleAttr := "cursor: pointer;")
          // Striped (odd numbered rows having different background via "pure-table-odd") if requested
          if (striped && odd)
            tr.amend(L.cls := "pure-table-odd")
          // Create onClick callback for row if requested
          if (onClick.isDefined)
            tr.amend(L.onClick --> (e => {
              val tableRow = e.target.parentNode.asInstanceOf[html.TableRow]
              // Get all the cells from the row clicked on
              val rowEntries = {
                Object.entries(tableRow.childNodes).map(
                  v => {
                    // First part of tuple should be cell index #
                    val entryIndex =
                      try {
                        v._1.toInt
                      } catch {
                        case _: Throwable => -1
                      }
                    // Set key from heading or just as index
                    val key =
                      if (entryIndex >= 0 && headings.isDefined && headings.get.length > entryIndex)
                        headings.get(entryIndex)
                      else
                        v._1
                    // Return key -> cellContents
                    (key, v._2.asInstanceOf[html.TableCell].textContent)
                  }
                )
              }
              // Call onClick method provided with map of row contents
              onClick.get(tableRow.id, rowEntries.toMap)
            }))
          // Amend row to contain columns
          tr.amend(rowContents.map(
            detail => L.td(detail)
          ))
      }
    }
    // Add table with entry rows to returned div
    titleDiv.amend(table.amend(entryRows))
  }
}
