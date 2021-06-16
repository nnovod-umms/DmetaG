package edu.umassmed.dmetag

import scalatags.Text
import scalatags.Text.TypedTag
import scalatags.Text.all._
import scalatags.Text.tags2.title

/**
 * Pages to be rendered to HTML response for requests.
 */
object Pages {
  /**
   * Create complete HTML page
   * @param tag HTML tag data to include in page body
   * @param boot JS function to execute when page loaded
   * @return HTML that can be rendered to respond to HTTP request
   */
  private def htmlPage(tag: TypedTag[String], boot: String) = {
    val htmlTag = html(lang := "en",
      head(
        meta(charset := "utf-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1.0"),
        title("DMetaG"),
        script(`type` := "text/javascript", src := "/resource/dmetagclient.js"),
        link(rel := "stylesheet", `type` := "text/css", href := "/resource/custom.css"),
        link(rel :="stylesheet", href := "https://cdn.jsdelivr.net/npm/purecss@2.0.3/build/pure-min.css"
          // Individual modules:
          // Base	https://unpkg.com/purecss@2.0.3/build/base-min.css
          // Buttons	https://unpkg.com/purecss@2.0.3/build/buttons-min.css
          // Forms (Responsive)	https://unpkg.com/purecss@2.0.3/build/forms-min.css
          // Forms (Non-Responsive)	https://unpkg.com/purecss@2.0.3/build/forms-nr-min.css
          // Grids (Base)	https://unpkg.com/purecss@2.0.3/build/grids-min.css
          // Grids (Responsive)	https://unpkg.com/purecss@2.0.3/build/grids-responsive-min.css
          // Menus (Responsive)	https://unpkg.com/purecss@2.0.3/build/menus-min.css
          // Tables	https://unpkg.com/purecss@2.0.3/build/tables-min.css
          // jsdelivr can be used to fetch multiple ones...
          // //cdn.jsdelivr.net/combine/npm/purecss@2.0.3/build/base-min.css,npm/purecss@2.0.3/build/forms-min.css"
          // Alternate direct from the source
          // href :="https://unpkg.com/purecss@2.0.3/build/pure-min.css"
          // Integrity was failing
          // attr("integrity") := "sha384-4ZPLezkTZTsojWFhpdFembdzFudphhoOzIunR1wH6g1WQDzCAiPvDyitaK67mp0+",
          // attr("crossorigin") := "anonymous"
        ),
        link(rel :="stylesheet", href :="https://purecss.io/layouts/side-menu/styles.css"),
        script(`type` := "text/javascript", src := "https://purecss.io/js/ui.js"),
        // script(`type` := "text/javascript", src :="https://purecss.io/js/menus.js")
        link(rel :="stylesheet", href :="/resource/justcontext.css"),
        script(`type` := "text/javascript", src := "/resource/justcontext.js")
      ),
      body(
        onload := boot,
        tag
      )
    )
    // Set Doctype
    Text.all.doctype("html")(htmlTag)
  }

  // Initial page - to be reloaded by client application
  private val divTag = div(id := "appcontainer")("Not loaded!")

  // Makes boot string to call method name with parameter to get element with tag id
  private def makeBoot(tag: TypedTag[String], method: String) = {
    // Get id attribute - should be there and there should only be one.
    // Look through the lists of tag modifiers, find the attributes, and keep the one for "id"
    // Note flatmap keeps ones found (Some is a list of one) but gets rid of Nones (empty lists)
    val id =
      tag.modifiers.flatten.flatMap {
        _ match {
          case ap: AttrPair if ap.a.name == "id" =>
            Some(ap.v.toString)
          case _ =>
            None: Option[String]
        }
      }
    // If id found (it should always be) then use it, otherwise assume id is tag type
    val idVal = if (id.size == 1) id.head else tag.tag
    s"$method(document.getElementById('$idVal'))"
  }

  /**
   * Initial page for application
   */
  val dMetaGPage:doctype = htmlPage(tag = divTag, boot = makeBoot(divTag, "DMetaG.main"))
}
