package edu.umassmed.dmetag.main

import com.raquo.domtypes.jsdom.defs.events.{TypedTargetEvent, TypedTargetMouseEvent}
import com.raquo.laminar.api._
import com.raquo.laminar.modifiers.EventPropBinder
import com.raquo.laminar.nodes.ReactiveHtmlElement
import edu.umassmed.dmetag.display.{Forms, Render, SvgElements}
import edu.umassmed.dmetag.dmeta.{DMetaDefs, URLs}
import edu.umassmed.dmetag.events.{EventHandlers, Requests}
import edu.umassmed.dmetag.state.Command
import org.scalajs.dom
import org.scalajs.dom.Element
import org.scalajs.dom.html.{FieldSet, Form}
import edu.umassmed.dmetag.state.Command._
import edu.umassmed.dmetag.state.UIState._
import edu.umassmed.dmetag.state.WorkFlowState._
import edu.umassmed.dmetag.utils.Types.DBID

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("DMetaG")
object Main {
  /**
   * Start of client application that's called on load of main page with a skeleton div to be populated with the
   * single page application.
   * @param container dom container to contain application
   */
  @JSExport
  def main(container: dom.html.Div): Unit = {
    // Get rid of any preliminary contents
    container.textContent = ""
    // Register to send any unhandled Laminar errors to the message bus
    registerUnhandledErrorCallback(MessageBus.displayErr)
    // Go render the application div into the container div and initialize
    Render.render(elementID = container.id, rootNode = app)
    // Set form display state to idle
    DisplayBus.displayClear()
    // With no message
    MessageBus.clearMsg()
    // Init workflow display
    GraphDisplayBus.wfSvgClear()
    // Init workflow context menu display
    GraphDisplay.initContextMenu()
  }

  /**
   * The heart of it all: a single page application. The contents is set based on the output of Laminar buses.
   * A Laminar bus has an "Observer" as input (something new events can sent to) and an "Observable" as output
   * (a stream of new events).
   * The contents of the dynamic divisions are updated as new events (mappings of bus Observables into html elements)
   * get streamed into the "children".
   * Besides the side menu there are the following divisions:
   *  1. A simple display to output messages.
   *  2. The workflow area. This is a svg region updated to display a workflow.
   *  3. A place for forms to be displayed.  Each command displays the necessary forms for execution.
   *     Some of the forms have subdivisions that are dynamically updated in their own sub-apps.
   *  Note that L.Div vs. L.div is a reactive element type (capital D) vs. a html tag (little d) - this is true for
   *  other L (Laminar) types/tags as well.
   */
  private lazy val app: L.Div =
    L.div(
      L.idAttr := "layout",
      // Menu toggle
      L.a(L.href :="#menu", L.idAttr :="menuLink", L.cls := "menu-link",
        // Hamburger icon
        L.span()
      ),
      L.div(L.idAttr := "menu",
        sideMenu,
      ),
      L.div(L.idAttr := "messages", L.cls := "pure-controls",
        L.children <-- MessageBus.msgDisplay
      ),
      L.br(),
      L.div(L.idAttr := "workflow", L.cls := "pure-controls", L.styleAttr := "overflow:auto;",
        L.children <-- GraphDisplay.wfSvgDisplay,
        L.child <-- GraphDisplay.cmDisplay // Context menu display for right clicked item
      ),
      L.br(),
      L.div(L.idAttr := "forms", L.cls := "pure-controls",
        L.children <-- Display.formDisplay
      )
    )

  // The menu, displayed on the left side, using purecss styling.
  // When a menu command is chosen it kicks off events to start execution of the command.
  private lazy val sideMenu: L.Div =
    L.div(L.cls := "pure-menu",
      // text-tranform to stop pure-menu-heading from capitalizing heading
      L.a(L.cls :="pure-menu-heading", L.href := "#", L.styleAttr("text-transform:none"), "DMetaG"),
      L.ul(L.cls := "pure-menu-list",
        makeMenuItem(command = LOGIN, label = "Login",
          selectInput = URLs.DMetaLoginURL,
          uponSelect = getFormOnClick(_, Forms.loginFieldset)),
        makeMenuItem(command = FIND, label = "Find",
          selectInput = FIND,
          uponSelect = getCommandFormOnClick),
        makeMenuItem(command = FIND_BARCODE, label = "Find Barcode",
          selectInput = FIND_BARCODE,
          uponSelect = getEntryFormOnClick(_,
            (_, mdDefs) =>
              // Command to look for ID field across collections
              // Display form to get ID field, going to SubmittedFormForFindID state upon submission
              DisplayForm(
                EntryForms.formToSubmit(
                  observer = DisplayBus.displayWriter,
                  nextState = form => SubmittedFormForFindID(mdDefs, form),
                  form = Forms.makeSubmitForm(DMetaDefs.COMMON_FIELD, Forms.idFieldset(DMetaDefs.COMMON_FIELD))
                )
              )
          )
        ),
        makeMenuItem(command = NEW, label = "New",
          selectInput = NEW,
          uponSelect = getCommandFormOnClick),
        // @TODO Put in addition command: "Reset" to clear out all displays, init buses, and reset metadata?
        // @TODO Put in "Settings" command to set DMeta URL (does reset as well), barcode field name
        makeMenuItem(command = ABOUT, label = "About",
          selectInput = ABOUT,
          uponSelect = displayRelGraph
        )
      )
    )

  /**
   * Make an menu item and set it up to go to a form when chosen
   * @param command state to display
   * @param label name for menu item
   * @param selectInput input to uponSelect callback
   * @param uponSelect callback when menu item selected to get on action to take
   * @return menu list entry
   */
  private def makeMenuItem[T](command: Command, label: String, selectInput: T,
                              uponSelect: T => EventPropBinder[TypedTargetMouseEvent[dom.Element]])
  : L.Li =
    L.li(L.cls := "pure-menu-item",
      L.a(L.idAttr := command.toString, L.cls := "pure-menu-link", label, uponSelect(selectInput)))
  // Including L.href := "#forms" ever useful?

  /**
   * Modifier that binds onClick event to send out a state change to display a form - used in menu to bring up
   * top level forms.  Form is first displayed via FormDisplay event to displayBus and then when the form is
   * submitted the request goes to the url and the response comes back as a RequestResponse to the displayBus.
   * @param action url to submit form to
   * @param fields form fields
   * @return onClick event bound to form
   */
  private def getFormOnClick(action: String, fields: => ReactiveHtmlElement[FieldSet])
  : EventPropBinder[TypedTargetMouseEvent[Element]] =
    L.onClick --> (aRef => {
      DisplayBus.displayBusOnNext(
        DisplayForm(
          Forms.makePostForm(
            action = action, name = aRef.target.id, fields = fields
          ).amend(submitCommandForm)
        )
      )
    })

  /**
   * Modifier that binds a form's onSubmit event to do submission of form to server.
   * A brief explanation of how Laminar event binders work: The method set as the action to take when a form is
   * submitted is magically made into a Laminar Observer with the onNext method set as the (form => ...) method below,
   * all meaning that when the onSubmit event occurs the Observer sees it to report the event.
   * This can be a val (as opposed to a def) because there's nothing here variable or that creates any
   * Laminar/dom elements.
   */
  private lazy val submitCommandForm: EventPropBinder[TypedTargetEvent[Form]] = {
    /*
     * Take an event (e.g., onSubmit) that happens for a form, convert data to JSON, submit data to the server, and
     * get back a response.  Interaction with server is done asynchronously so a Future to contain the server's
     * response is returned.
     *
     * @param f form event
     * @return server response
     */
    def commandToRequest(f: TypedTargetEvent[Form]): Future[RequestResponse] = {
      // Go submit json request
      val reply = Requests.formJsonRequest(f)
      // Get command (id should have been set from Command enum)
      val command = Command.withName(f.target.id)
      // Convert reply to request response with json returned
      reply.map(RequestResponse(command, _))
    }

    L.onSubmit.preventDefault -->
      (form => EventHandlers.toObserver(
        event = form, processEvent = commandToRequest, completionObserver = DisplayBus.displayWriter
      ))
  }

  /**
   * Modifier that binds onClick event to send out a NewCommandForm state change to display an initial form to work on
   * a command for entries.
   * @param command command being executed
   * @return onClick event to initialize display of foom
   */
  private def getCommandFormOnClick(command: Command): EventPropBinder[TypedTargetMouseEvent[Element]] =
    getEntryFormOnClick(command, (_, mdDefs) => NewCommandForm(mdDefs, command))

  /**
   * Modifier that binds onClick event to send out a state change to the display.  Use this method to display initial
   * forms for work on entries.
   * @param command command being executed
   * @param event callback to create new state for display
   * @return onClick event to initialize display
   */
  private def getEntryFormOnClick(command: Command, event: (Command, DMetaDefs) => UIState)
  : EventPropBinder[TypedTargetMouseEvent[Element]] = {
    L.onClick --> (_ => {
      GraphDisplayBus.wfSvgClear()
      MessageBus.clearMsg()
      // Make sure we're logged in
      Display.pleaseLogin.map(mdDefs => {
        // If logged in then display form
        DisplayBus.displayBusOnNext(event(command, mdDefs))
      }).getOrElse(DisplayBus.displayClear()) // Just clear out display if not logged in
    })
  }

  /**
   * Modifier that binds onClick event to display of the relationship graph in the svg area of the display.
   * @param command command
   * @return onClick event bound to display relationship graph
   */
  private def displayRelGraph(command: Command): EventPropBinder[TypedTargetMouseEvent[Element]] =
    L.onClick --> (_ => {
      // Set form display state to idle
      DisplayBus.displayClear()
      // With no message
      MessageBus.clearMsg()
      // Display graph of relationships between collections
      Display.pleaseLogin.map(mdDefs => {
        val graph = mdDefs.makeRelDisplayGraph()
        // Put display on svg bus
        GraphDisplayBus.wfSvgOnNext(
          // Create display (someday show more about relationship when it's clicked on?)
          WorkFlowSvg(
            SvgElements.workflow(
              graph = graph,
              isBlankIfEmpty =  false,
              nodeOnClick = (_, _, _, _) => (),
              nodeOnDblClick = None,
              contextMenu = (_, _, _, _) => (),
              highlight = scalax.collection.immutable.Graph[DBID, scalax.collection.GraphEdge.DiEdge](), // No focus
              title = "Entry Type Relationships"
            )
          )
        )
      })
    })

  /**
   * Register error handler for errors that percolate up and through user observables/observers.
   * Specifically, this handler will be called for errors not handled by any onError Observer methods.
   *
   * The handler registered here outputs the error to the specified display (most likely via the message bus,
   * which is hopefully still working).  Airstream has it's own error handler registered as well
   * (AirstreamErrors.consoleErrorCallback) that reports the error, along with a stacktrace, to the browser console.
   * That handler can be unregistered via Airstream.unregisterUnhandledErrorCallback but that's not advisable because
   * the stack trace will be lost.
   *
   * @param errDisplay callback to display error message
   */
  private def registerUnhandledErrorCallback(errDisplay: String => Unit): Unit =
    L.AirstreamError.registerUnhandledErrorCallback(
      t => errDisplay(s"${t.getLocalizedMessage} (see browser console for more detail)")
    )
}
