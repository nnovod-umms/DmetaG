package edu.umassmed.dmetag.state

import com.raquo.laminar.api.L
import com.raquo.laminar.nodes.ReactiveHtmlElement
import edu.umassmed.dmetag.dmeta.DMetaDefs
import edu.umassmed.dmetag.state.FormState.{FormState, NewCollForm}
import org.scalajs.dom
import org.scalajs.dom.html
import Command.Command
import FormState.{FormState, NewCollForm}

/**
 * State of main UI streamed to update display
 */
object UIState {

  /**
   * Base for state of UI
   */
  sealed trait UIState

  /**
   * Idle state - nothing to display
   */
  case object Idle extends UIState

  /**
   * Login complete - initialize MD etc.
   */
  case object LoginDone extends UIState

  /**
   * New form to display
   * @param form form to display
   */
  case class DisplayForm(form: L.FormElement) extends UIState

  /**
   * Form for find by ID across entry types submitted
   * @param mdDefs metadata definitions
   * @param form form submitted
   */
  case class SubmittedFormForFindID(mdDefs: DMetaDefs, form: dom.html.Form) extends UIState

  /**
   * Get initial data needed to complete requested action.
   * @param mdDefs metadata definitions
   * @param action requested action (find or new)
   */
  case class NewCommandForm(mdDefs: DMetaDefs, action: Command) extends UIState

  /**
   * Init display of entry.
   * @param action requested (new or find)
   * @param display entry initial display
   * @param displayBus subdisplay to use to display entry data
   */
  case class InitDisplayEntryData(action: Int, display: NewCollForm,
                                  displayBus: Int => (ReactiveHtmlElement[html.FieldSet], L.Observer[FormState]))
    extends UIState

  /**
   * Display entry
   * @param display workbench entry initial display
   * @param form form filled in with workbench data entry data
   * @param wbObserver observer for display to receive display updates (e.g., new form of data)
   */
  case class DisplayEntryData(display: NewCollForm, form: ReactiveHtmlElement[html.FieldSet],
                              wbObserver: L.Observer[FormState]) extends UIState

  /**
   * Process request response
   * @param request request type
   * @param json response json
   */
  case class RequestResponse(request: Command, json: String) extends UIState

  /**
   * Request error
   * @param request request type
   * @param err error message
   */
  case class RequestError(request: String, err: String) extends UIState

  case class DisplayHtml(html: L.Div) extends UIState
}
