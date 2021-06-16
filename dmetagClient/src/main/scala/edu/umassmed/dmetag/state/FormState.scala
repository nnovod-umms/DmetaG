package edu.umassmed.dmetag.state

import com.raquo.laminar.api.L
import org.scalajs.dom
import edu.umassmed.dmetag.utils.Types.DBID

/**
 * Form states for stream of events to update form display
 */
object FormState {
  /**
   * Base for all form states
   */
  sealed trait FormState

  /**
   * Initialize form
   */
  case object Init extends FormState

  /**
   * New form to display
   * @param formType form type being displayed
   * @param form form to display
   * @param origID optional original ID for workbench entry
   */
  case class NewCollForm(collName: String, entryID: Option[DBID], form: L.FormElement) extends FormState

  /**
   * Response to submitted form.
   * @param formType workbench type form submitted for
   * @param form form submitted
   * @param origID optional original ID for workbench entry
   * @param json json returned
   */
  case class FormForCollSubmitted(collName: String, entryID: Option[DBID], form: dom.html.Form, json: String) extends FormState

  /**
   * Form error
   * @param err error message
   */
  case class FormError(err: String) extends FormState
}
