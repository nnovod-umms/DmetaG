package edu.umassmed.dmetag.main

import com.raquo.airstream.eventstream.EventStream
import edu.umassmed.dmetag.state.Command.{Command, FIND}
import com.raquo.laminar.api._
import edu.umassmed.dmetag.display.Forms
import edu.umassmed.dmetag.dmeta.DMetaDefs
import edu.umassmed.dmetag.state.FormState.{FormError, FormForCollSubmitted, FormState, NewCollForm}

trait EventDisplayBus[BT, ST] {
  protected val eventBus: L.EventBus[BT]
  protected val func: PartialFunction[BT, ST]
}
// @TODO Make into trait that just has EventBus and others inherit from that with EventStream
// Then can use for all "subdisplays" such as wfSvgDisplay etc.
case class EntryDisplayBus(action: Command, mdDefs: DMetaDefs, displayTitle: Option[String], introDiv: Option[L.Div]) {
  private val stateBus = new L.EventBus[FormState]
  private val legend = displayTitle.map(title => L.legend(title))

  /**
   * Stream to update workbench entries based on new events.
   * @param action workbench action being worked on
   * @param observer where to send commands for next steps
   * @param events input stream of events
   * @return stream of forms to display
   */
  private[main] def wbEntryAppState(action: Command, observer: L.Observer[FormState], events: L.EventStream[FormState])
  : EventStream[Option[L.FormElement]] = {
    // Set up partial function of workbench states
    val wbState: PartialFunction[FormState, Option[L.FormElement]] = {
      // Display new form and setup to receive FormForCollSubmitted later
      case NewCollForm(collName, entryID, form) =>
        MessageBus.clearMsg()
        Some(EntryForms.entryFormToSubmit(observer = observer, collName, entryID, form))

      case FormForCollSubmitted(collName, entryID, form, json) =>
        DMetaDefs.getMDDefs match {
          case Left(err) => MessageBus.displayErr(err)
          case Right(mdDefs) =>
            val entryData = Forms.formToMap(form)
            // If FIND we need to get entry and then associated workflow to display the workflow with the path to the
            // wanted entry - also display entry we found
            // @TODO Common code in LimsWorkFlow2
            if (action == FIND) {
              Display.displayWorkflowForFind(mdDefs, collName, entryData)
            } else { // Not a find - simply do what's wanted first in workbench and then in database
              MessageBus.displayWarn(s"Form submitted for $action: $collName $entryID - ${entryData}")
            }
        }
        // Blank out form display (will kill any subDisplays in form)
        DisplayBus.displayClear()
        None

      // Error processing form
      case FormError(err) =>
        MessageBus.displayErr(s"Error processing form: $err")
        None

    }

    // Act on stream of events
    events.map {
      wbState orElse {
        // Unknown - just put up nothing
        case unknown =>
          MessageBus.displayErr(s"Unknown wbEntryAppState: $unknown")
          None
      }
    }
  }
}
