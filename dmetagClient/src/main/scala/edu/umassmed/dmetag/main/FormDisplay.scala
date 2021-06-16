package edu.umassmed.dmetag.main

import com.raquo.laminar.api._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import edu.umassmed.dmetag.dmeta.{DMetaDefs, DMetaRequests}
import edu.umassmed.dmetag.state.Command
import org.scalajs.dom.html.FieldSet
import Command.UPDATE
import edu.umassmed.dmetag.state.FormState.{FormState, NewCollForm}
import edu.umassmed.dmetag.state.UIState.{InitDisplayEntryData, RequestError, UIState}
import edu.umassmed.dmetag.utils.Types.DBID

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

object FormDisplay {
  /**
   * Create entry form filling it with data found for entry, and then create state to initiate
   * form display.
   * @param mdDefs metadata definitions
   * @param entryID DBID for entry
   * @param collName collection name (projectName_collectionName)
   * @param entryData entry data to set in form
   * @return state to use for form display
   */
  private[main] def getUpdateForm(mdDefs: DMetaDefs, entryID: DBID, collName: String, entryData: Map[String, Any])
  : Future[UIState] = {
    // Get foreign key values for entry
    DMetaRequests.findFKDataForIDWithData(
      mdDefs = mdDefs, collName = collName,
      entryID = entryID, entryData = entryData
    ).flatMap { /* Get Future results */
      case Left(err) => Future.successful(RequestError(UPDATE.toString, err))
      case Right(fkData) =>
        // Get foreign key value choices for any foreign keys not already set in entry
        DMetaRequests.findFKDataForAll(mdDefs = mdDefs, collName = collName, fkDataFound = Some(fkData))
          .map { /* Future results */
            case Left(err) => RequestError(UPDATE.toString, err)
            case Right(fks) =>
              /*
             * Form fieldset used to display an entry.  It contains a section that expands, based on new events,
             * to display sub-forms to get additional information needed.
             * @param action entry action being worked on
             * @return (fields setup for to display form, observer to see events to display)
             */
              def entryDisplayApp(action: Int): (ReactiveHtmlElement[FieldSet], L.Observer[FormState]) = {
                // Make our own bus for this form to use so events for this form only go to this form
                val stateBus = new L.EventBus[FormState]
                val topForm =
                  L.fieldSet(
                    L.child <-- EntryForms.getFormOrBlank(
                      Display.entryAppState(
                        mdDefs = mdDefs, action = Command(action), observer = stateBus.writer, events = stateBus.events
                      )
                    )
                  )
                (topForm, stateBus.writer)
              }

              // Make a form with values found
              EntryForms.getFormDefinition(mdDefs = mdDefs, fkData = fks, formSelected = collName,
                setUIState = (formSelected, fields, fkData) => {
                  // Make form given field definitions, foreign key data, and entry field data
                  val f = EntryForms.makeEntryForm(formSelected = formSelected,
                    label = mdDefs.collLabels.getOrElse(formSelected, formSelected),
                    fields = fields, isRequired = true,
                    fkFields = fkData, values = entryData)
                  // Setup to display form
                  InitDisplayEntryData(
                    action = UPDATE.id,
                    display = NewCollForm(collName, Some(entryID), f),
                    displayBus = entryDisplayApp // What to call to get bus to display form
                  )
                },
                setErrState = (_, err) => {
                  RequestError(UPDATE.toString, err)
                }
              )
          }
    }
  }
}
