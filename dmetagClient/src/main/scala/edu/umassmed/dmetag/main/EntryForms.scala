package edu.umassmed.dmetag.main

import com.raquo.domtypes.jsdom.defs.events.TypedTargetEvent
import com.raquo.laminar.api._
import com.raquo.laminar.modifiers.EventPropBinder
import edu.umassmed.dmetag.display.Forms
import edu.umassmed.dmetag.dmeta.DMetaDefs
import org.scalajs.dom.html.Form
import edu.umassmed.dmetag.dmeta.DMeta.FldDataEntry
import edu.umassmed.dmetag.events.{EventHandlers, Requests}
import edu.umassmed.dmetag.state.FormState.{FormForCollSubmitted, FormState}
import edu.umassmed.dmetag.utils.Types.DBID

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

object EntryForms {
  /**
   * Method to create a HTML form from a entry definition.
   * @param formSelected type of form
   * @param label form label
   * @param fields definitions of fields to include in form
   * @param isRequired have required fields be required (false if form going to be used to search for entries)
   * @param fkFields map of foreign key names to possible values found for foreign key
   * @param values values previously set for entry (fieldName -> fieldValue)
   * @return form created from input fields
   */
  private[main] def makeEntryForm(formSelected: String, label: String, fields: List[FldDataEntry], isRequired: Boolean,
                                  fkFields: Map[String, List[(DBID, Any)]], values: Map[String, Any] = Map.empty)
  : L.FormElement = {
    // Make definition into form elements
    val elements =
      Forms.makeFormElements(fields = fields, label = label,
        values = values, fkFields = fkFields, isRequired = isRequired)
    // Create form with elements for fields
    Forms.makePostForm(action = "/form", name = formSelected, fields = elements)
  }

  /**
   * Return form element to display to get entry specific information. Upon submission FormSubmitted event is sent
   * to observer.
   * @param observer observer looking for form onsubmit event
   * @param collName collection name
   * @param entryID DBID of entry (specified if working with existing entry)
   * @param form form to be used to get entry data
   * @return
   */
  private[main] def entryFormToSubmit
  (
    observer: L.Observer[FormState],
    collName: String,
    entryID: Option[DBID],
    form: L.FormElement
  ): L.FormElement = {
    /*
     * Modifier that binds the entry form's onSubmit event to do submission of form to observer.
     * @param observer observer looking for what to do next for entry form
     */
    def submitEntryForm(observer: L.Observer[FormState],
                          nextState: (Form, String) => FormState)
    : EventPropBinder[TypedTargetEvent[Form]] = {
      /*
       * Take an event (e.g., onSubmit) that happens for a form for an entry, convert data to JSON, submit data to
       * the server, and get back a response.  Interaction with server is done asynchronously so a Future to contain the
       * server's response is returned.
       * @param f form event
       * @param nextState callback to set next state using server reply
       * @return UI state to go to next
       */
      def entryToRequest(f: TypedTargetEvent[Form],
                           nextState: (Form, String) => FormState): Future[FormState] = {
        // Go submit json request
        val reply = Requests.formJsonRequest(f)
        // Convert reply to request response with json returned (target gets actual form)
        reply.map(nextState(f.target, _))
      }

      L.onSubmit.preventDefault -->
        (form => EventHandlers.toObserver(
          event = form, processEvent = entryToRequest(_, nextState), completionObserver = observer
        ))
    }

    form.amend(
      submitEntryForm(
        observer = observer,
        nextState = (form, json) => FormForCollSubmitted(collName, entryID, form, json)
      )
    )
  }

  /**
   * Return form element to display to get entry specific information. Upon submission FormSubmitted event is sent
   * to observer.
   * @param observer observer looking for form onsubmit event
   * @param nextState callback to set next state (form => state)
   * @param form form to be used to get entry data
   * @return form setup to send out next event to observer upon submission
   */
  private[main] def formToSubmit[T]
  (
    observer: L.Observer[T],
    nextState: Form => T,
    form: L.FormElement
  ): L.FormElement = {
    form.amend(
      L.onSubmit.preventDefault -->
        (form => observer.onNext(nextState(form.target)))
    )
  }

  /**
   * Get a form's definition via
   * @param mdDefs metadata definitions
   * @param fkData foreign key data (fieldName -> List(dbIDForFKEntry, foreignKeyValue))
   * @param formSelected entry type of form selected
   * @param setUIState callback to set next state of UI (typeOfForm, FldDataDefinitions, ForeignKeyValues) => UI_T
   * @param setErrState callback to set next state if an error (typeOfForm, Error) => ERR_T
   * @tparam B base type for output of callbacks
   * @tparam UI_T type for output of setUIState callback
   * @tparam ERR_T type for output of setErrState callback
   * @return next state based on server response
   */
  private[main] def getFormDefinition[B, UI_T <: B, ERR_T <: B]
  (
    mdDefs: DMetaDefs,
    fkData:  Map[String, List[(DBID, String)]],
    formSelected: String,
    setUIState: (String, List[FldDataEntry], Map[String, List[(DBID, String)]]) => UI_T,
    setErrState: (String, String) => ERR_T
  ): B = {
    // Start off by getting all possible foreign key values (excluding ones already found)
    // Get fields for collection
    mdDefs.collByName.get(formSelected)
      .flatMap(coll => mdDefs.collFields.get(coll.id)) match {
      case Some(fields) =>
        setUIState(formSelected, fields, fkData)
      case None => setErrState(formSelected, s"Unable to find fields for $formSelected")
    }
  }

  /**
   * Little fellow to map return of no form to blank Div.
   * @param formStream stream of form elements to display
   * @return stream of element (blank div if original streamed output is None)
   */
  private[main] def getFormOrBlank(formStream: L.EventStream[Option[L.FormElement]]): L.EventStream[L.HtmlElement] =
    formStream.map(_.getOrElse(L.div()))
}
