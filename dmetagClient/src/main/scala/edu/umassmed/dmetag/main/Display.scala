package edu.umassmed.dmetag.main

import com.raquo.airstream.eventstream.EventStream
import com.raquo.laminar.api._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import edu.umassmed.dmetag.dmeta.DMeta.{Login, Status}
import edu.umassmed.dmetag.dmeta.{DMeta, DMetaDefs, DMetaDefsVar, DMetaRequests, LoginInfo, URLs}
import edu.umassmed.dmetag.state.Command
import edu.umassmed.dmetag.state.FormState.FormState
import org.scalajs.dom.experimental.HttpMethod
import org.scalajs.dom.html.FieldSet
import DMeta.{Login, Status}
import edu.umassmed.dmetag.dmeta.LoginInfo
import Command.{Command, FIND, LOGIN, NEW, UPDATE}
import edu.umassmed.dmetag.display.{Forms, HtmlTable}
import edu.umassmed.dmetag.events.{EventHandlers, Requests}
import edu.umassmed.dmetag.state.FormState._
import edu.umassmed.dmetag.state.UIState._
import edu.umassmed.dmetag.state.WorkFlowState.WorkFlowDisplay
import edu.umassmed.dmetag.utils.Types.DBID
import upickle.default.read

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js

/**
 * Top level display for commands
 */
object Display {
  // Form display, that can be observed.  It is updated to appropriate html elements based on new events
  // for top level UI bus (displayBus)
  private[main] lazy val formDisplay: EventStream[List[L.HtmlElement]] =
  DisplayBus.displayEvents.map {
    // Go to idle state with nothing to display
    case Idle =>
      List.empty

    // Clear out any messages and display new top-level form
    case DisplayForm(f) =>
      MessageBus.clearMsg()
      List(f)

    // Act on HTTP request response (currently just used for Login response from FormDisplay)
    case RequestResponse(command, response) =>
      command match {
        case LOGIN =>
          Status.checkJsonStatus(response) match {
            case Some(err) =>
              LoginInfo.LoginResponse.set(None)
              MessageBus.displayErr(err)
            case None =>
              // Deserialize login info
              val loginInfo = read[Login](response)
              // Remember it for all to see
              LoginInfo.LoginResponse.set(Some(loginInfo))
              // Welcome user to DMetaG
              MessageBus.displayInfo(s"${loginInfo.data.user.name} welcome to DMetaG")
              // Go setup MetaData (put on bus to delay till LoginResponse info set completes)
              DisplayBus.displayBusOnNext(LoginDone)
          }
          // Blank out login form displayed
          List.empty
        // Nothing to do for other commands
        case _ =>
          List.empty
      }

    // Initialize metadata (done once login completes)
    case LoginDone =>
      DMetaDefs.getDMetaDefs.map {
        case Left(err) => MessageBus.displayErr(err)
        case Right(md) => DMetaDefsVar.setMDDef(md)
      }
      // Nothing to display
      List.empty

    // Form to get barcode ID submitted - now go find entries with ID specified
    case SubmittedFormForFindID(mdDefs, form) =>
      val entryData = Forms.formToMap(form)
      entryData.get(DMetaDefs.COMMON_FIELD) match {
        case None => MessageBus.displayErr(s"${DMetaDefs.COMMON_FIELD} not found in form")
        case Some(id) =>
          DMetaRequests.findEntryByCommonID(mdDefs = mdDefs, id = id.toString).map {
            case Left(err) => MessageBus.displayErr(s"Error finding unique ${DMetaDefs.COMMON_FIELD} entry: $err")
            case Right((collName, foundEntryData)) =>
              val visibleData = mdDefs.getVisibleFieldData(collName, foundEntryData)
              displayWorkflowForFind(mdDefs = mdDefs, collName = collName, entryCriteria = visibleData)
          }
      }
      // Blank out display
      List.empty

    // Display preliminary form to complete command - subdisplay entryActionApp expands as needed to
    // complete the command.
    case NewCommandForm(mdDefs, action) =>
      List(Forms.makePureForm(name = "Entry type selection", fields = entryActionApp(action, mdDefs)))

    // Initialize display of from to get entry data - subdisplay must be started up
    case InitDisplayEntryData(action, newForm, getSubdisplay) =>
      MessageBus.clearMsg()
      // Get subdisplay reactive elements to do entry display, and observer to receive events to update
      // subdisplay
      val (display, observer) = getSubdisplay(action)
      // Send out request to display data - we can't update display with the new entry form yet
      // because reactive elements, controlled by subdisplay setup by getSubdisplay, aren't mounted till the
      // subdisplay is linked in to the main display.  The subdisplay is linked in via this action when the main
      // display is updated once this initial event completes and is observed.  We can't sent out events to the
      // subdisplay till that occurs. Since Laminar transactions guarantee that the current event, which will mount
      // the subdisplay, will complete before the next event is sent to our observer, we initiate another event
      // (DisplayEntryData) to follow up this event and finish up display of the entry.
      DisplayBus.displayBusOnNext(DisplayEntryData(newForm, display, observer))
      // Put up initial reactive elements, that includes pointer to subdisplay including subdisplay that
      // will mount element display
      List(display)

    // Display element using display and form created via subDisplay in InitDisplayEntryData
    // The subdisplay is controlled by entryObserver that was setup in subDisplay call in InitDisplayEntryData
    case DisplayEntryData(formData, display, entryObserver) =>
      entryObserver.onNext(formData)
      List(display)

    // Display error response from HTTP request
    case RequestError(requestType, response) =>
      MessageBus.displayErr(s"$requestType failed: $response")
      List.empty

    // Display some random html - for example a table of choices for find or entries to be deleted
    case DisplayHtml(html) =>
      List(html)
  }

  /**
   * Form fieldset used to complete a command for an entry.  It consists of:
   * 1. A simple legend set as appropriate for the command (new, find, ...).
   * 2. Select to get what type of entry to work with. Once an entry type is selected
   *    additional forms are displayed to query for additional information needed.
   * 3. Section that expands, based on what's been selected so far, to display sub-forms to get additional
   *    information needed.
   * @param action command being worked on
   * @param mdDefs metadata definitions
   * @return fields setup for initial selection and followup action
   */
  private def entryActionApp(action: Command, mdDefs: DMetaDefs): ReactiveHtmlElement[FieldSet] = {
    // Make our own bus for this form to use - crucial so events for this form only go to this form and not other
    // forms created before or after. When the form is unmounted Laminar should unsubscribe the bus and garbage
    // collection should clean up the memory used.
    val stateBus = new L.EventBus[FormState]
    // Return display elements
    L.fieldSet(
      L.legend(Command.commandDesc(action)),
      entryFormDiv(action, stateBus.writer, mdDefs),
      L.child <-- EntryForms.getFormOrBlank(
        entryAppState(
          mdDefs = mdDefs, action = action, observer = stateBus.writer, events = stateBus.events)
      )
    )
  }

  /**
   * Create a div, with a selector used to pick what entry type to work on. Attached to the selector is
   * an onChange event that reports to the observer when an entry type is selected.
   * @param action command being worked on
   * @param observer where to send selection changes
   * @param mdDefs metadata definitions
   * @return div with entry type selector
   */
  private def entryFormDiv(action: Command, observer: L.Observer[FormState], mdDefs: DMetaDefs)
  : L.Div = {
    /*
     * Get a collection form definition and return state to use for displaying form.
     * @param action action being worked on
     * @param collSelected type of collection item selected
     * @return next state based on server response
     */
    def getEntryFormDefinition(mdDefs: DMetaDefs, action: Command, collSelected: String): Future[FormState] = {
      DMetaRequests.findFKDataForAll(mdDefs = mdDefs, collName = collSelected, fkDataFound = None)
        .map { /* Future Result */
          case Left(err) => FormError(err)
          case Right(fks) =>
            EntryForms.getFormDefinition(mdDefs = mdDefs, fkData = fks, formSelected = collSelected,
              setUIState = (formSelected, fields, fkData) => {
              val f = EntryForms.makeEntryForm(formSelected = formSelected,
                label = mdDefs.collLabels.getOrElse(formSelected, formSelected),
                fields = fields,
                // Fields not required if doing a search for an existing entry
                isRequired = !(action == FIND), fkFields = fkData)
                NewCollForm(collSelected, None, f)
              },
            setErrState = (_, err) => {
              FormError(err)
            })
        }
    }

    // Get select element and div containing select (if looking for an element in a workflow (FIND) exclude workflow
    // from list - workflows are not recursive)
    val choices = mdDefs.collLabels.map {
      case (name, label) => Some(name) -> label
    }.toList.sortBy(_._2)
    val (sel, div) = Forms.entrySelectAndDiv(choices)
    // Add on change event to bring up additions to form
    sel.amend(
      L.inContext { thisNode =>
        // When a new entry type is selected an additional form is displayed to get the necessary data to
        // complete the command - the request to display the new form is sent to the observer
        L.onChange.mapTo(thisNode.ref.value)
          .filter(_ != "") -->
          (selColl => {
            EventHandlers.toObserver(
              event = selColl,
              processEvent = getEntryFormDefinition(mdDefs, action, _),
              completionObserver = observer
            )
          })
      }
    )
    div
  }

  /**
   * Stream to update entries based on new events.
   * @param mdDefs metadata definitions
   * @param action command being worked on
   * @param observer where to send commands for next steps
   * @param events input stream of events
   * @return stream of forms to display
   */
  private[main] def entryAppState(mdDefs: DMetaDefs,
                                  action: Command, observer: L.Observer[FormState], events: L.EventStream[FormState])
  : EventStream[Option[L.FormElement]] = {
    // Set up partial function of entry states
    val entryState: PartialFunction[FormState, Option[L.FormElement]] = {
      // Display new form and setup to receive FormSubmitted later
      case NewCollForm(collName, entryID, form) =>
        MessageBus.clearMsg()
        Some(EntryForms.entryFormToSubmit(observer = observer, collName, entryID, form))

      case FormForCollSubmitted(collName, entryID, form, json) =>
        /*
         * Helper method to process request - send off json request and upon completion display error or display
         * workflow graph for entry
         */
        def doRequest(formEntries: js.Iterable[js.Tuple2[String, Any]], url: String, method: HttpMethod): Unit = {
          Requests.jsonRequest(
            entries = formEntries,
            url = url,
            method = method
          ).onComplete(requestCompletion =>
            if (requestCompletion.isFailure)
              MessageBus.displayErr(
                requestCompletion.failed
                  .getOrElse(new Exception("Request failed"))
                  .getLocalizedMessage
              )
            else {
              DMeta.parseJson(requestCompletion.get) match {
                case Left(err) => MessageBus.displayErr(err)
                case Right(entryMap) =>
                  if (entryMap.length != 1)
                    MessageBus.displayErr(s"More than one entry found for operation done in $collName")
                  else {
                    val entry = entryMap.head
                    entry.get(DMetaDefs.ID_FIELD) match {
                      case None =>
                        MessageBus.displayErr(s"${DMetaDefs.ID_FIELD} not found in returned data from $collName")
                      case Some(id) =>
                        displayGraphForEntryID(mdDefs = mdDefs, collName = collName, entryID = id.toString)
                    }
                  }
              }
            }
          )
        }

        // If find then go look for entry wanted and display graph when entry found
        if (action == FIND) {
          val entryData = Forms.formToMap(form)
          displayWorkflowForFind(mdDefs = mdDefs, collName = collName, entryCriteria = entryData)
        } else {
          // Get collection and project URL slugs
          mdDefs.getCollAndProjSlugs(collName) match {
            case None => MessageBus.displayErr(s"Unable to find project for $collName")
            case Some((projSlug, collSlug)) =>
              //need currentTarget.asInstanceOf[dom.html.Form]) if onSubmit not on form?
              // Get data in form
              val formEntries = Forms.formToJSIterable(form)
              if (action == UPDATE) {
                // Process update to entry
                entryID match {
                  case None => MessageBus.displayErr(s"EntryID not set for update to $collName")
                  case Some(eID) =>
                    doRequest(
                      formEntries = formEntries,
                      url = URLs.dmetaDocumentURL(projectName = projSlug, collectionName = collSlug, docID = eID),
                      method = HttpMethod.PATCH
                    )
                }
              } else if (action == NEW) {
                // Process addition of new entry
                doRequest(
                  formEntries = formEntries,
                  url = URLs.dmetaCollectionURL(projectName = projSlug, collectionName = collSlug),
                  method = HttpMethod.POST
                )
              } else
                MessageBus.displayErr(s"Unknown action $action")
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
      entryState orElse {
        // Unknown - just put up nothing
        case unknown =>
          MessageBus.displayErr(s"Unknown entryAppState: $unknown")
          None
      }
    }
  }

  /**
   * Go look for entry used for current workflow and if found display the workflow graph.
   * @param mdDefs metadata definitions
   * @return all displays are done when the future looking for the entry data completes
   */
  private[main] def displayWorkflow(mdDefs: DMetaDefs) = {
    GraphDisplay.LastGraph.now() match {
      case Some((entryID, collName)) =>
        displayGraphForEntryID(mdDefs = mdDefs, collName = collName, entryID = entryID)
      case None =>
        GraphDisplayBus.wfSvgClear()
    }
  }

  /**
   * Find entry to do workflow display, then display workflow with focus on entry and display entry contents.
   * @param mdDefs metadata Definitions
   * @param collName collection name (projectName_collectionName)
   * @param entryID entry ID
   * @return true if all displays are done
   */
  private[main] def displayGraphForEntryID(mdDefs: DMetaDefs, collName: String, entryID: DBID) = {
    DMetaRequests.findEntryDataForIDWithCollName(mdDefs = mdDefs, dbID = entryID, collName = collName)
      .flatMap {/* Get find Future result */
        case Left(err) =>
          if (DMetaRequests.isNotFound(err))
            MessageBus.displayWarn(s"Unable to display workflow for $collName entry (ID $entryID) not found")
          else
            MessageBus.displayErr(s"Unable to display workflow for $collName (ID $entryID): $err")
          GraphDisplayBus.wfSvgClear()
          DisplayBus.displayClear()
          Future.successful(false)
        case Right(entryValues) =>
          displayGraphForEntryFound(mdDefs = mdDefs, collName = collName, entryID = entryID, entryData = entryValues)
            .map(/* Future done*/ ok => {
              // Go display entry
              if (ok) displayEntry(mdDefs = mdDefs, entryID = entryID, collName = collName)
              ok
            })
      }
  }

  /**
   * Go display the workflow graph.
   * @param mdDefs metadata definitions
   * @param collName collection name
   * @param entryID entry DBID
   * @param entryData entry field values
   * @return true if all displays are done
   */
  private def displayGraphForEntryFound(mdDefs: DMetaDefs, collName: String, entryID: DBID, entryData: Map[String, Any])
  : Future[Boolean] = {
    FindDisplay.findDisplay(
      mdDefs = mdDefs, collName = collName, entryDBID = entryID, entryValues = entryData
    ).map { /* Get display Future result */
      case Left(err) =>
        MessageBus.displayWarn(s"Display of workflow for $collName entry $entryID failed: $err")
        GraphDisplayBus.wfSvgClear()
        DisplayBus.displayClear()
        false
      case Right((wfGraph, displayGraph)) =>
        GraphDisplayBus.wfSvgOnNext(
          WorkFlowDisplay(entryID = wfGraph.basedOn, collName = collName,
            graph = displayGraph, mdDefs = mdDefs, highlight = wfGraph.path)
        )
        true
    }
  }

  /**
   * Go look for wanted entry and if found display the workflow graph and entry found.  If multiple entries are found
   * display a table from which the user can pick which entry to display.
   * @param mdDefs metadata definitions
   * @param collName collection name
   * @param entryCriteria search criteria
   * @return true if displays are done, false if there's an error
   */
  private[main] def displayWorkflowForFind(mdDefs: DMetaDefs, collName: String, entryCriteria: Map[String, Any])
  : Future[Boolean] = {
    Find.getFindRows(mdDefs, collName, entryCriteria).flatMap {
      // Error - go report it
      case Left(err) =>
        MessageBus.displayErr(err)
        Future.successful(true)
      // Got back a single entry - go display graph for it
      case Right(Left((entryID, entryMap))) =>
        val collLabel = mdDefs.collLabels.getOrElse(collName, collName)
        // If no entry criteria but only one there then it's likely to take a while to show graph so put up message
        if (entryCriteria.isEmpty || !entryCriteria.exists(_._2.toString.nonEmpty))
          MessageBus.displayInfo(s"Creating graph for one and only entry in $collLabel...")
        // Display graph for workflow for single entry found
        displayGraphForEntryFound(mdDefs = mdDefs, collName = collName, entryID = entryID, entryData = entryMap)
          .map(/* Future done*/ ok => {
            // Go display entry
            if (ok) displayEntry(mdDefs, entryID, collName)
            ok
          })
      // Got back multiple entries - display table from which user can pick which entry to display
      case Right(Right((headers, entriesWithFKs))) =>
        // Init workflow display to empty
        GraphDisplayBus.wfSvgClear()
        // Put up display to choose which entry is wanted
        DisplayBus.displayBusOnNext(DisplayHtml(
          HtmlTable.makeEntryTable(
            title = Some(s"${entriesWithFKs.length} entries found.  Click on wanted entry below."),
            headings = Some(headers.map(_.label)),
            entries = sortIDList(entriesWithFKs), striped = true, cursorPointer = true,
            onClick = Some((id: String, map: Map[String, String]) => {
              displayGraphForEntryID(mdDefs = mdDefs, collName = collName, entryID = id)
            })
          )
        ))
        Future.successful(true)
    }
  }

  /**
   * Sort entries based on entry values.
   * @param values list of (entryDBID, entryValues)
   * @return list sorted by entry values
   */
  private def sortIDList(values: List[(DBID, List[String])]): List[(DBID, List[String])] = {
    values.sortWith {
      case ((_, entriesOne), (_, entriesTwo)) =>
        // Recursive sort to keep looking at entries until end of list or non-equivalent value found
        @tailrec
        def sortLoop(entries1: List[String], entries2: List[String]): Boolean = {
          // Little helper method get get value of string at head of list
          def getEntryVal(entry: List[String]): String = entry.headOption.getOrElse("")
          // Do comparison of first entry in list
          val nextCompare = getEntryVal(entries1).compareTo(getEntryVal(entries2))
          // If comparison wasn't equal or we've reached the end of either list then exit with last comparison
          // Otherwise try on next field in entries
          if (nextCompare != 0 || entries1.isEmpty || entries2.isEmpty) {
            nextCompare < 0
          } else {
            sortLoop(entries1.tail, entries2.tail)
          }
        }
        // Go sort list
        sortLoop(entriesOne, entriesTwo)
    }
  }

  /**
   * Go look for wanted entry and if found display the workflow graph and entry found.
   * @param mdDefs metadata definitions
   * @param collName collection name
   * @param entryID ID for entry  criteria
   * @return all displays are done when the future looking for the entry data completes
   */
  private[main] def displayEntry(mdDefs: DMetaDefs, entryID: DBID, collName: String)
  : Option[Future[Unit]] = {
    mdDefs.collByName.get(collName).map(coll =>
      // Get data for entry and display form with entries
      DMetaRequests.findEntryDataForID(mdDefs = mdDefs, dbID = entryID, coll = coll).map {
        case Left(err) => MessageBus.displayErr(err)
        case Right(data) =>
          // Display entry details
          FormDisplay.getUpdateForm(mdDefs = mdDefs, entryID = entryID, collName = collName, entryData = data)
            .map(DisplayBus.displayBusOnNext)
      }
    )
  }

  /**
   * Method to get metadata if available - asks to login if metadata not available
   * @return metadata definitions
   */
  def pleaseLogin: Option[DMetaDefs] = {
    DMetaDefs.getMDDefs match {
      case Left(err) =>
        MessageBus.displayErr(err)
        None
      case Right(mdDefs) =>
        Some(mdDefs)
    }
  }
}
