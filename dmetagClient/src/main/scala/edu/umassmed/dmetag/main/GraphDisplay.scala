package edu.umassmed.dmetag.main

import com.raquo.airstream.eventstream.EventStream
import com.raquo.domtypes.jsdom.defs.events.TypedTargetMouseEvent
import com.raquo.laminar.api._
import com.raquo.laminar.modifiers.EventPropBinder
import com.raquo.laminar.nodes.ReactiveHtmlElement
import com.raquo.laminar.nodes.ReactiveSvgElement.Base
import edu.umassmed.dmetag.dmeta.{DMeta, DMetaDefs, DMetaRequests, URLs}
import edu.umassmed.dmetag.events.Requests
import edu.umassmed.dmetag.state.ContextMenuCommand
import edu.umassmed.dmetag.state.FormState.FormState
import org.scalajs.dom
import org.scalajs.dom.experimental.HttpMethod
import org.scalajs.dom.html.LI
import org.scalajs.dom.Element
import edu.umassmed.dmetag.dmeta.DMeta
import edu.umassmed.dmetag.js.JsNative.FormData
import GraphDisplayBus._
import edu.umassmed.dmetag.state.ContextMenuState._
import edu.umassmed.dmetag.state.FormState._
import edu.umassmed.dmetag.state.UIState._
import ContextMenuCommand._
import edu.umassmed.dmetag.display.{HtmlTable, SvgElements}
import edu.umassmed.dmetag.state.WorkFlowState.{WorkFlowDisplay, WorkFlowSvg}
import edu.umassmed.dmetag.utils.Types.DBID
import edu.umassmed.dmetag.utils.{RequestUtils, UniqueID, Utils}

// Import future and execution context for futures
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
 * Methods to display the graph, including the context menu and its operations.
 */
object GraphDisplay {
  // Remember last graph (focusEntryID, collectionName) displayed (none at start)
  val LastGraph: L.Var[Option[(DBID, String)]] = L.Var[Option[(DBID, String)]](None)

  // Is there a workflow
  def isWorkflow: Boolean = LastGraph.now().nonEmpty

  // Update display based on new events arriving for the workflow
  private[main] lazy val wfSvgDisplay: EventStream[List[Base]] = wfSvgEvents.map {
    // Display svg already created
    case WorkFlowSvg(svg) =>
      List(svg)
    case WorkFlowDisplay(entryID, collName, graph, mdDefs, highlight) =>
      LastGraph.set(Some(entryID, collName))
      // Display graph
      List(SvgElements.workflow(graph = graph, isBlankIfEmpty = true,
        nodeOnClick = workflowNodeSelect(mdDefs, _, _, _, _),
        nodeOnDblClick = None,
        contextMenu = displayContextMenu(_, _, mdDefs, _, _),
        highlight = highlight, title = "Workflow Graph"
      ))
    case _ =>
      List.empty
  }

  // Bus to receive and send out context menu display events
  private lazy val contextMenuBus = new L.EventBus[ContextMenuState]

  private def contextMenuEvent(event: ContextMenuState): Unit = contextMenuBus.writer.onNext(event)

  private[main] def initContextMenu(): Unit = contextMenuEvent(InitMenu)

  // Context menu stream to setup display.  It is updated to appropriate html elements based on new events from context
  // menu bus.
  private[main] lazy val cmDisplay: EventStream[L.Div] = {
    contextMenuBus.events.map {
      case InitMenu => L.div()
      case DisplayMenu(div) =>  div
    }
  }

  /**
   * Make a context menu for an item in a workflow and then send it out on the context menu to be displayed.
   * @param collName collection name for workflow entry (projectName_catalogName)
   * @param entryID DBID for entry for which we are making a context menu
   * @param mdDefs metadata definitions
   * @param xPos x position to place context menu
   * @param yPos y position to place context menu
   */
  private def displayContextMenu(entryID: DBID, collName: String, mdDefs: DMetaDefs, xPos:Double, yPos:Double)
  : Unit = {
    val div = makeContextMenu(collName, entryID, mdDefs)
    // Set position of menu and display it
    div.ref.setAttribute("style", s"position:fixed;display:block;top:${yPos}px;left:${xPos}px")
    contextMenuEvent(DisplayMenu(div))
  }

  /**
   * Make a context menu for an item in a workflow.
   * @param collName collection name for workflow entry (projectName_catalogName)
   * @param entryID DBID for entry for which we are making a context menu
   * @param mdDefs metadata definitions
   */
  private def makeContextMenu(collName: String, entryID: DBID, mdDefs: DMetaDefs): L.Div = {

    // Method to make individual menu item
    def makeContextMenuItem[T]
    (
      command: ContextMenuCommand,
      label: String,
      selectInput: T,
      uponSelect: T => EventPropBinder[TypedTargetMouseEvent[dom.Element]]
    ): ReactiveHtmlElement[LI] =
      L.li(L.idAttr := command.toString, label, uponSelect(selectInput))

    // Method to make menu item for parents or children
    def makeRelMenuItem[T]
    (
      command: ContextMenuCommand,
      getRelNames: String => Either[String, List[String]],
      menuLabel: String,
      contextMenuFunction:
      (ContextMenuCommand, String, DBID, String, DMetaDefs) => PartialFunction[ContextMenuCommand, Unit]
    ): Either[String, List[ReactiveHtmlElement[LI]]] = {
      // Get names of relatives (parents or children)
      getRelNames(collName).map(relNames => {
        // Get labels for names
        val relLabels = relNames.flatMap(relName => {
          mdDefs.collLabels.get(relName).map(relLabel => (relName, relLabel))
        })
        // Make context menu selection
        relLabels.map {
          case (relCollName, relLabel) =>
            makeContextMenuItem(command = command, label = s"$menuLabel $relLabel",
              selectInput = command,
              uponSelect = contextMenuAction(_, contextMenuFunction(command, collName, entryID, relCollName, mdDefs))
            )
        }
      })
    }

    // Get individual items in context menu for children of entry - we want to be able to add a new or existing item
    // for each child type in the workflow definition
    val contextItems =
    makeRelMenuItem(ADD_NEW, mdDefs.findChildrenNames, "Add new", contextMenuAdd)
      .flatMap(childItems => {
        makeRelMenuItem(MOVE, mdDefs.findParentNames, "Move to different", contextMenuMove)
          .map(parentItems => {
            // Add items for current instance to removing it from workflow
            childItems ++ parentItems ++
              List(
                makeContextMenuItem(command = DELETE, label = s"Delete",
                  DELETE, contextMenuAction(_, contextMenuRemove(collName, entryID, mdDefs))
                )
              )
          })
      })

    // Get unique id for menu
    val jctx_id = s"jctx-id${UniqueID.getLocalID("ctx")}"

    // Get the list of context items
    val items = contextItems match {
      case Left(err) =>
        MessageBus.displayErr(s"Error constructing context menu: $err")
        List.empty
      case Right(liList) => liList
    }

    // Create div with menu
    L.div(
      L.className := s"jctx-host $jctx_id",
      L.ul(L.className := s"jctx $jctx_id jctx-white jctx-white-shadow", items)
    )
  }

  /**
   * Partial function to process context menu add command.  We put up a display to get data for the new entry and
   * then add the entry to the workflow.
   * @param command add command
   * @param collName collection name of entry to add
   * @param entryID workflow entry to add below
   * @param nextCollName collection to add
   * @param mdDefs metadata definitions
   * @return partial function to do add of a new item to the workflow
   */
  private def contextMenuAdd(command: ContextMenuCommand, collName: String, entryID: DBID,
                             nextCollName: String, mdDefs: DMetaDefs)
  : PartialFunction[ContextMenuCommand, Unit] = {
    case ADD_NEW =>
      // Little helper method to send an error to the display bus
      def displayErr(err: String): Unit = DisplayBus.displayBusOnNext(RequestError(command.toString, err))
      // Find relationship between existing entry and new one to be added
      mdDefs.findRelationship(collName, nextCollName) match {
        case None => displayErr(s"Can't find relationship between $collName and $nextCollName")
        case Some(rel) =>
          // Get primary key value for existing entry
          DMetaRequests.getPKForEntry(mdDefs, collName, entryID).map {
            case Left(err) => displayErr(err)
            case Right(label) =>
              // Setup foreign key values to use
              val fkValsFound = Map(rel.fk.field.name -> List((entryID, label)))
              // Get any remaining foreign key values
              DMetaRequests.findFKDataForAll(
                mdDefs = mdDefs, collName = nextCollName, fkDataFound = Some(fkValsFound)
              ).map { /* Future result */
                case Left(err) => displayErr(err)
                case Right(fks) =>
                  // Make form and display it
                  DisplayBus.displayBusOnNext(
                    EntryForms.getFormDefinition(
                      mdDefs = mdDefs,
                      fkData = fks,
                      formSelected = nextCollName,
                      setUIState = (formSelected, fields, fkData) => {
                        val f = EntryForms.makeEntryForm(
                          formSelected = formSelected,
                          label = mdDefs.collLabels.getOrElse(formSelected, formSelected),
                          fields = fields,
                          isRequired = true, // Require fields for new entry
                          fkFields = fkData, // Foreign key choices for foreign keys we didn't input via fkDataFound
                          values = Map(rel.fk.field.name -> entryID) // Set foreign key value known
                        )
                        // Setup to do display to get new item's info and return with new item to app to add item to workflow
                        InitDisplayEntryData(
                          action = command.id, display = NewCollForm(nextCollName, None, f) ,
                          displayBus = contextMenuDisplayApp(_, collName, entryID, mdDefs)
                        )
                      },
                      setErrState = (formSelected, err) => {
                        RequestError(command.toString, err)
                      }
                    )
                  )
              }
          }
      }
  }

  /**
   * Partial function to process context menu move command.  We put up a display to get data for where to move the
   * item and then do the move.
   * @param command move command
   * @param collName collection name of entry to move
   * @param entryID workflow entry to move
   * @param nextCollName collection type to move from/to
   * @param mdDefs metadata definitions
   * @return partial function to do move of an item between parents
   */
  private def contextMenuMove(command: ContextMenuCommand, collName: String, entryID: DBID,
                              nextCollName: String, mdDefs: DMetaDefs)
  : PartialFunction[ContextMenuCommand, Unit] = {
    case MOVE =>
      // Little helper method to send an error to the display bus
      def displayErr(err: String): Unit = DisplayBus.displayBusOnNext(RequestError(command.toString, err))
      // Find relationship between existing entry and parent collection we're switching from
      mdDefs.findRelationship(parentName = nextCollName, childName = collName) match {
        case None => displayErr(s"Can't find relationship between $nextCollName and $collName")
        case Some(rel) =>
          // Get foreign keys with labels including all levels of parents
          DMetaRequests.getCompleteFK(mdDefs, rel).map {
            case Left(err) => displayErr(s"Error: $err")
            case Right(found) =>
              if (found.isEmpty)
                displayErr(s"No data found for relationship between $nextCollName and $collName")
              else {
                DisplayBus.displayBusOnNext {
                  val f = EntryForms.makeEntryForm(
                    formSelected = collName,
                    label = mdDefs.collLabels.getOrElse(collName, collName),
                    fields = List(rel.fk.field), // Just want one field
                    isRequired = true, // Require fields for new entry
                    fkFields = found // Foreign key choices
                  )
                  // Setup to do display to get new item's info and return with new item to app to add item to workflow
                  InitDisplayEntryData(
                    action = command.id, display = NewCollForm(collName, Some(entryID), f),
                    displayBus = contextMenuDisplayApp(_, collName, entryID, mdDefs)
                  )
                }
              }
          }
      }
  }

  /**
   * Partial function to process context menu delete command.  The chosen entry, and its descendants, are deleted.
   * @param collName collection name
   * @param entryID workflow entry to remove
   * @param mdDefs metadata definitions
   * @return partial function to do delete of entries
   */
  private def contextMenuRemove(collName: String, entryID: DBID, mdDefs: DMetaDefs)
  : PartialFunction[ContextMenuCommand, Unit] = {
    case DELETE =>
      // Get labels for entries to be deleted
      DMetaRequests.getChildrenLabels(mdDefs = mdDefs, parentDBID = entryID, collectionName = collName).map {
        case Left(err) => MessageBus.displayErr(err)
        case Right(data) =>
          // Make list of entry descriptions to display for confirmation
          val deleteList =
            data.toList.map {
              case (entryID, (_, collLabel, entryLabel)) =>
                (entryID, List(collLabel, entryLabel, entryID))
            }.sortBy(entry => entry._2.head)
          // Go display confirmation
          val numEntries = deleteList.length
          val entryStr = if (numEntries == 1) "entry" else "entries"
          val deleteMsg = s"Delete $numEntries $entryStr below?"
          val confirmationList =
            HtmlTable.makeEntryTable(
              title = Some(deleteMsg), headings = Some(List("Entry Type", "Entry", "ID")),
              entries = deleteList, striped = true, cursorPointer = false, onClick = None
            )
          val confirm =
            List(
              L.br(),
              L.button("Cancel", L.cls := "pure-button pure-button-primary", L.marginRight("12px"),
                L.onClick --> (_ => {
                  MessageBus.displayInfo("Delete cancelled")
                  DisplayBus.displayClear()
                })),
              L.button("Delete", L.cls := "pure-button pure-button-primary",
                L.onClick --> (_ => {
                  // Temp function that makes each delete wait for the previous one
                  // Otherwise deletes after 1st are failing
                  def doDelete(list: List[(DBID, (String, _, _))], soFar: List[Either[String, String]]): Unit = {// List[Either[String, String]] = {
                    if (list.isEmpty) {
                      val (failures, successes) = RequestUtils.getEitherLists(soFar)
                      if (failures.nonEmpty)
                        MessageBus.displayErr(s"Error deleting entries: $failures")
                      else {
                        /* Kludge to wait a second to have deletes take effect (DB caches?) */
                        Utils.delay(1000).onComplete(_ => {
                          // Put up message that delete is complete
                          MessageBus.displayInfo(s"Delete complete for $numEntries $entryStr")
                          // Clear out entry display
                          DisplayBus.displayClear()
                          // See if we deleted entry workflow was focused on
                          val isFocusDeleted =
                            LastGraph.now() match {
                              case Some((wfEntryID, _)) =>
                                deleteList.exists {
                                  case (delID, _) => delID == wfEntryID
                                }
                              case None => false
                            }
                          // Redisplay workflow if delete didn't get rid of focus
                          // Otherwise clear out workflow
                          if (!isFocusDeleted) {
                            Display.displayWorkflow(mdDefs)
                          } else {
                            GraphDisplayBus.wfSvgClear()
                            DisplayBus.displayClear()
                          }
                        })
                      }
                    } else {
                      val (dbID, (collName, _, _)) = list.head
                      val requestAction = {
                        (projSlugName: String, collSlugName: String) => {
                          val url = URLs.dmetaDocumentURL(projSlugName, collSlugName, dbID)
                          Requests.deleteRequest(url)
                        }
                      }
                      DMetaRequests.doIDRequest(
                        mdDefs = mdDefs,
                        collName = collName,
                        doRequest = requestAction,
                        idString = s"delete of entries in $collName"
                      ).onComplete(stat => {
                        val compStat = stat.getOrElse(Left(s"Can't fetch completion for $collName $dbID"))
                        doDelete(list.tail, compStat :: soFar)
                      })
                    }
                  }

                  doDelete(data.toList, List.empty)
                  /* Doesn't work if deletes are done async as below - all but first fail with not found
                  // Go do all the deletes together
                  val deletes =
                    data.map {
                      case (dbID, (collName, _, _)) =>
                        // Delete request callback function
                        val requestAction = {
                          (projSlugName: String, collSlugName: String) => {
                            val url = URLs.dmetaDocumentURL(projSlugName, collSlugName, dbID)
                            org.scalajs.dom.console.log(s"proj: $projSlugName, coll: $collSlugName, id: $dbID, url: $url")
                            Requests.deleteRequest(url)
                          }
                        }
                        // Do delete
                        DMetaDefs.doIDRequest(
                          collName = collName,
                          doRequest = requestAction,
                          idString = s"delete of entries in $collName",
                          mdDefs = mdDefs
                        )
                    }
                  // Make list of Futures into a single Future returning a list of parent entry data
                  Future.sequence(deletes.toList).map(deletesDone => {
                    // Get failures and successes - if any failures abort
                    val (failures, successes) = DMetaDefs.getEitherLists(deletesDone)
                    if (failures.nonEmpty)
                      displayErr(s"Error deleting entries: $failures")
                    else {
                      displayInfo(s"Delete complete for $numEntries $entryStr")
                      displayClear()
                      // Redisplay workflow if possible
                      LimsDisplay2.displayWorkflow(mdDefs)
                    }
                   })
*/
                })
              )
            )
          // Put up display of what will be deleted to ask for confirmation
          DisplayBus.displayBusOnNext(DisplayHtml(confirmationList.amend(confirm)))
      }
  }

  /**
   * Callback when a node is selected in a workflow. Assuming there's no error a display of the node is started and the
   * graph is redrawn with the selected node as the focus.
   * @param mdDefs metadata definitions
   * @param nodeID node DBID
   * @param collName collection name (projectName_collectionName)
   * @param title title used for node
   * @param subTitle subtitle used for node
   * @return initializes display of form with data for item selected
   */
  private def workflowNodeSelect(mdDefs: DMetaDefs, nodeID: DBID, collName: String, title: String, subTitle: String)
  : Unit = {
    mdDefs.collByName.get(collName) match {
      case None =>
        MessageBus.displayErr(s"Unable to find collection entry for $collName")
      case Some(coll) =>
        // Display graph with entry as focus
        Display.displayGraphForEntryID(mdDefs = mdDefs, collName = collName, entryID = nodeID)
    }
  }

  /**
   * Method with actions to take when a context menu choice is made.
   * @param command menu choice
   * @param commandFunction partial function to process command
   * @return onClick callback to complete action when item in context menu is chosen
   */
  private def contextMenuAction(command: ContextMenuCommand,
                                commandFunction: PartialFunction[ContextMenuCommand, Unit])
  : EventPropBinder[TypedTargetMouseEvent[Element]] =
    L.onClick --> (_ => {
      contextMenuEvent(InitMenu)
      // Make match for unknown command - should never be reached but doesn't hurt
      val unknownAction: PartialFunction[ContextMenuCommand, Unit] = {
        case _ =>
      }
      // Apply partial function - choose action based on command
      (commandFunction orElse unknownAction)(command)
    })

  /**
   * Form fieldset used to take an action for an entry in a workflow.  It contains a section that expands,
   * based on new events, to display sub-forms to get additional information needed.
   * @param action workflow action being worked on
   * @param collName collection name for entry (collectionName_projectName)
   * @param entryID workflow entry DBID that context menu action is being taken for
   * @param mdDefs metadata definitions
   * @return (fields setup for to display form, observer to see events to display)
   */
  private def contextMenuDisplayApp(action: Int, collName: String, entryID: DBID, mdDefs: DMetaDefs) = {
    // Make our own bus for this form to use so events for this form only go to this form
    val stateBus = new L.EventBus[FormState]
    val topForm =
      L.fieldSet(
        L.child <-- getFormOrBlank(
          contextMenuDisplayAppState(
            action = ContextMenuCommand(action), mdDefs = mdDefs, collName = collName, entryID = entryID,
            observer = stateBus.writer, events = stateBus.events
          )
        )
      )
    (topForm, stateBus.writer)
  }

  /**
   * Little fellow to map return of no form to blank Div.
   * @param formStream stream of form elements to display
   * @return stream of element (blank div if original streamed output is None)
   */
  private def getFormOrBlank(formStream: EventStream[Option[L.FormElement]]): EventStream[L.HtmlElement] =
    formStream.map(_.getOrElse(L.div()))

  /**
   * Stream to update workflow entries based on new events from context menu.
   * @param action workflow action being worked on
   * @param mdDefs metadata definitions
   * @param observer where to send commands for next steps
   * @param collName collection name for entry (collectionName_projectName)
   * @param entryID workflow entry DBID that actin is being taken for
   * @param events input stream of events
   * @return stream of forms to be displayed
   */
  private def contextMenuDisplayAppState(action: ContextMenuCommand, mdDefs: DMetaDefs, collName: String, entryID: DBID,
                                         observer: L.Observer[FormState],
                                         events: L.EventStream[FormState])
  : EventStream[Option[L.FormElement]] =
    events.map {
      // Display a new form
      case NewCollForm(collName, entryID, form) =>
        MessageBus.clearMsg()
        Some(EntryForms.entryFormToSubmit(observer = observer, collName = collName, entryID = entryID, form = form))

      // Form has been submitted - do action requested for entry
      case FormForCollSubmitted(collName, entryID, form, json) =>
        if (action == MOVE) {
          val requestAction = {
            (projName: String, collName: String) =>  {
              val formData = new FormData(form) //need currentTarget.asInstanceOf[dom.html.Form]) if onSubmit not on form?
              Requests.jsonRequest(
                entries = formData.entries(),
                url = URLs.dmetaDocumentURL(projName, collName, entryID.get),
                method = HttpMethod.PATCH
              )
            }
          }
          DMetaRequests.doIDRequest(
            mdDefs = mdDefs,
            collName = collName,
            doRequest = requestAction,
            idString = s"move of ${entryID.get}"
          ).flatMap {
            case Left(err) =>
              Future.successful(MessageBus.displayErr(err))
            case Right(retJson) =>
              // @TODO Should check status of retJson?  Already done in doIDRequest?
              // Display graph with entry as focus
              Display.displayGraphForEntryID(mdDefs = mdDefs, collName = collName, entryID = entryID.get)
          }
        } else if (action == ADD_NEW) {
          val requestAction = {
            (projName: String, collName: String) =>  {
              val formData = new FormData(form)
              Requests.jsonRequest(
                entries = formData.entries(),
                url = URLs.dmetaCollectionURL(projName, collName),
                method = HttpMethod.POST
              )
            }
          }
          DMetaRequests.doIDRequest(
            mdDefs = mdDefs,
            collName = collName,
            doRequest = requestAction,
            idString = s"add of entry to $collName"
          ).map {
            case Left(err) =>
              MessageBus.displayErr(err)
            case Right(retJson) =>
              DMeta.parseJson(retJson) match {
                case Left(err) => MessageBus.displayErr(err)
                case Right(data) =>
                  if (data.length != 1)
                    MessageBus.displayErr(s"${data.length} entries returned after add of entry to $collName")
                  else {
                    data.head.get(DMetaDefs.ID_FIELD) match {
                      case Some(idVal) =>
                        Display.displayGraphForEntryID(
                          mdDefs = mdDefs, collName = collName, entryID = idVal.toString
                        )
                      case _ =>
                        MessageBus.displayErr(s"No _id returned after add of entry to $collName")
                    }
                  }
              }
          }
        }
        // Blank out form display (will kill this mini-app as well)
        DisplayBus.displayClear()
        None

      // Error processing form
      case FormError(err) =>
        MessageBus.displayErr(s"Error processing form: $err")
        None

      // Unknown - just put up nothing
      case _ =>
        None
    }
}
