package edu.umassmed.dmetag.main

import com.raquo.airstream.eventstream.EventStream
import com.raquo.laminar.api._
import edu.umassmed.dmetag.state.Message._

object MessageBus {
  // Bus to receive and send out messages to be displayed
  private lazy val msgBus = new L.EventBus[Message]

  // Message display, that can be observed.  It is updated to appropriate html elements based on new events
  // from message bus (msgBus).
  private[main] lazy val msgDisplay: EventStream[List[L.HtmlElement]] =
  msgBus.events.map {
    // Nothing to display
    case NoMsg => List()
    // Message - display message, coloring as wanted
    case tMsg: TextMessage => makeMsgElements(msg = tMsg.tMsg, color = tMsg.tMsgColor)
  }

  /**
   * Make HTML elements to display message.
   * @param msg message
   * @param color color to make message
   * @return elements with label and textarea with message
   */
  private def makeMsgElements(msg: String, color: MsgColor.Value): List[L.HtmlElement] =
    List(
      L.label("Status", L.forId := "stat"),
      L.div(L.cls := "pure-g",
        L.textArea(L.readOnly := true, L.idAttr := "stat", L.cls := "pure-u-7-8", L.color := color.toString, msg)
      ),
      L.br()
      // If we want an OK button to get rid of message - don't think it's needed
      // L.button("OK", L.cls := "pure-button", L.cls := "pure-button-primary")
      //   .amend(L.onClick --> (_ => msgBus.writer.onNext(NoMsg)))
    )


  /**
   * Clear out message display
   */
  private[main] def clearMsg(): Unit =
    msgBus.writer.onNext(NoMsg)

  /**
   * Write out information message to message bus
   * @param s message
   */
  private[main] def displayInfo(s: String): Unit =
    msgBus.writer.onNext(InfoMsg(s))

  /**
   * Write out warning message to message bus
   * @param s message
   */
  private[main] def displayWarn(s: String): Unit =
    msgBus.writer.onNext(WarnMsg(s))

  /**
   * Write out error message to message bus
   * @param s message
   */
  private[main] def displayErr(s: String): Unit =
    msgBus.writer.onNext(ErrMsg(s))
}
