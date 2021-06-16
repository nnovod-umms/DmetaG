package edu.umassmed.dmetag.main

import com.raquo.airstream.eventbus.WriteBus
import com.raquo.airstream.eventstream.EventStream
import com.raquo.laminar.api._
import edu.umassmed.dmetag.state.UIState.{Idle, UIState}

object DisplayBus {
  // Bus to receive and send out top level UI state changes
  private lazy val displayBus = new L.EventBus[UIState]

  /**
   * Send out a new state to the display bus that can be observed via events.
   * @param state next UI state
   */
  private[main] def displayBusOnNext(state: UIState): Unit = {
    displayBus.writer.onNext(state)
  }

  /**
   * Get events stream for display bus
   */
  private[main] def displayEvents: EventStream[UIState] = displayBus.events

  /**
   * Get writer (observable) for display bus
   */
  private[main] def displayWriter: WriteBus[UIState] = displayBus.writer

  /**
   * Idle display
   */
  private[main] def displayClear(): Unit = displayBusOnNext(Idle)
}
