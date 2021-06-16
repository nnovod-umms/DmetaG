package edu.umassmed.dmetag.main

import com.raquo.airstream.eventstream.EventStream
import com.raquo.laminar.api._
import edu.umassmed.dmetag.state.WorkFlowState.{WorkFlowDisplayInit, WorkFlowState}

object GraphDisplayBus {
  // Bus to receive and send out workflow events
  lazy private val wfSvgBus = new L.EventBus[WorkFlowState]

  /**
   * Send out next event for workflow
   * @param event next event
   */
  private[main] def wfSvgOnNext(event: WorkFlowState): Unit = wfSvgBus.writer.onNext(event)

  /**
   * Clear workflow display
   */
  private[main] def wfSvgClear(): Unit = wfSvgOnNext(WorkFlowDisplayInit)

  /**
   * Get workflow events stream
   * @return stream of events from workflow svg bus
   */
  private[main] def wfSvgEvents: EventStream[WorkFlowState] = wfSvgBus.events
}
