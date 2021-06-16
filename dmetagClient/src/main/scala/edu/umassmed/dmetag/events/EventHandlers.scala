package edu.umassmed.dmetag.events

import com.raquo.domtypes.jsdom.defs.events.TypedTargetEvent
import com.raquo.laminar.api._
import org.scalajs.dom

import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.util.{Failure, Success}

/**
 * Some handlers to convert events, processed through futures, to Laminar streams/observers.
 */
object EventHandlers {
  /**
   * Convert a future dom event into a stream.
   * @param event dom event
   * @param getFuture convert event into a future
   * @tparam A event type
   * @tparam B future type
   * @return stream of values from future
   */
  def toStream[A <: dom.EventTarget, B]
  (
    event: TypedTargetEvent[A],
    getFuture: TypedTargetEvent[A] => Future[B]
  ): L.EventStream[B] = {
    L.EventStream.fromFuture(getFuture(event))
  }

  /**
   * Convert an event into a future and upon completion write the return value to an observer.
   * @param event event
   * @param processEvent event processor returning a future
   * @param completionObserver observer to receive future completion data
   * @tparam A event type
   * @tparam B future type
   */
  def toObserver[A, B]
  (
    event: A,
    processEvent: A => Future[B],
    completionObserver: L.Observer[B]
  ): Unit = {
    processEvent(event).onComplete {
      case Success(value) => completionObserver.onNext(value)
      case Failure(exception) => completionObserver.onError(exception)
    }
  }
}
