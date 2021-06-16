package edu.umassmed.dmetag.js

import org.scalajs.dom
import org.scalajs.dom.raw.HTMLFormElement

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/**
 * Javascript objects defined to be accessible from scala.js
 */
object JsNative {
  // Extend Formdata defined in scala.js to include entries method
  @js.native
  @JSGlobal("FormData")
  class FormData(form: HTMLFormElement = js.native) extends dom.FormData {
    def entries(): js.Iterable[js.Tuple2[String, js.Any]] = js.native
  }
}
