package edu.umassmed.dmetag.events

import com.raquo.domtypes.jsdom.defs.events.TypedTargetEvent
import edu.umassmed.dmetag.dmeta.LoginInfo
import org.scalajs.dom
import org.scalajs.dom.experimental.{Fetch, HttpMethod, RequestInit}
import edu.umassmed.dmetag.js.JsNative.FormData

import scala.concurrent.Future
import scala.scalajs.js

/**
 * Requests made to server
 */
object Requests {
  /**
   * Convert a form's entries to JSON and then submit that and return the JSON response as a string
   * @param targetForm event containing html form
   * @return JSON response
   */
  def formJsonRequest(targetForm: TypedTargetEvent[dom.html.Form]): Future[String] =
    htmlFormJsonRequest(targetForm.target)

  /**
   * Convert a form's entries to JSON and then submit that and return the JSON response as a string
   * @param form html form
   * @return JSON response
   */
  private def htmlFormJsonRequest(form: dom.html.Form) = {
    val formData = new FormData(form) //need currentTarget.asInstanceOf[dom.html.Form]) if onSubmit not on form?
    jsonRequest(entries = formData.entries(), url = form.action, method = HttpMethod.POST)
  }

  /**
   * Convert entries to JSON and then submit that and return the JSON response as a string
   * @param entries entries to submit
   * @param url URL to submit request to
   * @return JSON response
   */
  def jsonRequest(entries: js.Iterable[js.Tuple2[String, Any]], url: String, method: HttpMethod): Future[String] = {
    // Convert iterable to a dictionary object
    val dictionary = js.Object.fromEntries(entries)
    // Make a string formatted as JSON
    val jsonString = js.JSON.stringify(dictionary)
    // Go submit json version of form to url and get back response as json
    doRequest(url, fetchOptions(method, Some(jsonString)))
  }

  /**
   * Do GET request and return the JSON response as a string
   * @param url URL to submit request to
   * @return JSON response
   */
  def getRequest(url: String): Future[String] = doRequest(url, fetchOptions(HttpMethod.GET))

  /**
   * Do GET request and return the JSON response as a string
   * @param url URL to submit request to
   * @return JSON response
   */
  def deleteRequest(url: String): Future[String] = doRequest(url, fetchOptions(HttpMethod.DELETE))

  /**
   * Setup fetch request options
   * @param httpMethod http method (GET, POST, etc.)
   * @param bodyJSON optional json body to be sent
   * @return request initialization dictionary setup to do fetch
   */
  private def fetchOptions(httpMethod: HttpMethod, bodyJSON: Option[String] = None) = {
    val initHeaders = {
      List("Authorization" -> s"Bearer ${LoginInfo.LoginResponse.now().getOrElse(LoginInfo.EmptyLogin).token}",
        "Accept" -> "application/json")
    }
    val httpHeaders =
      if (bodyJSON.isDefined)
        "Content-Type" -> "application/json" :: initHeaders
      else
        initHeaders

    new RequestInit {
      /**
       * Get method
       */
      method = httpMethod
      // mode = RequestMode.`no-cors`
      //credentials = RequestCredentials.include //RequestCredentials.`same-origin`
      /**
       * These headers will be added to the request and tell the API that we're expecting JSON in return.
       */
      headers = js.Dictionary(httpHeaders: _*)
      body = bodyJSON match {
        case None => js.undefined
        case Some(b) => b
      }
    }
  }

  // Import execution context for futures
  import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  /**
   * Do request and return the JSON response as a string
   * @param url URL to submit request to
   * @param init initial data for request
   * @return request response
   */
  private def doRequest(url: String, init: RequestInit) =
    Fetch.fetch(info = url, init = init).toFuture // Make js promise into a future
      .flatMap(resp =>
        // If HTTP failure then returned failed status
        if (!HTTPCodes.isOK(resp.status))
          Future.failed(new Exception(
            s"HTTP request for $url failed (code ${resp.status.toString} - ${HTTPCodes.getCodeText(resp.status)._1})"
          ))
        else {
          // Read response stream till done and then convert json to a string
          resp.json().toFuture.map(js.JSON.stringify(_))
        }
      )
}

