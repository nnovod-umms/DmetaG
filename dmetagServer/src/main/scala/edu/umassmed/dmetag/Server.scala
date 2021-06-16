package edu.umassmed.dmetag

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import edu.umassmed.dmetag.directives.MultiPartDirective._
import edu.umassmed.dmetag.directives.PartsAndFiles

// To get implicit (un)marshalling for JsValue
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import scalatags.Text
import spray.json.JsValue

import scala.concurrent.ExecutionContextExecutor
import scala.io.StdIn

//import akka.http.scaladsl.model.StatusCodes

// Based on HttpServerRoutingMinimal from https://doc.akka.io/docs/akka-http/current/introduction.html
object Server {
  val host = "localhost"
  val port = 8080

  def main(args: Array[String]): Unit = {

    implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "my-system")
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext: ExecutionContextExecutor = system.executionContext

    def getGet(page: Text.all.doctype) = {
      get {
        complete(
          HttpEntity(
            ContentTypes.`text/html(UTF-8)`,
            page.render
          )
        )
      }
    }

    val route =
      path("dmetag") {
        getGet(Pages.dMetaGPage)
      } ~
        path("form") { /* Test path for receiving and sending back json */
          // Form made into json on client - just return json for now to show we got it
          post {
            extractRequest { req =>
              entity(as[JsValue]) { value =>
                complete(OK, req.headers, value)
              }
            }
          }
        } ~
        path("form1") { /* Alternate test path to receive and send back json and files */
          // form with fields and files
          formAndFiles {
            case PartsAndFiles(fields, files) =>
              //files.foreach(_._2.delete())
              val body = s"""
                            |File: ${files}
                            |Form: ${fields}
        """.stripMargin
              complete(OK, body)
          }

        } ~
        pathPrefix("resource") { /* Get resource files */
          extractUnmatchedPath { unmatched =>
            val resourceStr = unmatched.toString()
            // Resource must be prefix before "/"
            if (!resourceStr.startsWith("/"))
              reject()
            else {
              // Get actual resource name by taking out starting "/"
              getFromResource(resourceStr.substring(1))
            }
          }
        }

    val bindingFuture = Http().newServerAt(host, port).bind(route)

    println(s"Server online at http://$host:$port/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
