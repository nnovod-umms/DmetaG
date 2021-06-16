package edu.umassmed.dmetag.directives

import akka.http.scaladsl.model.{ContentType, HttpEntity, Multipart}
import akka.http.scaladsl.server.Directive1
import akka.http.scaladsl.server.directives.FileInfo
import akka.stream.scaladsl._

import java.io.File
import java.nio.file.{Files, Paths}
import scala.concurrent.Future

import akka.http.scaladsl.server.directives.BasicDirectives._
import akka.http.scaladsl.server.directives.FutureDirectives._
import akka.http.scaladsl.server.directives.MarshallingDirectives._
/**
 * Based on https://davidfrancoeur.com/post/akka-http-multipart-form/
 *
 * The directive will look for parts that are files to be streamed on disk. Other parts, that are non-binary and
 * strict will be gathered in a Map[String, List[String]] similar to what FormFieldDirectives#formFieldMultiMap
 * returns.  All other parts will be discarded.
 *
 * POST /submit
 * Content-Type: multipart/form-data; boundary=------------------------boundary
 * --------------------------boundary
 * Content-Disposition: form-data; name="name"
 * Content-Type: text/plain; charset=UTF-8
 *
 * name_value
 * --------------------------boundary
 * Content-Disposition: form-data; name="version"
 * Content-Type: text/plain; charset=UTF-8
 *
 * version_value
 * --------------------------boundary
 * Content-Disposition: form-data; name="unannounced"; filename="dicarded.txt"
 * Content-Type: text/plain; charset=UTF-8
 *
 * discarded
 * --------------------------boundary
 * Content-Disposition: form-data; name="file"; filename="content.txt"
 * Content-Type: text/plain; charset=UTF-8
 *
 * file content
 * --------------------------boundary--
 *
 *
 * The map would be: Map("name" -> List("name_value"), "version" -> List("version_value"))
 * The list would be List(FileInfo("file", "content.txt", Content.Type.`text/plain`) ->
 *                        Some(File("TEMPDIR/file-TEMPNUM.txt")))
 *
 */
object MultiPartDirective {
  /**
   * Akka HTTP custom directive to take form and upload files and parse non-binary input into name/value pairs.
   * @return form contents along with information about uploaded files.
   */
  def formAndFiles : Directive1[PartsAndFiles] = {
    // Unmarshall data and make it into a new directive
    entity(as[Multipart.FormData]).flatMap { formData =>
      extractRequestContext.flatMap { ctx =>
        // Use system materializer to look at stream
        implicit val mat = ctx.materializer
        // Use system execution context for executing futures
        implicit val ec = ctx.executionContext

        val uploadingSink = {
          // Fold all the futures for processing the elements together
          Sink.foldAsync[PartsAndFiles, Multipart.FormData.BodyPart](PartsAndFiles.Empty) {
            (acc, part) => // acc has what's been done so far; part is next element to process
              // Little function to discard an element
              def discard(p: Multipart.FormData.BodyPart): Future[PartsAndFiles] = {
                p.entity.discardBytes()
                // Complete with what we've done so far
                Future.successful(acc)
              }

              // If a file then go upload it
              part.filename.map (fileName => {
                // Setup fileinfo to return
                val fileInfo = FileInfo(part.name, fileName, part.entity.contentType)
                // If no filename then simply return without upload
                if (fileName.isEmpty) {
                  Future.successful(acc.addFile(fileInfo, None))
                } else {
                  // Get original file extension
                  val filePath = Paths.get(fileName).getFileName
                  val extension = filePath.toString.split("\\.").last
                  // Create new temp file with form part name as preface and original file extension as suffix
                  val dest = new File(Files.createTempFile(s"${part.name}-", s".$extension}").toUri)
                  // Stream down file to temporary location and complete with (original fileinfo, file created)
                  part.entity.dataBytes.runWith(FileIO.toPath(dest.toPath)).map { _ =>
                    acc.addFile(fileInfo, Some(dest)) // Add to accumulator of what we've done
                  }
                }
              }) getOrElse {
                // Go check for non-file entry in file (note numbers, etc. come down as text)
                part.entity match {
                  case HttpEntity.Strict(ct, data) if ct.isInstanceOf[ContentType.NonBinary] =>
                    // Get string in proper character set
                    val charsetName = ct.asInstanceOf[ContentType.NonBinary].charset.nioCharset.name
                    val partContent = data.decodeString(charsetName)
                    // Complete with key->value found
                    Future.successful(acc.addForm(part.name, partContent))
                  case _ =>
                    // Unknown part - just ignore it
                    discard(part)
                }
              }
          }
        }

        // Setup sink to look over parts
        val uploadedF = formData.parts.runWith(uploadingSink)

        // Wait for completion
        onSuccess(uploadedF)
      }
    }
  }
}
