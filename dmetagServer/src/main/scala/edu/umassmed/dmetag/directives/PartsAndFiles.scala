package edu.umassmed.dmetag.directives

import scala.collection.immutable
import akka.http.scaladsl.server.directives.FileInfo
import java.io.File

/**
 * Output of a parsing of a form into non-binary input fields and files.
 *
 * @see [[edu.umassmed.dmetag.directives.MultiPartDirective]]
 * @param form non-binary input fields in form
 * @param files (form file info, file uploaded to server)
 */
case class PartsAndFiles(form: immutable.Map[String, List[String]], files: immutable.Seq[(FileInfo, Option[File])]) {
  /**
   * Add a name/value to form's fields.  If there's already a value for the field the new value is added to the start
   * of the list.
   * @param fieldName field name
   * @param content content of field
   * @return new PartsAndFiles including the added name/value
   */
  def addForm(fieldName: String, content: String): PartsAndFiles =
    this.copy(
      form = {
        val existingContent: List[String] = this.form.getOrElse(fieldName, List.empty)
        val newContents: List[String] = content :: existingContent

        this.form + (fieldName -> newContents)
      }
    )
  def addFile(info: FileInfo, file: Option[File]): PartsAndFiles = this.copy(
    files = this.files :+ ((info, file))
  )
}

/**
 * Companion object
 * @see [[PartsAndFiles]]
 */
object PartsAndFiles {
  // object with no parts or files
  val Empty = PartsAndFiles(immutable.Map.empty, immutable.Seq.empty)
}

