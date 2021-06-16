package edu.umassmed.dmetag.display

import com.raquo.laminar.api._
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom
import org.scalajs.dom.html.FieldSet
import edu.umassmed.dmetag.dmeta.DMeta.FldDataEntry
import edu.umassmed.dmetag.js.JsNative.FormData
import edu.umassmed.dmetag.utils.Types.DBID

import scala.scalajs.js

/**
 * Known fieldsets and other tools for making forms.
 */
object Forms {
  // ids
  private val usernameID = "username"
  private val passwordID = "password"

  /**
   * Retrieve set of fields for login (username and password)
   * @return login fields
   */
  def loginFieldset: ReactiveHtmlElement[FieldSet] =
    L.fieldSet(
      L.legend("Login"),
      HtmlFormElements.formTextInput(name = usernameID, label = doCapLabel(usernameID), required = true),
      HtmlFormElements.formPassword(name = passwordID, label = doCapLabel(passwordID), required = true)
    )

  /**
   * Retrieve id to be searched across collections
   * @return id field name
   */
  def idFieldset(id: String): ReactiveHtmlElement[FieldSet] =
    L.fieldSet(
      L.legend(s"Find ${doCapLabel(id)}"),
      HtmlFormElements.formTextInput(name = id, label = doCapLabel(id), required = true)
    )

  /**
   * Get a submit button, formatted using PureCss
   * @return reactive div containing submit button
   */
  private def submitButton: L.Div =
    L.div(
      L.cls := "pure-controls",
      L.button("submit", L.`type` := "submit", L.cls := "pure-button pure-button-primary")
    )

  /**
   * Make a form element with specified fields and submit button set to post form to url (action) specified.
   * @param action url to submit form to
   * @param name form id
   * @param fields callback to get fieldset to set in form
   * @return reactive html form element with specified fields and appending submit button setup to do post
   */
  def makePostForm(action: String, name: String,
                   fields : => ReactiveHtmlElement[FieldSet]): L.FormElement = {
    makeSubmitForm(name, fields)
      .amend(L.method := "post", L.action := action)
  }

  /**
   * Make a form element with specified fields and submit button
   * @param name form id
   * @param fields callback to get fieldset to set in form
   * @return reactive html form element with specified fields and appended submit button
   */
  def makeSubmitForm(name: String,
                     fields : => ReactiveHtmlElement[FieldSet]): L.FormElement = {
    makePureForm(name, fields.amend(submitButton))
  }

  /**
   * Make a form, using Pure.css styling.
   * @param name id for form
   * @param fields fields to set in form
   * @return reactive form element with Pure.css styling containing input fields
   */
  def makePureForm(name: String, fields : => ReactiveHtmlElement[FieldSet]): L.FormElement =
    L.form(L.idAttr := name, L.cls := "pure-form", L.cls := "pure-form-aligned", fields)

  /**
   * Take field information returned from DMeta and make a set of HTML form elements from it.
   * @param fields DMeta field descriptions
   * @param label label for form
   * @param values values previously set for entry (fieldName -> fieldValue)
   * @param fkFields map of foreign key names to possible values found for foreign key
   * @param isRequired set required elements to required (if false nothing is set as required)
   * @return form elements
   */
  def makeFormElements(fields: List[FldDataEntry], label: String,
                       values: Map[String, Any], fkFields: Map[String, List[(DBID, Any)]],
                       isRequired: Boolean)
  : ReactiveHtmlElement[FieldSet] = {
    // Get map of foreign keys
    L.fieldSet(
      L.legend(doCapLabel(label)),
      fields.map(field => {
        val fieldName = field.name
        val label = field.label
        val hidden = field.hidden
        val default = values.get(fieldName).map(_.toString)
        // Note: Need to do something with required.msg?
        val required = isRequired && field.required.required
        field.`type`.toLowerCase() match {
          case "boolean" =>
            HtmlFormElements.formSelect(options = List((None, "true"), (None, "false")), optionHeading = Some("true or false"),
              name = fieldName, label = label, required = required, default = default, hidden = hidden)
          case "text" =>
            HtmlFormElements.formTextAreaInput(name = fieldName, label = label,
              required = required, defaultValue = default, hidden = hidden)
          case "string" if field.`enum`.isEmpty =>
            HtmlFormElements.formTextInput(name = fieldName, label = label,
              required = required, defaultValue = default, hidden = hidden)
          case "string" =>
            // Make drop down list of enumerations
            HtmlFormElements.formSelect(options = field.`enum`.map((None, _)), optionHeading = Some(s"Select $label"),
              name = fieldName, label = label,
              required = required, default = default, hidden = hidden)
          case "number" =>
            HtmlFormElements.formNumInput(name = fieldName, label = label,
              isRequired = required, defaultValue = default, hidden = hidden)
          case "date" =>
            HtmlFormElements.formDateInput(name = fieldName, label = label,
              isRequired = required, defaultValue = default, hidden = hidden)
          case "time" =>
            HtmlFormElements.formTimeInput(name = fieldName, label = label,
              isRequired = required, defaultValue = default, hidden = hidden)
          case "mixed" | "array" =>
            HtmlFormElements.formTextAreaInput(name = fieldName, label = label,
              required = required, defaultValue = default, hidden = hidden)
          // For foreign keys the value is actually a DBID that points off to the associated entry - go fetch possible
          // values from input map
          case "fk" | "mongoose.schema.objectid" =>
            val options = fkFields.getOrElse(fieldName, List.empty)
            // Make drop down list of options - note that if a default value is set formSelect will disable all
            // values except the one matching the default value
            HtmlFormElements.formSelect(
              options = options.map{
                case (dbID, display) => (Some(dbID), display.toString)
              },
              optionHeading = Some(s"Select $label"),
              name = fieldName, label = label, required = required, default = default, disable = true, hidden = hidden)
          case _ => throw new Exception(s"Unknown form element type: '${field.`type`}'")
        }
      })
    )
  }

  /**
   * Capitalize first letter in the label.
   * @param label initial label
   * @return label with wanted capitalization done
   */
  private def doCapLabel(label: String) =
    if (label.nonEmpty) label.substring(0, 1).toUpperCase + label.substring(1) else label

  /**
   * Create an input element to prompt for text and receive result in observer.
   * @param labelName name for label on text field
   * @param observer observer to react to enter key stroke
   * @return reactive div with input field to prompt for text with bound observer
   */
  def onEnterInput(labelName: String, observer: L.Observer[String]): L.Div = {
    // Get div and input element contained within div
    val (input, div) = HtmlFormElements.formTextInputAndDiv(name = labelName, label = labelName)
    // Add observer for when enter key pressed on input
    input.amend(L.autoFocus(true),
      L.inContext { thisNode =>
        // Enter key event
        // Note: mapTo below accepts parameter by-name, evaluating it on every enter key press
        L.onKeyPress.filter(_.keyCode == dom.ext.KeyCode.Enter)
          .mapTo(thisNode.ref.value).filter(_.nonEmpty) --> observer
      }
    )
    div
  }

  /**
   * Convert a form's entries to a map
   * @param form html form
   * @return map of entry names to values
   */
  def formToMap(form: dom.html.Form): Map[String, Any] = {
    val entries = formToJSIterable(form)
    val mapBuilder = Map.newBuilder[String, Any]
    entries.foreach(
      entry =>  mapBuilder += entry._1 -> entry._2
    )
    mapBuilder.result()
  }

  /**
   * Convert a form's entries to iterable of tuples (key -> value)
   * @param form html form
   * @return iterable of entry names to values
   */
  def formToJSIterable(form: dom.html.Form): js.Iterable[js.Tuple2[String, Any]] =
    new FormData(form).entries()

  /**
   * Get selection list for commands that work on workbench entries.
   * @param options options to make into select list (optional value, display value)
   * @return (reactive select element containing options, reactive division containing select element)
   */
  def entrySelectAndDiv(options: List[(Option[String], String)]): (L.Select, L.Div) =
    HtmlFormElements.formSelectAndDiv(options = options, optionHeading = Some("Select Entry Type"),
      name = "wbType", label = "Type", required = true, default = None)
}
