package edu.umassmed.dmetag.display

import com.raquo.laminar.api._
import com.raquo.laminar.keys.ReactiveHtmlAttr
import com.raquo.laminar.nodes.ReactiveHtmlElement
import com.raquo.domtypes.generic.codecs.StringAsIsCodec
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.html.{Div, Label, OptGroup, Select}

/**
 * Methods to make html elements for forms.
 */
object HtmlFormElements {
  // Label attribute not included in standard Laminar (note - this is an attribute, not an element which L.label is)
  private lazy val labelAttr: ReactiveHtmlAttr[String] =
    new ReactiveHtmlAttr[String]("label", StringAsIsCodec)

  /**
   * Create reactive password field.
   * @param name id for field
   * @param label label for field
   * @param required true if input required
   * @return div containing label and password input box
   */
  def formPassword(name: String, label: String, required: Boolean)
  : ReactiveHtmlElement[Div] =
    formInput(name = name, label = label, `type` = "password",
      isRequired = required, defaultValue = None, readOnly = false)

  /**
   * Create reactive HTML text input box.
   * @param name id for text input box
   * @param label label for text input box
   * @param required true if input required
   * @param defaultValue optional default value for input
   * @param readOnly is field read only?
   * @param hidden is field hidden?
   * @return div containing label and text input box
   */
  def formTextInput(name: String, label: String,
                    required: Boolean = false, defaultValue: Option[String] = None, readOnly: Boolean = false,
                    hidden: Boolean = false)
  : ReactiveHtmlElement[Div] =
    formInput(name = name, label = label, `type` = "text", isRequired = required, defaultValue = defaultValue,
      readOnly = readOnly, hidden = hidden)

  /**
   * Create reactive HTML text input box.
   * @param name id for text input box
   * @param label label for text input box
   * @param isRequired true if input required
   * @param defaultValue optional default value for input
   * @param readOnly is field read only
   * @param hidden is field hidden?
   * @return input box and div containing label and text input box
   */
  def formTextInputAndDiv(name: String, label: String,
                          isRequired: Boolean = false, defaultValue: Option[String] = None,
                          readOnly: Boolean = false, hidden: Boolean = false)
  : (ReactiveHtmlElement[dom.html.Input], ReactiveHtmlElement[dom.html.Div]) =
    formInputAndEle(name = name, label = label, `type` = "text", isRequired = isRequired, defaultValue = defaultValue,
      readOnly = readOnly, hidden = hidden)

  /**
   * Create reactive HTML numeric input box.
   * @param name id for numeric input box
   * @param label label for input box
   * @param isRequired true if input required
   * @param defaultValue optional default value for input
   * @param readOnly is field read only
   * @param hidden is field hidden?
   * @return div containing label and numeric input box
   */
  def formNumInput(name: String, label: String,
                   isRequired: Boolean = false, defaultValue: Option[String] = None,
                   readOnly: Boolean = false, hidden: Boolean = false)
  : ReactiveHtmlElement[Div] =
    formInput(name = name, label = label, `type` = "number", isRequired = isRequired, defaultValue = defaultValue,
      readOnly = readOnly, hidden = hidden)

  /**
   * Create reactive HTML time input box.
   * @param name id for time input box
   * @param label label for input box
   * @param isRequired true if input required
   * @param defaultValue optional default value for input
   * @param readOnly is field read only
   * @param hidden is field hidden?
   * @return div containing label and time input box
   */
  def formTimeInput(name: String, label: String,
                    isRequired: Boolean = false, defaultValue: Option[String] = None,
                    readOnly: Boolean = false, hidden: Boolean = false)
  : ReactiveHtmlElement[Div] =
    formInput(name = name, label = label, `type` = "time", isRequired = isRequired,
      defaultValue = defaultValue, readOnly = readOnly, hidden = hidden)

  /**
   * Create reactive HTML input box.
   * @param name id for input box
   * @param label label for input box
   * @param `type` input type
   * @param isRequired true if input required
   * @param defaultValue optional default value for input
   * @param readOnly is field read only?
   * @param hidden is field hidden?
   * @return div containing label and input box
   */
  private def formInput(name: String, label: String, `type`: String,
                        isRequired: Boolean, defaultValue: Option[String], readOnly: Boolean,
                        hidden: Boolean = false) =
    formInputAndEle(name = name, label = label, `type` = `type`, isRequired = isRequired, defaultValue = defaultValue,
      readOnly = readOnly, hidden = hidden)._2

  /**
   * Create reactive HTML input box.
   * @param name id for input box
   * @param label label for input box
   * @param `type` input type
   * @param isRequired true if input required
   * @param defaultValue optional default value for input
   * @param readOnly is field readonly?
   * @param hidden is field hidden?
   * @return (input box, div containing label and numeric input box)
   */
  private def formInputAndEle(name: String, label: String, `type`: String,
                              isRequired: Boolean, defaultValue: Option[String],
                              readOnly: Boolean, hidden: Boolean = false)
  : (ReactiveHtmlElement[dom.html.Input], ReactiveHtmlElement[dom.html.Div]) = {
    val attrList = List(L.`type` := `type`, L.idAttr := name, L.name := name,
      L.required := isRequired, L.hidden := hidden, L.readOnly := readOnly)
    val aList = defaultValue match {
      case Some(value) => (L.value := value) :: attrList
      case None => attrList
    }
    val ele = L.input(aList)
    val div = fieldWithLabel(label, name, ele, hidden)
    (ele, div)
  }

  /**
   * Create selection list from enumeration.  Initial selection is set to default value if one specified, otherwise
   * set to heading if one specified, otherwise set to first value in list.
   * @param enum enumeration to make into select list
   * @param optionHeading optional initial choice shown in selection list
   * @param name name for selection list
   * @param label label for selection list
   * @param required required input?
   * @param default optional initial value for list
   * @param exclude items to exclude from list
   * @param hidden is field hidden?
   * @tparam T enumeration type
   * @return (select element containing options, division containing select element)
   */
  def enumSelectAndDiv[T <: scala.Enumeration](enum: T, optionHeading: Option[String],
                                               name: String, label: String,
                                               required: Boolean, default: Option[String], exclude: List[String],
                                               hidden: Boolean = false)
  : (ReactiveHtmlElement[Select], ReactiveHtmlElement[Div]) =
    formSelectAndDiv(
      options = enum.values.map(_.toString).toList.filterNot(exclude.contains).map((None, _)),
      optionHeading = optionHeading, name = name, label = label,
      required = required, default = default, hidden = hidden
    )

  /**
   * Create selection list from list of values.  Initial selection is set to default value if one specified, otherwise
   * set to heading if one specified, otherwise set to first value in list.
   * @param options options to make into select list (optional value, display value)
   * @param optionHeading optional initial choice shown in selection list
   * @param name name for selection list
   * @param label label for selection list
   * @param required required input?
   * @param default optional initial value for list
   * @param disable disable non-matched picks?
   * @param hidden is field hidden?
   * @return division containing select element
   */
  def formSelect(options: List[(Option[String], String)], optionHeading: Option[String],
                 name: String, label: String, required: Boolean, default: Option[String],
                 disable: Boolean = false, hidden: Boolean = false)
  : ReactiveHtmlElement[Div] =
    formSelectAndDiv(
      options = options, optionHeading = optionHeading, name = name, label = label,
      required = required, default = default, disable = disable, hidden = hidden
    )._2

  /**
   * Create selection list from list of values.  Initial selection is set to default value if one specified, otherwise
   * set to heading if one specified, otherwise set to first value in list.
   * @param options options to make into select list (optional value, displayed value)
   * @param optionHeading optional initial choice shown in selection list
   * @param name name for selection list
   * @param label label for selection list
   * @param required required input?
   * @param default optional initial value for list
   * @param disable disable non-matched picks?
   * @param hidden is field hidden?
   * @return (select element containing options, division containing select element)
   */
  def formSelectAndDiv(options: List[(Option[String], String)], optionHeading: Option[String],
                       name: String, label: String,
                       required: Boolean, default: Option[String], disable: Boolean = false, hidden: Boolean = false)
  : (ReactiveHtmlElement[Select], ReactiveHtmlElement[Div]) = {
    // Get if default matches one of options and list of options (in same order as enumeration)
    val (foundMatch, entryOptions) =
      options.foldRight((false, List.empty[ReactiveHtmlElement[html.Option]])) {
        case (next, (matched, soFar)) =>
          val (nextValue, nextDisplay) = next
          val nextMatch = default.isDefined &&
            default.get.equals(if (nextValue.isDefined) nextValue.get else nextDisplay)
          (nextMatch || matched, option(nextDisplay, nextMatch, nextValue) :: soFar)
      }
    // Disable non-matched picks if desired
    if (foundMatch && disable) {
      entryOptions.map(entry => {
        if (entry.ref.value != default.get)
          entry.amend(L.disabled := true)
      })
    }
    // Create selection list with heading
    val (sel, div) = selectAndDiv(name = name, label = label, required = required,
      options = optionHeading match {
        // Create reactive option with description that shows up first in selection drop down.  It can not be chosen,
        // it's simply a description of what needs to be chosen and shows up as the top choice when the selection list
        // is first displayed.  Also, the value of the option is an empty string so resetting the selection to "" resets
        // the option list to the description.
        case Some(heading) =>
          List(L.option(L.value(""), L.disabled := true, L.selected := !foundMatch, heading))
        case None => List.empty[ReactiveHtmlElement[html.Option]]
      }, hidden = hidden
    )
    // Add options to select, and return div and select
    sel.amend(entryOptions)
    (sel, div)
  }

  /**
   * Create reactive selection option.
   * @param name selection description
   * @param selected is this initial selection for list?
   * @param value optional value if to be different than name
   * @return selection option
   */
  def option(name: String, selected: Boolean, value: Option[String] = None): ReactiveHtmlElement[html.Option] =
    L.option(L.value(value.getOrElse(name)), L.selected := selected, name)

  /**
   * Create reactive HTML select element with a label.
   * @param name element id/name
   * @param label element label
   * @param required required input?
   * @param options select options
   * @param hidden is field hidden?
   * @return div containing label and select element which contains specified options, select element within div
   */
  def selectAndDiv(name: String, label: String, required: Boolean,
                   options: List[ReactiveHtmlElement[dom.raw.HTMLElement]], hidden: Boolean = false)
  : (ReactiveHtmlElement[dom.html.Select], ReactiveHtmlElement[dom.html.Div]) = {
    val selectElement = L.select(L.name := name, L.idAttr := name, L.required := required, L.hidden := hidden, options)
    val div = fieldWithLabel(label, name, selectElement, hidden)
    (selectElement, div)
  }

  /**
   * Create reactive HTML options group containing specified select options.
   * @param label label for options group
   * @param options select options
   * @return options group with specified title and options
   */
  def optGroup(label: String, options: ReactiveHtmlElement[dom.raw.HTMLElement]*)
  : ReactiveHtmlElement[OptGroup] =
    L.optGroup(labelAttr(label), options)

  /**
   * Create reactive HTML text input box.
   * @param name input box id/name
   * @param required true if input required
   * @param defaultValue optional default value for input
   * @param hidden is field hidden?
   * @return input box
   */
  def formTextAreaInput(name: String, label: String,
                        required: Boolean, defaultValue: Option[String], hidden: Boolean = false)
  : ReactiveHtmlElement[Div] = {
    val initValue = defaultValue match {
      case Some(value) => value
      case None => ""
    }
    val textArea = L.textArea(initValue, L.idAttr := name, L.name := name, L.required := required, L.hidden := hidden)
    fieldWithLabel(label, name, textArea, hidden)
  }

  /**
   * Date input field - needs placeholder and pattern for Safari which doesn't support type date.
   * @param name name of date field
   * @param label label for date input field
   * @param isRequired is field required input?
   * @param defaultValue optional default value for input
   * @param hidden is field hidden?
   * @return division containing date input field
   */
  def formDateInput(name: String, label: String, isRequired: Boolean = false,
                    defaultValue: Option[String] = None, hidden: Boolean = false)
  : ReactiveHtmlElement[Div] = {
    val initialAttrs = List(L.`type` := "date", L.placeholder := "mm/dd/yyyy",
      // Pattern only needed for Safari which doesn't have date handling
      // ddmm pattern if needed...
      //val ddmm = L.pattern := """(^(((0[1-9]|1[0-9]|2[0-8])[\/](0[1-9]|1[012]))|""" +
      //  """((29|30|31)[\/](0[13578]|1[02]))|""" +
      //  """((29|30)[\/](0[4,6,9]|11)))[\/](19|[2-9][0-9])\d\d$)|""" +
      //  """(^29[\/]02[\/]""" +
      //  """(19|[2-9][0-9])(00|04|08|12|16|20|24|28|32|36|40|44|48|52|56|60|64|68|72|76|80|84|88|92|96)$)"""
      // mmdd pattern
      L.pattern := """(^(((0[1-9]|1[012])[\/](0[1-9]|1[0-9]|2[0-8]))|""" + // mm/1-28
        """((0[13578]|1[02])[\/](29|30|31))|""" + // Jan|Mar|May|July|Aug/Oct/Dec/29-31
        """((0[4,6,9]|11)[\/](29|30)))[\/]""" + // Apr/Jun/Sept/Nov/29-30
        """(19|[2-9][0-9])\d\d$)|""" + // yyyy
        """(^02[\/]29[\/]""" + // Feb 29 (leap year) - yea, it's wrong for end-of-century years not divisible by 400
        """(19|[2-9][0-9])(00|04|08|12|16|20|24|28|32|36|40|44|48|52|56|60|64|68|72|76|80|84|88|92|96)$)""",
      L.idAttr := name, L.name := name, L.required := isRequired, L.hidden := hidden)
    // Add default value if one present
    val inputAttrs = defaultValue match {
      case Some(value) =>
        (L.value := value) :: initialAttrs
      case None => initialAttrs
    }
    fieldWithLabel(label, name, L.input(inputAttrs), hidden)
  }

  /**
   * Make div with field and label.  If field is hidden the label is left out.
   * @param label label text
   * @param name name of field
   * @param input input field
   * @param hidden true if field is to be hidden.
   * @tparam T type of input
   * @return div with field and label if field is not optional
   */
  private def fieldWithLabel[T <: html.Element](label: String, name: String,
                                                input: ReactiveHtmlElement[T], hidden: Boolean)
  : ReactiveHtmlElement[Div] = {
    if (hidden)
      L.div(L.className := "pure-control-group", input)
    else
      L.div(L.className := "pure-control-group", htmlLabel(label, name), input)
  }

  /**
   * Create reactive HTML label element.
   * @param label label for associated HTML element
   * @param name id of associated element
   * @return reactive label element
   */
  private def htmlLabel(label: String, name: String): ReactiveHtmlElement[Label] =
    L.label(L.forId := name, s"$label: ")
}
