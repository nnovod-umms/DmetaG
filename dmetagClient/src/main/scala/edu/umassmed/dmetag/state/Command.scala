package edu.umassmed.dmetag.state

/**
 * Commands in main menu
 */
object Command extends Enumeration {
  type Command = Value
  val LOGIN = Value("login")
  val NEW = Value("new")
  val UPDATE = Value("update")
  val ABOUT = Value("about")
  val FIND = Value("find")
  val FIND_BARCODE = Value("findBarcode")
  val UNKNOWN = Value("unknown")

  /**
   * Get the description for a command.
   * @param command command
   * @return description of form
   */
  def commandDesc(command: Command): String =
    command match {
      case LOGIN => "Login"
      case NEW => "Add New Entry"
      case UPDATE => "Update Entry"
      case FIND => "Find Entry"
      case FIND_BARCODE => "Find Entry by Barcode"
      case ABOUT => "About"
      case UNKNOWN => "Unknown"
    }
}

