package edu.umassmed.dmetag.state

object ContextMenuCommand extends Enumeration {
  type ContextMenuCommand = Value
  val ADD_NEW = Value("addNew")
  val MOVE = Value("move")
  val DELETE = Value("delete")

  /**
   * Get the description for a command.
   * @param command command
   * @return description of form
   */
  def commandDesc(command: ContextMenuCommand): String =
    command match {
      case ADD_NEW => "Add new entry to workflow"
      case MOVE => "Move entry"
      case DELETE => "Delete entry"
    }

}
