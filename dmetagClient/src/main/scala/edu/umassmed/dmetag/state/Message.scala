package edu.umassmed.dmetag.state

/**
 * Messages for stream to display messages
 */
object Message {
  /**
   * Colors for messages
   */
  object MsgColor extends Enumeration {
    type MsgColor = Value
    val INFO = Value("green")
    val WARN = Value("darkgoldenrod")
    val ERR = Value("red")
  }

  /**
   * Base for messages
   */
  sealed trait Message

  /**
   * No message to display
   */
  case object NoMsg extends Message

  /**
   * Message to display
   * @param tMsg message text
   * @param tMsgColor message text display color
   */
  sealed abstract class TextMessage(val tMsg: String, val tMsgColor: MsgColor.Value) extends Message

  /**
   * Informational message to display
   * @param iMsg message text
   */
  case class InfoMsg(iMsg: String) extends TextMessage(iMsg, MsgColor.INFO)

  /**
   * Warning message to display
   * @param wMsg message text
   */
  case class WarnMsg(wMsg: String) extends TextMessage(wMsg, MsgColor.WARN)

  /**
   * Error message to display
   * @param eMsg message text
   */
  case class ErrMsg(eMsg: String) extends TextMessage(eMsg, MsgColor.ERR)
}

