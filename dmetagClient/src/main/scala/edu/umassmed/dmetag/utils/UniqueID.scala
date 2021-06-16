package edu.umassmed.dmetag.utils

import scala.annotation.tailrec
import scala.util.Random
import java.util.UUID

/**
 * Utilities to create unique IDs.
 */
object UniqueID {
  lazy private val randomChar = Random.alphanumeric.iterator

  /**
   * Get random string of specified length
   * @param length length of string
   * @param acc accumulator of string made so far for recursive calls
   * @return random string
   */
  @tailrec
  private def getRandomString(length: Int, acc: String = ""): String = {
    require(length >= 0, message = "length needs to be non-negative")
    if (length == 0) acc
    else getRandomString(length - 1, acc + randomChar.next().toString)
  }

  /**
   * Get local unique ID (key_millisecondTime_random5characterString)
   * @param key string to put at start of ID
   * @return unique ID for local node
   */
  def getLocalID(key: String): String = {
    val t = System.currentTimeMillis().toHexString
    s"${key}_${t}_${getRandomString(5)}"
  }

  /**
   * Get random universally unique identifier (UUID)
   * @return string representation of random UUID
   */
  def getUUID: String = UUID.randomUUID().toString
}
