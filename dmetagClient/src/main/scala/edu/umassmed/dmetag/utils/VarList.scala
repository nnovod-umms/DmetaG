package edu.umassmed.dmetag.utils

import com.raquo.airstream.signal.StrictSignal
import com.raquo.laminar.api.L

/**
 * A list, maintained in a Laminar var (so it can be observed).
 * @tparam T type of element in list
 */
trait VarList[T] {
  // List of entries
  private lazy val varList = L.Var[List[T]](List.empty[T])

  /**
   * Method to determine if two list entries are equal - needs to be implemented by extending objects
   * @param one one entry
   * @param another another entry
   * @return true if the entries are equal
   */
  protected def isEqual(one: T, another: T): Boolean

  /**
   * Get stream to listen for list changes.
   * @return stream of list changes
   */
  def getVarSignal: StrictSignal[List[T]] = varList.signal

  /**
   * Empty list
   */
  def emptyList(): Unit = varList.set(List.empty[T])

  /**
   * Add an entry to the list. The add becomes a replace if the entry is already in the list.
   * @param entry entry to add to list
   * @param ifThere callback with entry found if a replace is being done
   */
  def addToList(entry: T, ifThere: Option[T => Unit] = None): Unit = {
    updateList(entry,
      doUpdate = {
        case (list, Some((listEntry, index))) =>
          doCallback(ifThere, listEntry)
          val dropList = list.patch(index, Nil, 1)
          entry :: dropList
        case (wbList, None) => entry :: wbList
      }
    )
  }

  /**
   * Remove an entry from the list.
   * @param entry entry to remove from list.
   * @param ifNotFound callback if entry not found
   */
  def delFromList(entry: T, ifNotFound: Option[T => Unit] = None): Unit = {
    updateList(entry,
      doUpdate = {
        case (list, Some((_, index))) =>
          list.patch(index, Nil, 1)
        case (list, None) =>
          doCallback(ifNotFound, entry)
          list
      }
    )
  }

  /**
   * Update list.
   * @param entry entry to update
   * @param doUpdate callback to do update, input is (list zipped up with indexes, matching entry found in list)
   */
  private def updateList(entry: T, doUpdate: (List[T], Option[(T, Int)]) => List[T]): Unit =
    varList.update(curList => {
      val listWithIndex = curList.zipWithIndex
      val entryWithIndex = listWithIndex.find {
        case (listEntry, _) => isEqual(listEntry, entry)
      }
      doUpdate(curList, entryWithIndex)
    })

  /**
   * Do callback if specified.
   * @param callback if specified, called back with entry
   * @param entry entry
   */
  private def doCallback(callback: Option[T => Unit], entry: T): Unit =
    callback match {
      case Some(callback) => callback(entry)
      case _ =>
    }

  /**
   * Get an entry currently in the list. Put it at the top of the list if found.
   * @param isFound callback to determine if entry is one wanted
   * @return entry, if found
   */
  protected def get(isFound: T => Boolean): Option[T] =
    varList.now().find(isFound) match {
      case Some(entryFound) =>
        addToList(entryFound)
        Some(entryFound)
      case None => None
    }
}
