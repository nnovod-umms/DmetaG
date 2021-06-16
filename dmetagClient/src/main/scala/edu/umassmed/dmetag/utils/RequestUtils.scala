package edu.umassmed.dmetag.utils

/**
 * Some useful methods to handle requests.
 */
object RequestUtils {

  /**
   * Get all failures in a list into one string.
   * @param completions list of Either completions
   * @return (string with failures concatenated - empty if no failures, list of right completions)
   */
  def getEitherLists[T](completions: List[Either[String, T]]): (String, List[T]) = {
    val (lefts, rights) =
      completions.foldLeft((List.empty[String], List.empty[T])) {
        case ((errsSoFar, completeSoFar), Left(err)) => (err :: errsSoFar, completeSoFar)
        case ((errsSoFar, completeSoFar), Right(data)) => (errsSoFar, data :: completeSoFar)
      }
    (lefts.mkString(", "), rights)
  }
}
