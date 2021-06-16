import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter}
import java.nio.file.{Files, Path, StandardCopyOption}

import sbt.internal.util.ManagedLogger

import scala.annotation.tailrec

/**
 * Utility functions for builds
 */
object Utils {
  // Set to true to not actually create any new files but simply print out (if logging set to debug level) info
  private val debug = false

  /**
   * Transfer (move or copy) a file.  It will overwrite the target file if it already exists.
   * @param in source file
   * @param out target file
   * @param oper operation description (e.g., "move" or "copy")
   * @param logger message logger
   * @param transferFunc function to do transfer
   * @return output path
   */
  private def transferFile(in: File, out: File, oper: String, logger: ManagedLogger,
                           transferFunc: (Path, Path, StandardCopyOption) => Path) = {
    val inPath = in.toPath
    val outPath = out.toPath
    logger.debug(s"$oper of $inPath to $outPath")
    if (debug) {
      outPath
    } else {
      // Make sure directories exist before transferring file
      Files.createDirectories(out.getParentFile.toPath)
      transferFunc(inPath, outPath, StandardCopyOption.REPLACE_EXISTING)
    }
  }

  /**
   * Copy a file, overwriting any preexisting target file
   * @param in source file
   * @param out target file
   * @param logger message logger
   */
  def copyFile(in: File, out: File, logger: ManagedLogger): Path =
    transferFile(in, out, "copy", logger, Files.copy(_, _, _))

  /**
   * Move a file, overwriting any preexisting target file
   * @param in source file
   * @param out target file
   * @param logger message logger
   */
  def moveFile(in: File, out: File, logger: ManagedLogger): Path =
    transferFile(in, out, "move", logger, Files.move(_, _, _))

  /**
   * Function to execute code for an item (e.g., file) that should be closed when the code completes.
   * When the code to be executed completes the associated item is closed, regardless of whether or not
   * there is an error.
   * @param closable item that is closable (it must contain a close() function)
   * @param exec code to be executed for item
   * @tparam T type of item to be worked on (it must minimally contain a close() function)
   * @tparam O type of output from code black
   * @return code block return
   */
  private def use[T <: { def close(): Unit }, O](closable: T)(exec: T => O): O = {
    try {
      exec(closable)
    }
    finally {
      closable.close()
    }
  }

  /**
   * Replace a string in a file.  After doing the replacement into a temporary file we check if the expected number
   * of replacements took place.  If yes, then we overwrite the original file with the one created with replaced strings.
   * @param orig original file
   * @param fromStr string to be replaced
   * @param preReplace callback to do caller specific replacement in string
   * @param toStr string to substitute for replaced string
   * @param logger message logger
   * @param numReplacements optional # of replacements expected, if not specified original file is always replaced
   */
  def doReplace(origIn: File, orig: File, preReplace: Option[(String) => String],
                fromStr: String, toStr: String, logger: ManagedLogger,
                numReplacements: Option[Int] = None): Unit = {
    val origFileName = orig.getCanonicalPath()
    val replacementFileName = s"$origFileName.rep"
    val replaceCnt = copyAndReplace(from = origFileName, to = replacementFileName, preReplace = preReplace,
      fromStr = fromStr, toStr = toStr, logger)
    if (numReplacements.isEmpty || replaceCnt == numReplacements.get) {
      moveFile(new File(replacementFileName), orig, logger)
    } else
      logger.error(s"Unexpected number of replacements in $origFileName ($replaceCnt)")
  }

  /**
   * Copy one file to another replacing all occurrences of a string with a new one.  It can also be done with
   * sed or awk but getting those to work via os.process.run was impossible.
   * @param from input file
   * @param to output file
   * @param preReplace callback to do caller specific replacement in string
   * @param fromStr input string to replace
   * @param toStr output string to substitute
   * @param logger message logger
   * @return # of times string replacement takes place
   */
  private def copyAndReplace(from: String, to: String, preReplace: Option[(String) => String],
                             fromStr: String, toStr: String, logger: ManagedLogger): Int = {
    logger.debug(s"copying $from to $to replacing occurrences of $fromStr with $toStr")
    if (debug) {
      1
    } else
      use(new BufferedReader(new FileReader(from))) { in =>
        use(new BufferedWriter(new FileWriter(to))) { out =>

          // Loop to keep track of number of times we replace the string
          @tailrec
          def loop(cnt: Int): Int = {
            // Get next line - if none there we're done, otherwise get line and do replacement if needed.
            val next = in.readLine()
            if (next == null) {
              cnt // All done - exit with total # of replacements
            }
            else {
              // Do preliminary replacement if specified
              val str = preReplace match {
                case Some(func) => func(next)
                case _ => next
              }
              // Do replacement - a bit inefficient if replacement is actually done (goes over string twice) but it's
              // assumed that replacements do not occur most of the time.  Also the contains call is needed to keep
              // count of replacements.
              val replaceCnt =
              if (!str.contains(fromStr)) {
                // No need to replace string
                out.write(str)
                0
              } else {
                // String needs to be replaced
                out.write(str.replace(fromStr, toStr))
                1
              }
              out.newLine()
              loop(cnt + replaceCnt)
            }
          }

          loop(0)
        }
      }
  }
}
