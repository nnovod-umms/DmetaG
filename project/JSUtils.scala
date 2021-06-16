import java.io.File
import java.nio.file.Path

import Utils.{copyFile, doReplace}
import sbt.internal.util.ManagedLogger

import scala.annotation.tailrec

/**
 * Utility functions for javascript builds. Used to copy over generated javascript files and maps to resources area.
 * ?? Could plugin https://github.com/sbt/sbt-web be used instead ??
 */
object JSUtils {
  /**
   * Copy JS output from destination directories, where they are named strangely, to resources
   * directory with names moduleName.js[.map].  Besides moving the files we need to change the pointers from one to
   * the other (e.g., js file points to js.map file) inside the files. We also copy local source files, named in the
   * map file, to the resource directory under the subdirectory modNameSources. Pointers to the source files in the
   * map file are changed to point to where the sources are moved in the resource directory. This is all done to make
   * it easy to debug the created javascript using the source files.
   * @param resourceDir resource directory
   * @param inJSFile JS file initially created
   * @param modName module name
   * @param logger message logger
   */
  def copyJSout(resourceDir: File, inJSFile: File, modName: String, logger: ManagedLogger): File = {
    // Copy js file
    val outJSFileName = s"$modName.js"
    val outJSFile = new File(s"$resourceDir/$outJSFileName")
    copyFile(inJSFile, outJSFile, logger)
    // Copy map file
    val inMapFileName = s"${inJSFile.getCanonicalFile}.map"
    val inMapFile = new File(inMapFileName)
    val outMapFileName = s"$outJSFileName.map"
    val outMapFile = new File(s"$resourceDir/$outMapFileName")
    copyFile(inMapFile, outMapFile, logger)
    // Replace pointer to map file inside js file
    doReplace(origIn = inJSFile, orig = outJSFile, preReplace = None,
      fromStr = s"${inMapFile.getName}", toStr = s"${outMapFile.getName}", logger, numReplacements = Some(1))
    // Replace pointers to js file and local source files inside map file and copy local source files to resource area
    val sourcesDir = s"${modName}Sources"
    doReplace(origIn = inJSFile, orig = outMapFile,
      preReplace = Some(
        moveSources(_, origDir = inJSFile.getParentFile, newDir = new File(resourceDir, sourcesDir),
          newPreface = s"/resource/$sourcesDir/", copyFile = (origFile, newFile) => copyFile(origFile, newFile, logger)
        )
      ),
      fromStr = s"${inJSFile.getName}", toStr = s"${outJSFile.getName}", logger, numReplacements = Some(1)
    )
    outJSFile
  }

  /**
   * Get offset to where directory portion of strings no longer match
   * @param str1 first string
   * @param str2 second string
   * @param soFar # of characters that have matched so far
   * @return # of characters that match
   */
  @tailrec
  private def chkMatch(str1: String, str2: String, soFar: Int = 0): Int = {
    // Get number of characters to skip to get past next subdirectory
    val matchLen = {
      (str1.indexOf('/'), str2.indexOf('/')) match {
        // No more directories
        case (-1, -1) => 0
        // Next 2 cases to check if all that's left is <dirName> vs. <dirName>/...
        case (-1, idx) =>
          if (str1 == str2.substring(0, idx)) idx else 0
        case (idx, -1) =>
          if (str2 == str1.substring(0, idx)) idx else 0
        // Just look if next part of both strings up to and including "/" match
        case (idx1, idx2) =>
          if (str1.substring(0, idx1) == str2.substring(0, idx2)) idx1 + 1 else 0
      }
    }
    // If no match found then exit with # of characters matched so far, otherwise recurse to find more
    if (matchLen == 0)
      soFar
    else
      chkMatch(str1.substring(matchLen), str2.substring(matchLen), soFar + matchLen)
  }

  // Regular expression to find sources in line: <stuff>"sources":["spec1", "spec2", ...]<stuff>
  private val sourcesR = """(.*)"sources":\[([^\]]*)\](.*)""".r

  /**
   * Go through sources in a map file and change any that are in one local directory to be in another - usually we'll be
   * moving the source files from their original src location to one in the application's resources area.
   * @param inLine line from map file containing list of source files
   * @param origDir root directory from which source files listed in map file are relative
   * @param newDir new root directory to which to copy source files
   * @param newPreface new preface to put in front of new relative source file path
   * @param copyFile callback to copy file to new location
   * @return new line to put in map file with updated source file paths
   */
  private def moveSources(inLine: String, origDir: File, newDir: File,
                          newPreface: String, copyFile: (File, File) => Path) = {
    inLine match {
      case sourcesR(pre, sources, post) =>
        // Split sources into individual specs
        val sourceList = sources.split(",")
        // replace specs with new paths relative to new directory to contain files
        val newSources =
          sourceList.map(src => {
            if (!src.startsWith("\"https:")) {
              // Get rid of quotes
              val srcSpec = src.replace("\"", "")
              // Find where file is based on root directory (it is set as relative to root directory)
              val srcFile = new File(origDir, srcSpec)
              // If file exists then copy over file to new directory area and point source specification to it
              if (srcFile.exists()) {
                // Find where root and source file paths deviate
                val fileName = srcFile.getCanonicalPath
                val rootName = origDir.getCanonicalPath
                val matchIndex = chkMatch(fileName, rootName)
                // Get relative path and find destination for file
                val relPath =
                  fileName.substring(matchIndex) match {
                    case path if path.startsWith("/") => path.substring(1)
                    case path => path
                  }
                val newFile = new File(newDir, relPath)
                // Copy over the file to the new destination
                copyFile(srcFile, newFile)
                // Set new source specification
                s""""$newPreface$relPath""""
              } else
                src // File couldn't be found
            } else
              src // File started with http
          })
        // Put it all back together again (with new sources)
        s"""$pre"sources":[${newSources.mkString(",")}]$post"""
      case _ =>
        inLine
    }
  }

}
