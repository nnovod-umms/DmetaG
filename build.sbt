import sbt.internal.util.ManagedLogger

lazy val scalaVer = "2.13.4"
lazy val akkaVersion = "2.6.10"
lazy val akkaHttpVersion = "10.2.2"
lazy val scalaTagsVersion = "0.9.2"
lazy val scalaJSVersion = "1.5.1"
lazy val scalaJSDomVersion = "1.1.0"
lazy val scalaJSReactVersion = "1.7.6"
lazy val laminarVersion = "0.11.0"
lazy val airStreamVersion = "0.11.1"
lazy val uPickleVersion = "1.3.11"
lazy val uJsonVersion = "1.3.11"
lazy val scalaGraphVersion = "1.13.2"
lazy val mongoDriverVersion = "4.2.1"

// thisBuild used for settings to go across projects
ThisBuild / scalaVersion := scalaVer
ThisBuild / organization := "edu.umassmed"
ThisBuild / version := "0.1"

def akkaDeps = Seq(
  "com.typesafe.akka" %% "akka-stream" % akkaVersion withSources(),
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion withSources(),
  "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion withSources(),
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion withSources()
)

// Root project that aggregates subprojects so a single command at the root will be applied to all subprojects
lazy val root = (project in file("."))
  .aggregate(dmetagServer, dmetagClient)
  
lazy val dmetagServer = (project in file("dmetagServer"))
  .settings(
    name := "dmetagServer",
    libraryDependencies ++= akkaDeps,
    libraryDependencies += "com.lihaoyi" %% "scalatags" % scalaTagsVersion withSources(),
    libraryDependencies += "org.mongodb.scala" %% "mongo-scala-driver" % mongoDriverVersion withSources()
  )

// Define new tasks that copy over bundle file and sources to resource directory
val fastOptJSBundle = taskKey[Seq[File]]("fastOptJS::webjar followed by copy to resource directory")
val fullOptJSBundle = taskKey[Seq[File]]("fullOptJS::webjar followed by copy to resource directory")

/**
 * Rename the javascript bundle file to be just the module name and move it, the associated map, and sources over to
 * the resources directory after fixing up any pointers to files to point to the resource directories.
 * @param pack files created by bundle command
 * @param outDir resources directory
 * @param modName module name
 * @return javascript file we created
 */
def renameBundle(pack: Seq[Attributed[File]], outDir: File, modName: String, logger: ManagedLogger): Seq[File] = {
  // Get bundled file
  val bundle =
    pack.find(
      _.metadata.keys.exists(
        _.label == "bundlerFileType"
      )
    ).map(_.data)
  // Go copy the file to the resource directory (and fix sourcefile names in files)
  bundle match {
    case Some(file) =>
      Seq(JSUtils.copyJSout(outDir, file, modName, logger))
    case _ => Seq.empty[File]
  }
}


lazy val dmetagClient = (project in file("dmetagClient"))
  // Update bundler when it can support fastLinkJS with module splitting (put into scala.js 1.3)
  .enablePlugins(ScalaJSPlugin, ScalaJSBundlerPlugin)
  .settings(
    // Needed to get scala.js names from JSExport and JSExportTopLevel to actually get exported
    webpackBundlingMode := BundlingMode.LibraryAndApplication(),
    name := "dmetagClient",
    resolvers += "jitpack" at "https://jitpack.io", // For scala-js-d3v4
    libraryDependencies ++= Seq(
      // "%%%" means to append the Scala.JS specific platform suffix to the artifact id
      "com.raquo" %%% "laminar" % laminarVersion withSources(),
      "com.raquo" %%% "airstream" % airStreamVersion withSources(),
      "org.scala-js" %%% "scalajs-dom" % scalaJSDomVersion withSources(),
      "com.lihaoyi" %%% "upickle" % uPickleVersion withSources(),
// Is included with upickle      "com.lihaoyi" %%% "ujson" % uJsonVersion withSources(),
      "org.scala-graph" %%% "graph-core" % scalaGraphVersion withSources(),
      "com.github.fdietze.scala-js-d3v4" %%% "scala-js-d3v4" % "809f086" withSources()
    ),
    npmDependencies in Compile ++= Seq(
      "dagre" -> "0.8.5"
      //"@dagrejs/dagre" -> "0.8.0"
      //  "purecss" -> "2.0.3"
      //  "react" -> "17.0.1"
      //  "react-dom" -> "17.0.1"
    ),
    fullOptJSBundle := {
      renameBundle((Compile / fullOptJS / webpack).value, (dmetagServer / Compile / resourceDirectory).value,
        moduleName.value, streams.value.log)
    },
    fastOptJSBundle := {
      renameBundle((Compile / fastOptJS / webpack).value, (dmetagServer / Compile / resourceDirectory).value,
        moduleName.value, streams.value.log)
    }
  )


