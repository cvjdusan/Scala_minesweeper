ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    name := "ScalaFX-Demo",
    libraryDependencies += "org.scalafx" %% "scalafx" % "16.0.0-R24",
      libraryDependencies ++= {
      // Determine OS version of JavaFX binaries
      lazy val osName = System.getProperty("os.name") match {
        case n if n.startsWith("Linux") => "linux"
        case n if n.startsWith("Mac") => "mac"
        case n if n.startsWith("Windows") => "win"
        case _ => throw new Exception("Unknown platform!")
      }
      Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
        .map(m => "org.openjfx" % s"javafx-$m" % "16" classifier osName)
    }
  )

javaOptions ++= Seq(
  "--module-path", "C:/javafx-sdk/javafx-sdk-21.0.5/lib",
  "--add-modules", "javafx.controls,javafx.fxml"
)
