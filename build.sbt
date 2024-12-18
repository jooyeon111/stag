// See README.md for license details.

ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "com.github.jooyeon111"

val chiselVersion = "6.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "stag",
    libraryDependencies ++= Seq(
      "org.chipsalliance" %% "chisel" % chiselVersion,
      "org.scalatest" %% "scalatest" % "3.2.16" % "test",
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-Ymacro-annotations",
    ),
    addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full),


    Test / test := {
      throw new MessageOnlyException(
        """
          |ERROR: Direct test execution is disabled.
          |Please use Makefile
          |""".stripMargin
      )
    },

    Test / testOnly := {
      throw new MessageOnlyException(
        """
          |ERROR: Direct test execution is disabled.
          |Please use Makefile
          |""".stripMargin
      )
    }

  )
