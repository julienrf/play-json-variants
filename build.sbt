import com.typesafe.tools.mima.core.{IncompatibleSignatureProblem, ProblemFilters}
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

ThisBuild / organization := "org.julienrf"

ThisBuild / scalaVersion := "3.3.3"

ThisBuild / crossScalaVersions := Seq(scalaVersion.value, "2.13.15", "2.12.20")

ThisBuild / versionPolicyIntention := Compatibility.None

ThisBuild / version := "11.0.0-local"

ThisBuild / mimaBinaryIssueFilters ++= Seq(
  // package private method
  ProblemFilters.exclude[IncompatibleSignatureProblem]("julienrf.json.derived.DerivedOWritesUtil.makeCoProductOWrites")
)

ThisBuild / developers := List(
  Developer(
    "julienrf",
    "Julien Richard-Foy",
    "julien@richard-foy.fr",
    url("http://julien.richard-foy.fr")
  )
)
ThisBuild / homepage := Some(url(s"https://github.com/julienrf/play-json-derived-codecs"))
ThisBuild / licenses := Seq("MIT License" -> url("http://opensource.org/licenses/mit-license.php"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url(s"https://github.com/julienrf/play-json-derived-codecs"),
    s"scm:git:git@github.com:julienrf/play-json-derived-codecs.git"
  )
)


val library =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(
      name := "play-json-derived-codecs",
      libraryDependencies ++= Seq(
        "org.scalatest" %%% "scalatest" % "3.2.19" % Test,
        "org.scalacheck" %%% "scalacheck" % "1.15.4" % Test,
        "org.scalatestplus" %%% "scalacheck-1-15" % "3.2.11.0" % Test,
        "org.playframework" %%% "play-json" % "3.0.4"
      ),
      libraryDependencies ++= {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, _)) => Seq(
            "com.chuusai" %%% "shapeless" % "2.3.3"
          )
          case _ =>
            Seq.empty
        }
      },
      scalacOptions ++= {
        Seq(
          "-deprecation",
          "-encoding", "UTF-8",
          "-feature",
          "-unchecked",
          "-Ywarn-dead-code",
          "-Ywarn-numeric-widen",
          "-Ywarn-value-discard",
          "-Xlint"
        ) ++
        (CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, n)) if n >= 13 => Seq("-Xsource:3")
          case _ => Seq("-Yno-adapted-args", "-Ywarn-unused-import", "-Xfuture")
        })
      },
      Test / scalacOptions += "-Yrangepos"
    )

val libraryJVM = library.jvm
val libraryJS = library.js

val `play-json-derived-codecs` =
  project.in(file(".")).aggregate(libraryJVM, libraryJS)
