import laika.helium.Helium
// https://typelevel.org/sbt-typelevel/faq.html#what-is-a-base-version-anyway
ThisBuild / tlBaseVersion := "0.1"

ThisBuild / organization     := "mostly"
ThisBuild / organizationName := "Mostly Codes"
ThisBuild / startYear        := Some(2025)
ThisBuild / licenses         := Seq(License.Apache2)
ThisBuild / developers       := List(
  // your GitHub handle and name
  tlGitHubDev("TobiasRoland", "Tobias Roland"),
  tlGitHubDev("AdamJKing", "Adam King"),
  tlGitHubDev("smithleej", "Lee Smith")
)

ThisBuild / tlSitePublishBranch := Some("main")

val scala3 = "3.3.7"
ThisBuild / crossScalaVersions := Seq(scala3)
ThisBuild / scalaVersion       := scala3

ThisBuild / tlSiteHelium := Helium.defaults

lazy val root = tlCrossRootProject.aggregate(
  uncertainTee,
  uncertainTeeCats,
  uncertainTeeZio,
  uncertainTeeScalacheck,
  uncertainTeeSquants
)

lazy val uncertainTee = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("uncertain-tee"))
  .settings(
    name := "uncertain-tee",
    libraryDependencies ++= Dependencies.test
  )

lazy val uncertainTeeCats = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("uncertain-tee-cats"))
  .settings(
    name := "uncertain-tee-cats",
    libraryDependencies ++= Dependencies.cats,
    libraryDependencies ++= Dependencies.test
  )
  .dependsOn(uncertainTee)

lazy val uncertainTeeZio = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("uncertain-tee-zio"))
  .settings(
    name := "uncertain-tee-zio",
    libraryDependencies ++= Dependencies.zio,
    libraryDependencies ++= Dependencies.test
  )
  .dependsOn(uncertainTee)

lazy val uncertainTeeScalacheck = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("uncertain-tee-scalacheck"))
  .settings(
    name := "uncertain-tee-scalacheck",
    libraryDependencies ++= Dependencies.scalaCheck,
    libraryDependencies ++= Dependencies.test
  )
  .dependsOn(uncertainTee)

lazy val uncertainTeeSquants = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("uncertain-tee-squants"))
  .settings(
    name := "uncertain-tee-squants",
    libraryDependencies ++= Dependencies.squants,
    libraryDependencies ++= Dependencies.test
  )
  .dependsOn(uncertainTee)

lazy val docs = project
  .in(file("site"))
  .enablePlugins(TypelevelSitePlugin)

lazy val benchmarks = project
  .in(file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .dependsOn(uncertainTee.jvm)

addCommandAlias("benchmark", "benchmarks/Jmh/clean; benchmarks/Jmh/run")

lazy val Dependencies = new {

  object V {
    val munit           = "1.2.0"
    val munitDiscipline = "2.0.0"
    val scalacheck      = "1.19.0"
    val cats            = "2.13.0"
    val catsLaws        = "2.13.0"
    val squants         = "1.8.3"
  }

  val cats = Seq(
    "org.typelevel" %% "cats-core" % V.cats
  )

  val zio = Seq(
    "dev.zio" %% "zio"         % "2.1.24",
    "dev.zio" %% "zio-prelude" % "1.0.0-RC44",
    "dev.zio" %% "zio-test"    % "2.1.24"
  )

  val squants = Seq(
    "org.typelevel" %% "squants" % V.squants
  )

  val scalaCheck = Seq(
    "org.scalacheck" %% "scalacheck" % V.scalacheck
  )

  val test = Seq(
    "org.scalameta"  %% "munit"            % V.munit           % Test,
    "org.scalameta"  %% "munit-scalacheck" % V.munit           % Test,
    "org.scalacheck" %% "scalacheck"       % V.scalacheck      % Test,
    "org.typelevel"  %% "cats-laws"        % V.catsLaws        % Test,
    "org.typelevel"  %% "discipline-munit" % V.munitDiscipline % Test
  )
}
