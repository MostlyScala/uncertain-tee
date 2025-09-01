// https://typelevel.org/sbt-typelevel/faq.html#what-is-a-base-version-anyway
ThisBuild / tlBaseVersion := "0.1" // your current series x.y

ThisBuild / organization := "mostly"
ThisBuild / organizationName := "Mostly Codes"
ThisBuild / startYear := Some(2025)
ThisBuild / licenses := Seq(License.MIT)
ThisBuild / developers := List(
  // your GitHub handle and name
  tlGitHubDev("TobiasRoland", "Tobias Roland")
)

ThisBuild / tlSitePublishBranch := Some("main")

val scala3 = "3.3.6"
ThisBuild / crossScalaVersions := Seq(scala3)
ThisBuild / scalaVersion := scala3

lazy val root = tlCrossRootProject.aggregate(core)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "uncertain-tee",
    libraryDependencies ++= Seq(
//      "org.typelevel" %%% "cats-core"   % "2.13.0" % Test,
//      "org.typelevel" %%% "cats-effect" % "3.6.3"% Test,
      "org.scalameta" %%% "munit"                % "1.1.1" % Test,
      "org.scalameta" %%% "munit"                % "1.1.1" % Test,
      "org.typelevel" %%% "munit-cats-effect"    % "2.1.0" % Test
    )
  )

lazy val docs = project.in(file("site"))
  .enablePlugins(TypelevelSitePlugin)
