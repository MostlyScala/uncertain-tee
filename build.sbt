// https://typelevel.org/sbt-typelevel/faq.html#what-is-a-base-version-anyway
ThisBuild / tlBaseVersion := "0.1"

ThisBuild / organization     := "mostly"
ThisBuild / organizationName := "Mostly Codes"
ThisBuild / startYear        := Some(2025)
ThisBuild / licenses         := Seq(License.MIT)
ThisBuild / developers       := List(
  // your GitHub handle and name
  tlGitHubDev("TobiasRoland", "Tobias Roland"),
  tlGitHubDev("AdamJKing", "Adam King"),
)

ThisBuild / tlSitePublishBranch := Some("main")

val scala3 = "3.3.6"
ThisBuild / crossScalaVersions := Seq(scala3)
ThisBuild / scalaVersion       := scala3

lazy val root = tlCrossRootProject.aggregate(core)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "uncertain-tee",
    libraryDependencies ++= Dependencies.test
  )

lazy val docs = project
  .in(file("site"))
  .enablePlugins(TypelevelSitePlugin)

lazy val Dependencies = new {

  object V {
    val munit      = "1.1.0"
    val scalacheck = "1.18.1"
//     val cats = "2.10.0"
//     val catsEffect = "3.6.3"
  }

  val test = Seq(
    "org.scalameta"  %% "munit"            % V.munit      % Test,
    "org.scalameta"  %% "munit-scalacheck" % V.munit      % Test,
    "org.scalacheck" %% "scalacheck"       % V.scalacheck % Test
  )
}
