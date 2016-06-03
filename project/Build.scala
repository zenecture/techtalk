import sbt.Keys._
import sbt._

object TechTalk extends Build {

  val sets = Defaults.coreDefaultSettings ++ Seq(
    name in ThisBuild := "TechTalk",
    organization in ThisBuild := "com.zenecture",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.7",
    scalacOptions := Seq("-Xlog-implicits")
  )

  lazy val hlist = Project(id = "hlist", base = file("hlist"), settings = sets)
  lazy val ycombinator = Project(id = "ycombinator", base = file("ycombinator"), settings = sets) dependsOn hlist

}
