import scala.xml.XML

lazy val commonSettings = Seq(
  organization := "org.sorm-framework",
  name := "sorm",
  version := "0.3.21-SNAPSHOT",
  scalaVersion := "2.11.8",
  description := "A functional boilerplate-free Scala ORM",
  crossScalaVersions := Seq("2.11.8")
)

lazy val root = project.in(file(".")).settings(commonSettings:_*).
  settings(
    libraryDependencies ++= Seq(
      "com.mchange" % "c3p0" % "0.9.2-pre5",
      "com.github.nikita-volkov" % "embrace" % "0.1.5",
      "com.github.nikita-volkov" % "sext" % "0.2.5",
      "joda-time" % "joda-time" % "2.1",
      "org.joda" % "joda-convert" % "1.2",
      "com.google.guava" % "guava" % "13.0.1",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
      "org.slf4j" % "slf4j-simple" % "1.7.2" % "test",
      "postgresql" % "postgresql" % "9.1-901.jdbc4" % "test",
      "org.hsqldb" % "hsqldb" % "2.2.8" % "test",
      "com.h2database" % "h2" % "1.3.168" % "test",
      "mysql" % "mysql-connector-java" % "5.1.19" % "test",
      "org.scalatest" %% "scalatest" % "2.2.3" % "test",
      "junit" % "junit" % "4.7" % "test",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value
    ),
    publishMavenStyle := true,
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    parallelExecution in Test := false,
    pomExtra := {
      val xml = XML.loadFile(file("pom-extra.xml"))
      xml.child
    }
  )



