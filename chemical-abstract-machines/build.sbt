import Dependencies._ 

ThisBuild / scalaVersion     := "2.12.10"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "app.vizion"
javaOptions += "-Xmx32G"
lazy val root = (project in file("."))
  .settings(
    name := "scalaQ-psi-collapse",
    Compile / run / mainClass := Some("com.sinanspd.Cham2"),
    Compile / run / fork := true,
    libraryDependencies ++= Seq(
      Libraries.logback,
      "io.chymyst" %% "chymyst-core" % "0.2.0",
      "javax.xml.bind" % "jaxb-api" % "2.3.1",
      "org.scalanlp" %% "breeze" % "2.1.0",
      "org.typelevel" %% "spire" % "0.17.0", 
      "co.fs2" %% "fs2-core" % "3.9.3",
      "co.fs2" %% "fs2-io" % "3.9.3",
      "org.typelevel" %% "cats-effect" % "3.5.2",
      "com.sinanspd" %% "qure" % "0.1.20-SNAPSHOT"
    )
  )
  
