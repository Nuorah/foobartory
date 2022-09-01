val AkkaVersion = "2.6.19"
ThisBuild / scalaVersion := "2.13.8"
libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
    "com.typesafe.akka" %% "akka-actor-testkit-typed" % AkkaVersion % Test,
    "ch.qos.logback" % "logback-classic" % "1.1.3" % Runtime
)