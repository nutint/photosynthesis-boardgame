name := "PhotoSynthesis"

version := "0.1"

scalaVersion := "2.12.8"

val akkaStreamVersion = "2.5.26"
val akkaHttpVersion = "10.1.11"


libraryDependencies ++= List(
  "com.typesafe.akka" %% "akka-http"   % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaStreamVersion,
  "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-stream-testkit" % akkaStreamVersion,
  "io.spray" %% "spray-json" % "1.3.3",
  "org.typelevel" %% "cats-effect" % "2.1.3",
  "org.typelevel" %% "cats-effect-laws" % "2.1.3" % "test",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.mockito" % "mockito-all" % "1.8.4" % "test"
)
