name := "PhotoSynthesis"

version := "0.1"

scalaVersion := "2.12.7"

val akkaStreamVersion = "2.5.26"
val akkaHttpVersion = "10.1.11"

libraryDependencies ++= List(
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "com.typesafe.akka" %% "akka-http"   % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaStreamVersion,
  "com.typesafe.akka" %% "akka-stream-testkit" % akkaStreamVersion,
  "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
  "io.spray" %% "spray-json" % "1.3.3",
  "org.mockito" % "mockito-all" % "1.8.4" % "test"
)
