name := "PhotoSynthesis"

version := "0.1"

scalaVersion := "2.12.8"

val akkaStreamVersion = "2.6.5"
val akkaHttpVersion = "10.1.11"
val akkaVersion = "2.6.5"
val scalaTestVersion = "3.1.1"
val mockitoVersion = "1.14.0"

libraryDependencies ++= List(
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test,
  "com.typesafe.akka" %% "akka-http"   % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaStreamVersion,
  "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-stream-testkit" % akkaStreamVersion,
  "io.spray" %% "spray-json" % "1.3.3",
  "org.typelevel" %% "cats-effect" % "2.1.3",
  "org.typelevel" %% "cats-effect-laws" % "2.1.3" % "test",
  "org.scalactic" %% "scalactic" % scalaTestVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "org.mockito" % s"mockito-scala_2.12" % mockitoVersion % "test"
)
