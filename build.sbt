name := "scala-example"

version := "1.0"

scalaVersion := "2.11.6"

EclipseKeys.withSource:=true

libraryDependencies += "com.twitter" %% "finagle-http" % "6.24.0"

resolvers += "rediscala" at "http://dl.bintray.com/etaty/maven"

libraryDependencies ++= Seq(
  "com.etaty.rediscala" %% "rediscala" % "1.4.0"
)


    
