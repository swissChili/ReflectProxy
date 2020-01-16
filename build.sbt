name := "com.reflectjs.Proxy"

version := "0.1"

mainClass in Compile := Some("com.reflectjs.Main")

scalaVersion := "2.13.1"

resolvers += "spring-repo" at "https://repo.spring.io/libs-milestone"

// https://mvnrepository.com/artifact/rawhttp/rawhttp-core
libraryDependencies ++= Seq(
  "com.athaydes.rawhttp" % "rawhttp-core" % "2.1",
  "mysql" % "mysql-connector-java" % "6.0.6"
)

enablePlugins(JavaAppPackaging)
