name := "com.reflectjs.Proxy"

version := "0.1"

mainClass in Compile := Some("com.reflectjs.Main")

scalaVersion := "2.13.1"

resolvers += "spring-repo" at "https://repo.spring.io/libs-milestone"

// https://mvnrepository.com/artifact/rawhttp/rawhttp-core
libraryDependencies += "com.athaydes.rawhttp" % "rawhttp-core" % "2.1"

enablePlugins(JavaAppPackaging)
