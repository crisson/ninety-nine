name := "ninety-nine problems"

organization := "com.crisson.ninety"

libraryDependencies ++= Seq("org.specs2" %% "specs2" % "2.2.3" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq("releases"  at "http://oss.sonatype.org/content/repositories/releases")
