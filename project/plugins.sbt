//========= sbt-assembly================//
// Deploy fat JARs. Restart processes.
// sbt-assembly is a sbt plugin originally ported from codahale's assembly-sbt,
// which I'm guessing was inspired by Maven's assembly plugin. The goal is
// simple: Create a fat JAR of your project with all of its dependencies.
// https://github.com/sbt/sbt-assembly
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.13.0")


//========= sbt-resolver================//
// sbt-revolver is a plugin for SBT enabling a super-fast development turnaround
// for your Scala applications.  It supports the following features:
//
//  - Starting and stopping your application in the background of your interactive SBT shell (in a forked JVM)
//  - Triggered restart: automatically restart your application as soon as some of its sources have been changed
// https://github.com/spray/sbt-revolver
addSbtPlugin("io.spray" % "sbt-revolver" % "0.7.2")

// https://github.com/MasseGuillaume/ScalaKata/
// scala in the browser
addSbtPlugin("com.scalakata" % "plugin" % "0.5.0")
