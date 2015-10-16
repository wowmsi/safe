// Exclude scala-library.jar because we re-wrote a few classes, such as
// Symbol and UniquenessCache
assemblyExcludedJars in assembly := {
  val cp = (fullClasspath in assembly).value
  cp filter { _.data.getName == "scala-library-2.11.6.jar" }
}

assemblyMergeStrategy in assembly := {
  case PathList("org", "intellij", "lang", "annotations", xs @ _*) => MergeStrategy.first
  case PathList("org", "jetbrains", "annotations", xs @ _*) => MergeStrategy.first
  case PathList("org", "mockito", "internal", xs @ _*) => MergeStrategy.first
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

