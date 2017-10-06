// Add sonatype repository settings
useGpg := true

pomIncludeRepository := { _ => false }

publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)

publishArtifact in Test := false

// Your profile name of the sonatype account. The default is the same with the organization value
sonatypeProfileName := "com.github.slowaner"

// To sync with Maven central, you need to supply the following information:
publishMavenStyle := true

// License of your choice
licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

homepage := Some(url("https://github.com/slowaner/reflections4s"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/slowaner/reflections4s"),
    "scm:git@github.com:slowaner/reflections4s.git"
  )
)

developers := List(
  Developer(id = "slowaner", name = "Turchaninov Nikita", email = "opensource@slowaner.ru", url = url("https://github.com/slowaner"))
)
