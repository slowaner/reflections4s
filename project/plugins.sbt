// Versions
// SBT-Pack
val sbtPackVersion = "0.9.3-SNAPSHOT"
// Release plugin
val releasePluginVersion = "1.0.6"

// SBT-Pack plugin
addSbtPlugin("org.xerial.sbt" %% "sbt-pack" % sbtPackVersion cross CrossVersion.binary )

// Release plugin
addSbtPlugin("com.github.gseitz" % "sbt-release" % releasePluginVersion)
