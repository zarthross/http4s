
resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies += "org.scalamacros" % "quasiquotes" % "2.0.0-M3" cross CrossVersion.full

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full)