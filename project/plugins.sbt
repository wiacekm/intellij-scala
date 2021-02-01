resolvers += Resolver.url("jetbrains-sbt", url(s"https://dl.bintray.com/jetbrains/sbt-plugins"))(Resolver.ivyStylePatterns)

addSbtPlugin("org.jetbrains" % "sbt-ide-settings" % "1.0.0")
addSbtPlugin("org.jetbrains" % "sbt-idea-plugin" % "3.8.4+9-94260e9f+20210201-2313")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.7.0")
