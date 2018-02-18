resolvers += Resolver.url("dancingrobot84-bintray",
    url("http://dl.bintray.com/dancingrobot84/sbt-plugins/"))(Resolver.ivyStylePatterns)

addSbtPlugin("org.jetbrains" % "sbt-ide-settings" % "0.1.1")
addSbtPlugin("com.dancingrobot84" % "sbt-idea-plugin" % "0.4.2")
