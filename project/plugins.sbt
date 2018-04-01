resolvers += Resolver.url(
  "tpolecat-sbt-plugin-releases",
    url("http://dl.bintray.com/content/tpolecat/sbt-plugin-releases"))(
        Resolver.ivyStylePatterns)

addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.6.2")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.22")

addSbtPlugin("com.lucidchart" % "sbt-scalafmt" % "1.14")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")

// addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.2.3")