resolvers += Resolver.url(
  "tpolecat-sbt-plugin-releases",
    url("https://dl.bintray.com/content/tpolecat/sbt-plugin-releases"))(
        Resolver.ivyStylePatterns)

addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.6.12")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "0.6.1")

addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "0.6.29")

addSbtPlugin("com.geirsson" % "sbt-scalafmt" % "1.5.1")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.0")

addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.2.7")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.4.2")

addSbtPlugin("org.wartremover" % "sbt-wartremover" % "2.4.3")

addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.8")

addSbtPlugin("org.duhemm" % "sbt-errors-summary" % "0.6.3")




