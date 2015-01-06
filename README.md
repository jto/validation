# The Play data validation library

Play new validation API extracted from Play.

## Using the validation api in your project

Add the following lines in your `build.sbt`

```scala
resolvers += "JTO snapshots" at "https://raw.github.com/jto/mvn-repo/master/snapshots"

// If you want only the core
libraryDependencies +="io.github.jto" %% "validation-core" % "1.0-1c770f4"

// Json validation
libraryDependencies +="io.github.jto" %% "validation-json" % "1.0-1c770f4"

// Form validation
libraryDependencies +="io.github.jto" %% "validation-form" % "1.0-1c770f4"

// ...
```

## Documentation

```bash
cd documentation
sbt -Dplay.version=2.3.0-RC1 run
# browse http://localhost:9000/@documentation/ScalaValidation
```

## Contributors

- Julien Tournay - http://jto.github.io
- Nick - https://github.com/stanch
- Ian Hummel - https://github.com/themodernlife
- Arthur Gautier - https://github.com/baloo
- Jacques B - https://github.com/Timshel
- Alexandre Tamborrino - https://github.com/atamborrino
