# v2.0 Migration guide

Version 2.x breaks back compatibility with the 1.x version. The migration has been tested on production code making heavy use of validation for json (based on play-json) and xml. Even for big projects, migrating to 2.x should not take more than 30 min.

The best method is just to update validation in your dependencies, and let the compiler figure out what's broken. The following changes list should cover everything needed.

#### Build file.

The project name for play-json based validation has changed.

```scala
"io.github.jto" %% "validation-json" % validationVersion
```

becomes

```scala
"io.github.jto" %% "validation-playjson" % validationVersion
```

#### Package name

- Since the library does not depend on Play anymore and is not planned to be integrated into Play, the package names have changed. Basically `play.api.mapping` now becomes `jto.validation`. A simple search and replace in your project should work.
- The validation api support both json4s and play-json. Therefore, the package name for play json changes. `play.api.mapping.json` becomes `play.api.mapping.playjson`

#### Rule renaming

The following `Rule` and `Write` were renamed to better match the naming convention in all subprojects.

- `Rules.jodaDate` becomes `Rules.jodaDateR`
- `Writes.jodaDate`  becomes `Writes.jodaDateW`

If you encounter implicit resolution problem, you probably have a name clash. Make sure none of your `Rule` / `Write` uses those names.

#### unlift

Since validation does not uses play-functional anymore, `unlift` should be imported [directly](https://github.com/playframework/playframework/blob/2.5.3/framework/src/play-functional/src/main/scala/play/api/libs/functional/syntax/package.scala#L31) as `scala.Function.unlift` instead of `play.api.libs.functional.unlift`.

#### ValidationError

Since we removed all the dependencies on Play, `play.api.mapping.ValidationError` is re-defined in validation. If you're using this class, make sure to replace it by `jto.validation.ValidationError`.
