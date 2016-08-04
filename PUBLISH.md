# Publish instructions

- Update [version.sbt](version.sbt)

- Update `libraryDependencies` in [README.md](README.md)

- Update comment in `play-scalajs-example/build.sbt`

- Publish book:

    ```sh
    git checkout gh-pages
    git checkout master .
    sh scripts/build-book.sh
    git add .
    git commit -am "Update book"
    git push
    ```

- Publish library:

    ```sh
    sbt publishSigned
    sbt sonatypeReleaseAll
    ```
