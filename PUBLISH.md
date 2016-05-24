# Publish instructions

- Update [version.sbt](version.sbt)

- Update `libraryDependencies` in [README.md](README.md)

- Publish book:

    ```sh
    git checkout gh-pages
    git checkout master .
    sh misc/build-book.sh
    git add .
    git commit -am "Update book"
    git push
    ```

- Publish library:

    ```sh
    sbt publish # TBD
    ```
