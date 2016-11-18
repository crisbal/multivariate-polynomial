#!/bin/bash
set -e
cd "$(dirname "$0")"

name="Cristian"
surname="Baldi"
number="806830"

scriptDirectory=($PWD)
recipe=${1:-build}

case "$recipe" in
    build)
        buildName=$surname"_"$name"_"$number"_mvpoli_LP_201701"
        echo "Building $buildName..."

        echo "Creating temp dir..."
        tempDir=$(mktemp -d)
        echo "Created $tempDir"
        mkdir -p "$tempDir/$buildName"

        echo "Building Prolog..."
        mkdir -p "$tempDir/$buildName/Prolog"
        cp "Prolog/mvpoli.pl" "$tempDir/$buildName/Prolog"
        cp "Prolog/README.txt" "$tempDir/$buildName/Prolog"

        echo "Building Lisp..."
        mkdir -p "$tempDir/$buildName/Lisp"
        cp "Lisp/mvpoli.lisp" "$tempDir/$buildName/Lisp"
        cp "Lisp/README.txt" "$tempDir/$buildName/Lisp"
        
        echo "Creating zip archive"
        cd $tempDir
        zip "$scriptDirectory/$buildName.zip" -r .
        
        echo "Cleaning up"
        rm -rf $tempDir
        cd $scriptDirectory

        echo "Done!"
        ;;
    *)
        echo "Unknown option $recipe"
        exit 1
esac