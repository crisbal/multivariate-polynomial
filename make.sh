#!/bin/bash
AUTHOR1=("Cristian" "Baldi" "806830")
AUTHOR2=("Luca" "Toma" "789662")

set -e
if test ! -f make.sh
then
  echo "Error with cd"
  exit 1
fi

cd `dirname "$0"`
scriptDirectory=$(pwd)
outputDirectory=${scriptDirectory}/build
if test ! -d $outputDirectory
then
    mkdir "$outputDirectory"
fi
recipe=${1:-build}

build() {
    name="$1"
    surname="$2"
    number="$3"
    co_name="$4"
    co_surname="$5"
    co_number="$6"

    buildName=$surname"_"$name"_"$number"_mvpoli_LP_201701"
    echo "Building $buildName..."

    echo "Creating temp dir..."
    tempDir=`mktemp -d`
    echo "Created $tempDir"
    mkdir -p "$tempDir/$buildName"

    if test -f "$scriptDirectory/Prolog/Makefile"
    then
        cd "$scriptDirectory/Prolog"
        echo "Building Prolog..."
        make build
        mkdir -p "$tempDir/$buildName/Prolog"
        cp "mvpoli.pl" "$tempDir/$buildName/Prolog"
        cp "README.txt" "$tempDir/$buildName/Prolog"
        cd "$scriptDirectory"
    fi

    if test -f "$scriptDirectory/Lisp/Makefile"
    then
        cd "$scriptDirectory/Lisp"
        echo "Building Lisp..."
        #make build
        mkdir -p "$tempDir/$buildName/Lisp"
        cp "mvpoli.lisp" "$tempDir/$buildName/Lisp"
        cp "README.txt" "$tempDir/$buildName/Lisp"
        cd "$scriptDirectory"
    fi

    echo "Creating zip archive"
    cd $tempDir
    find "$tempDir" -type f | while read file
    do
        sed -i 's/XXXCO_NUMBER/'$co_number'/g' $file
        sed -i 's/XXXCO_SURNAME/'$co_surname'/g' $file
        sed -i 's/XXXCO_NAME/'$co_name'/g' $file
        sed -i 's/XXXNUMBER/'$number'/g' $file
        sed -i 's/XXXSURNAME/'$surname'/g' $file
        sed -i 's/XXXNAME/'$name'/g' $file
    done
    if test -f "$outputDirectory/$buildName.zip"
    then
        rm "$outputDirectory/$buildName.zip"
    fi
    zip "$outputDirectory/$buildName.zip" -r .

    echo "Cleaning up"
    rm -rf $tempDir
    cd $scriptDirectory
}

case "$recipe" in
    build)
        build \
          "${AUTHOR1[0]}" \
          "${AUTHOR1[1]}" \
          "${AUTHOR1[2]}" \
          "${AUTHOR2[0]}" \
          "${AUTHOR2[1]}" \
          "${AUTHOR2[2]}"
        build \
          "${AUTHOR2[0]}" \
          "${AUTHOR2[1]}" \
          "${AUTHOR2[2]}" \
          "${AUTHOR1[0]}" \
          "${AUTHOR1[1]}" \
          "${AUTHOR1[2]}"
        echo "Done!"
        ;;
    *)
        echo "Unknown option $recipe"
        exit 1
esac
