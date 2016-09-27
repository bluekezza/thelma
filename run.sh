#!/bin/sh

cd "$(dirname "$0")"
set -e

elm-package install -y 

elm-make --yes src/Main.elm --output thelma.js
./node_modules/.bin/electron .
