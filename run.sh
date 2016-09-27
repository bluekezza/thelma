#!/bin/bash
set -e
npm install
elm-make src/Main.elm --output thelma.js
./node_modules/.bin/electron .
