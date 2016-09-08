#!/bin/bash
set -e
npm install
elm-make Main.elm --output thelma.js
./node_modules/.bin/electron .
