#!/bin/sh
set -eu

ahc-cabal build exe:ormolu-live

OUTPUT_DIR=dist/asterius
mkdir -p $OUTPUT_DIR
ahc-dist \
  --input-exe $(ahc-cabal list-bin exe:ormolu-live) \
  --output-directory $OUTPUT_DIR \
  --browser \
  --yolo
