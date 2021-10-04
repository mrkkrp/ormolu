#!/bin/sh
set -eu

mkdir -p dist
ahc-link \
  --input-hs OrmoluLive.hs \
  --output-directory dist \
  --no-main \
  --export-function=webOrmolu \
  --browser \
  --yolo
