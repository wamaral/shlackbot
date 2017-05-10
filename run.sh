#!/bin/bash

stack build --fast --pedantic

until stack run; do
  echo "Slaskellbot crashed with exit code $?.  Respawning.." >&2
  sleep 1
done

