#!/bin/bash

# Check if a directory is provided as an argument
if [ -z "$1" ]; then
  echo "Usage: $0 <directory>"
  exit 1
fi

# Get the directory from the argument
DIRECTORY=$1

# Check if the provided argument is a directory
if [ ! -d "$DIRECTORY" ]; then
  echo "Error: $DIRECTORY is not a directory"
  exit 1
fi

# Loop through all files in the directory with .lat extension
for FILE in "$DIRECTORY"/*.lat; do

  if [ -f "$FILE" ]; then # Ensure it is a file
    echo "Running interpreter on $FILE"
    ./latc "$FILE"
  fi
done
