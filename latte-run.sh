#!/bin/bash

# Check if a filename argument is provided
if [ -z "$1" ]; then
    echo "Usage: $0 <filename.lat>"
    exit 1
fi

# Set the filename and paths
FILENAME="$1"
FILEPATH="test-files/back/$FILENAME"
OUTPUT_LL="output.ll"
OUTPUT_BC="output.bc"
LINKED_BC="linked.bc"
RUNTIME_BC="lib/runtime.bc"

# Step 1: Build the project
make

# Step 2: Run the LATC compiler
./latc "$FILEPATH"

# Step 3: Convert LLVM IR (.ll) to bitcode (.bc)
llvm-as "$OUTPUT_LL" -o "$OUTPUT_BC"

# Step 4: Link with runtime library

llvm-link "$OUTPUT_BC" "$RUNTIME_BC" -o "$LINKED_BC"


# Step 5: Always Run the LLVM bitcode
lli "$LINKED_BC"

exit 0
