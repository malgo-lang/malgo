#!/bin/bash

# Remove .malgo-work directory if it exists
if [ -d ".malgo-work" ]; then
  rm -rf .malgo-work
fi

# Add `--test-option=` to the all arguments
args=("$@")
for i in "${!args[@]}"; do
  args[$i]="--test-option='${args[$i]}'"
done
# Run the command with the modified arguments
command="cabal test ${args[*]}"
echo "Running command: $command"
eval $command