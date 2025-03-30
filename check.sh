#!/bin/zsh
# Compare .golden/Malgo.NewParser/** with .golden/Malgo.Parser.**
# and print the diff if there is any difference.

# Usage: ./check.sh

# List of directories to compare
for dir in .golden/Malgo.NewRename/*; do
  old_dir=.golden/Malgo.Rename/${dir##*/}
  delta $dir $old_dir
done