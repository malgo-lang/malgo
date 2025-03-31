#!/bin/zsh
# Compare .golden/Malgo.NewParser/** with .golden/Malgo.Parser.**
# and print the diff if there is any difference.

# Usage: ./check.sh

# List of directories to compare
for dir in .golden/Malgo.NewRename/**/sexpr/actual; do
  echo $dir
  dir2=${dir/NewRename/Rename}
  delta $dir $dir2
done