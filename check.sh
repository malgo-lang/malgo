#!/bin/zsh
# Compare .golden/Malgo.NewParser/** with .golden/Malgo.Parser.**
# and print the diff if there is any difference.

# Usage: ./check.sh

# List of directories to compare
delta <(cat .golden/Malgo.NewRename/**/actual) <(cat .golden/Malgo.Rename/**/actual)