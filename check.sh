# Compare .golden/Malgo.Core.Eval/** with .golden/Malgo.Driver/driver/normalCase/**
# and print the diff if there is any difference.

# Usage: ./check.sh

# List of directories to compare
dirs=$(ls .golden/Malgo.Sequent.Eval)

# Compare each directory
for dir in $dirs; do
    diff -r .golden/Malgo.Sequent.Eval/$dir .golden/Malgo.Core.Eval/$dir
done