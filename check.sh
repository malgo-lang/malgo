# Compare .golden/Malgo.Core.Eval/** with .golden/Malgo.Driver/driver/normalCase/**
# and print the diff if there is any difference.

# Usage: ./check.sh

# List of directories to compare
dirs=$(ls .golden/Malgo.Sequent.Eval)

# Compare each directory
for dir in $dirs; do
    echo "Comparing $dir"
    diff .golden/Malgo.Sequent.Eval/$dir/actual .golden/Malgo.Core.Eval/$dir/actual
done

# Copy golden files
for dir in $dirs; do
    echo "Coping $dir"
    cp .golden/Malgo.Core.Eval/$dir/golden .golden/Malgo.Sequent.Eval/$dir/golden
done