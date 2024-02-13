# Builds and copies the executables to bin/<current git HEAD hash>
# This is useful when wanting to compile some snapshots to compare how different versions behave.
make
echo ""
dir="bin/$(git describe --long --dirty --always)"
mkdir -p $dir
cp bin/* $dir 2>/dev/null
echo "Copied executables to $dir"
