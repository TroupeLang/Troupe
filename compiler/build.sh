# Builds and copies the executables to ../bin/<current git HEAD hash>
make
echo ""
dir="./../bin/$(git describe --long --dirty --always)"
mkdir -p $dir
cp ./../bin/* $dir 2>/dev/null
echo "Copied executables to $dir"