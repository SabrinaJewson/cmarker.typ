#!/bin/sh
set -eu
case "$(git remote get-url upstream)" in
	(*typst/packages) ;;
	(*)
		echo This command must be run in the typst/packages repository!
		exit 1
		;;
esac
SOURCE="$(dirname $0)"
VERSION=$(sed -n 's/^version = "\(.*\)"$/\1/p' < "$SOURCE/typst.toml")
echo Preparing version $VERSION
mkdir packages/preview/cmarker/$VERSION
mkdir packages/preview/cmarker/$VERSION/examples
TARGET="$PWD/packages/preview/cmarker/$VERSION"
cd "$SOURCE"
./build.sh
cp LICENSE README.md lib.typ typst.toml plugin.wasm "$TARGET"
cp examples/simple.png examples/hexagons.png "$TARGET/examples"
echo Prepared version at packages/preview/cmarker/$VERSION
