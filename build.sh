set -ex

if [ "$1" = "--release" ]; then
    arg="--release ${@:2}"
    path="debug"
else
    arg="${@:1}"
    path="debug"
fi

cargo build $arg --bin runtime
cp "target/$path/runtime.exe" "src/"
strip "src/runtime.exe"
cargo build $arg --bin wsc
