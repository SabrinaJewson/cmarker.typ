#!/bin/sh
set -e
cd "$(dirname $0)"
cargo build -p plugin --release --no-default-features --target wasm32v1-none
cp ./target/wasm32v1-none/release/plugin.wasm .
