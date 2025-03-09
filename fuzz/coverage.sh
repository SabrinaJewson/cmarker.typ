#!/bin/sh
set -eu
cd "$(dirname "$0")"

fuzz_target=fuzz
cargo +nightly fuzz coverage $fuzz_target corpus/$fuzz_target

toolchain=$(rustup +nightly show active-toolchain | awk '{ print $1 }')
target=$(echo $toolchain | sed 's/nightly-//')

llvm_cov="$RUSTUP_HOME"/toolchains/$toolchain/lib/rustlib/$target/bin/llvm-cov

"$llvm_cov" show target/$target/coverage/$target/release/$fuzz_target \
	-Xdemangler=rustfilt \
	-instr-profile=coverage/$fuzz_target/coverage.profdata \
	--ignore-filename-regex="cargo|rustup" \
	--format=html > coverage.html
