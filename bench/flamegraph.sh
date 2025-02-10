#!/bin/sh
export CARGO_PROFILE_BENCH_DEBUG=true
cargo flamegraph --bench markdown-it --features simd -- --bench
[target.x86_64-unknown-linux-gnu]
flamegraph --reverse --perfdata perf.data --output flamegraph-reverse.svg

