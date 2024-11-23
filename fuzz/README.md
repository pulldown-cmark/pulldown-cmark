# Fuzz targets

This crate specifies fuzzing targets which are
instrumented with [cargo-fuzz](https://github.com/rust-fuzz/cargo-fuzz).

## Fixing fuzz build issue

At the moment, building fuzz targets with default settings
(`cargo fuzz build`) throws many errors like this:

```
: rust-lld: error: undefined symbol: __sancov_gen_.1094
          >>> referenced by parse.3be71763e9de75d0-cgu.0
          >>>               /home/user/projects/pulldown-cmark/target/x86_64-unknown-linux-gnu/release/deps/parse-4bac226fcf249aac.parse.3be71763e9de75d0-cgu.0.rcgu.o:(asan.module_dtor.1168)
```

The issue seems to be triggered during linking of the binaries
with default `profile.release.lto=true` set at the workspace `Cargo.toml`
file.

To fix the build, you can override `lto` config using env variable:

```bash
$ CARGO_PROFILE_RELEASE_LTO=thin cargo fuzz run parse -- -only_ascii=1 -max_total_time=60
```
