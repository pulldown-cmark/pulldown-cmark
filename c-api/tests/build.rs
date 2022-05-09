use cc;
use glob::glob;
use std::path::{Path, PathBuf};

const CFLAGS: &'static [&str] = &[
    "-std=c99",
    "-pthread",
    "-Wcast-qual",
    "-Wwrite-strings",
    "-Wshadow",
    "-Winline",
    "-Wdisabled-optimization",
    "-Wuninitialized",
    "-Wcast-align",
    "-Wcast-align",
    "-Wno-missing-field-initializers",
    "-Wno-address",
];

const SRC_DIR: &str = "src";
const PICOTEST_DIR: &str = "src/deps/picotest";
const INCLUDE_DIR: &str = "../include";

fn glob_c_files<P: AsRef<Path>>(dirname: P) -> Vec<PathBuf> {
    const C_PATTERN: &str = "*.c";
    let cwd = std::env::current_dir().expect("failed to determine working directory");

    glob(
        dirname
            .as_ref()
            .join(C_PATTERN)
            .to_str()
            .expect("Path is not valid unicode."),
    )
    .expect("Failed to read glob pattern")
    .filter_map(Result::ok)
    .inspect(|path| {
        let relative_path = path
            .strip_prefix(&cwd)
            .unwrap_or(path)
            .to_str()
            .expect("non-ascii C source file");
        println!("cargo:rerun-if-changed={}", relative_path);
    })
    .collect::<Vec<_>>()
}

fn main() {
    let mut build = cc::Build::new();

    for cflag in CFLAGS {
        build.flag(cflag);
    }

    println!("cargo:rerun-if-changed=../include/pulldown-cmark.h");

    // Collect all the C files from src/deps/picotest and src.
    let mut c_files = glob_c_files(PICOTEST_DIR);

    c_files.append(&mut glob_c_files(SRC_DIR));

    build
        .debug(true)
        .opt_level(0)
        .flag_if_supported("-Wl,no-as-needed")
        .warnings(true)
        .extra_warnings(true)
        .warnings_into_errors(true)
        .include(INCLUDE_DIR)
        .include(PICOTEST_DIR)
        .files(c_files)
        .compile("pulldown_cmark_ctests");

    // Link against the C API.
    println!("cargo:rustc-link-lib=dylib=pulldowncmark");
}
