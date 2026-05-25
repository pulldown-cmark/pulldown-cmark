use std::env;
use std::fs;
use std::process::Command;

fn main() {
    env::set_current_dir(env::current_exe().unwrap().parent().unwrap()).unwrap();

    let status = Command::new("cargo")
        .args([
            "build",
            "--package=pulldown-cmark-demo",
            "--target=wasm32v1-none",
            "--release",
        ])
        .env("RUSTFLAGS", "-Clto -Ccodegen-units=1")
        .status()
        .unwrap();
    assert!(status.success());

    fs::rename(
        "../target/wasm32v1-none/release/pulldown_cmark_demo.wasm",
        "src/demo.wasm",
    )
    .unwrap();
}
