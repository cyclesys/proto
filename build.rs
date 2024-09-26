use std::env;
use std::process::Command;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let target = env::var("TARGET").unwrap();

    let target = match target.as_str() {
        "aarch64-apple-darwin" => "aarch64-macos-none",
        _ => {
            panic!("unsupported target");
        }
    };

    Command::new("zig")
        .args([
            "build",
            "--prefix",
            &out_dir,
            "-Doptimize=ReleaseSafe",
            &format!("-Dtarget={target}"),
        ])
        .status()
        .expect("failed to compile zig lib");

    println!("cargo::rustc-link-search=native={out_dir}/lib/");
    println!("cargo::rustc-link-lib=static=cycleproto");

    tonic_build::compile_protos("src/cycle.proto").unwrap();
}
