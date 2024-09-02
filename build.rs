use std::env;
use std::path::Path;
use std::process::Command;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let target = env::var("TARGET").unwrap();

    Command::new("zig")
        .args([
            "build",
            "--prefix-lib-dir",
            &out_dir,
            "-O",
            "ReleaseSafe",
            "-target",
            &target,
        ])
        .status()
        .expect("failed to compile zig lib");

    println!("cargo::rustc-link-search=native={out_dir}");
    println!("cargo::rustc-link-lib=static=cycleproto");
}
