use std::env;

fn main() {
    let target_arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap();
    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap();

    // macOS uses leading-underscore symbols in assembler labels.
    // Linux/Windows expect plain symbol names.
    let use_macos_symbols = target_os == "macos";

    // Compile the appropriate assembly file for the target architecture and OS.
    let asm_file = match (target_arch.as_str(), use_macos_symbols) {
        ("x86_64", true) => "src/arch/x86_64/context.s",
        ("x86_64", false) => "src/arch/x86_64/context_linux.s",
        ("aarch64", true) => "src/arch/aarch64/context.s",
        ("aarch64", false) => "src/arch/aarch64/context_linux.s",
        _ => {
            panic!(
                "Unsupported architecture: {}. Only x86_64 and aarch64 are supported.",
                target_arch
            );
        }
    };

    cc::Build::new().file(asm_file).compile("context");

    println!("cargo:rerun-if-changed={}", asm_file);
    println!("cargo:rerun-if-changed=build.rs");
}
