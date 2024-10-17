fn main() {
    println!("cargo:rustc-link-arg=-Wl,-rpath,$ORIGIN/../lib/adam-launcher/_up_/_up_/target/release/lib");
    tauri_build::build()
}
