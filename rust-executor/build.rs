use std::fs;
use std::path::Path;

fn copy_dir_recursive(source: &Path, target: &Path) -> std::io::Result<()> {
    if source.is_dir() {
        fs::create_dir_all(target)?;
        for entry in fs::read_dir(source)? {
            let entry = entry?;
            let entry_target = target.join(entry.file_name());
            copy_dir_recursive(&entry.path(), &entry_target)?;
        }
    } else {
        fs::copy(source, target)?;
    }
    Ok(())
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs"); // Ensure the build script is re-run if it is changed
    println!("cargo:rerun-if-changed=../dapp/public"); // Re-run if the dapp directory changes

    let source_dir = "../dapp/public";
    let target_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/dapp");

    println!("Copying from {} to {}", source_dir, target_dir);


    if let Err(err) = copy_dir_recursive(Path::new(source_dir), Path::new(target_dir)) {
        eprintln!("Error copying directory: {}", err);
        std::process::exit(1); // Exit with an error code to fail the build if the copy fails
    }
}