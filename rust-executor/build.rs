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
    let source_dir = "../dapp/public";

    let target_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/dapp");

    if let Err(err) = copy_dir_recursive(Path::new(source_dir), Path::new(target_dir)) {
        eprintln!("Error copying directory: {}", err);
    }
}