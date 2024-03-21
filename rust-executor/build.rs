use std::fs;
use std::path::Path;
use std::process::Command;

fn build_dapp() -> std::io::Result<()> {
    let dapp_dir = Path::new("./dapp");

    // Navigate to the dapp directory
    std::env::set_current_dir(&dapp_dir)?;

    // Run `pnpm install`
    let status = Command::new("pnpm")
        .arg("install")
        .status()?;
    if !status.success() {
        return Err(std::io::Error::new(std::io::ErrorKind::Other, "pnpm install failed"));
    }

    // Run `pnpm build`
    let status = Command::new("pnpm")
        .arg("build")
        .status()?;
    if !status.success() {
        return Err(std::io::Error::new(std::io::ErrorKind::Other, "pnpm build failed"));
    }

    // Navigate back to the original directory
    std::env::set_current_dir(env!("CARGO_MANIFEST_DIR"))?;

    Ok(())
}

#[allow(dead_code)]
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

fn build_js_executor() -> std::io::Result<()> {
    let executor_dir = Path::new("./executor");

    // Navigate to the executor directory
    std::env::set_current_dir(&executor_dir)?;

    // Run `pnpm install`
    let status = Command::new("pnpm")
        .arg("install")
        .status()?;
    if !status.success() {
        return Err(std::io::Error::new(std::io::ErrorKind::Other, "pnpm install failed"));
    }

    // Run `pnpm build`
    let status = Command::new("pnpm")
        .arg("build")
        .status()?;
    if !status.success() {
        return Err(std::io::Error::new(std::io::ErrorKind::Other, "pnpm build failed"));
    }

    // Navigate back to the original directory
    std::env::set_current_dir(env!("CARGO_MANIFEST_DIR"))?;

    Ok(())
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs"); // Ensure the build script is re-run if it is changed
    println!("cargo:rerun-if-changed=../dapp"); // Re-run if the dapp directory changes
    println!("cargo:rerun-if-changed=../dapp/public"); // Re-run if the dapp directory changes
    println!("cargo:rerun-if-changed=../executor"); // Re-run if the dapp directory changes

    if let Err(err) = build_dapp() {
        eprintln!("Error building dapp: {}", err);
        std::process::exit(1);
    }

    if let Err(err) = build_js_executor() {
        eprintln!("Error building JS executor: {}", err);
        std::process::exit(1);
    }
}