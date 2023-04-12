use std::env::var_os;
use std::fs::File;
use std::io::{Cursor, Read, Write};
use std::path::{Path, PathBuf};
use zip::ZipArchive;

pub(crate) fn ad4m_data_directory() -> PathBuf {
    let mut ad4m_dir = var_os("HOME")
        .unwrap_or_else(|| panic!("Failed to get home directory"))
        .into_string()
        .unwrap_or_else(|_| panic!("Failed to convert HOME env variable to string"));
    ad4m_dir.push_str("/.ad4m");
    Path::new(&ad4m_dir).to_path_buf()
}

pub(crate) fn write_zip(zip_data: Vec<u8>, target_path: PathBuf) {
    // Read the zip archive from the byte data
    let reader = Cursor::new(zip_data);
    let mut archive = ZipArchive::new(reader).unwrap();

    // Extract the files from the zip archive
    for i in 0..archive.len() {
        let mut file = archive.by_index(i).unwrap();
        let mut outpath = target_path.clone();
        #[allow(deprecated)]
        outpath.push(file.sanitized_name());

        if file.is_dir() {
            // Create the directory if it doesn't exist
            std::fs::create_dir_all(&outpath).unwrap();
        } else {
            // Create the parent directory if it doesn't exist
            if let Some(parent) = outpath.parent() {
                if !parent.exists() {
                    std::fs::create_dir_all(&parent).unwrap();
                }
            }

            // Write the file's contents
            let mut outfile = File::create(&outpath).unwrap();
            let mut buffer = Vec::new();
            file.read_to_end(&mut buffer).unwrap();
            outfile.write_all(&buffer).unwrap();
        }
    }
}
