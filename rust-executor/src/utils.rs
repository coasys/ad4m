use dirs::home_dir;
use portpicker;
use std::path::PathBuf;

pub(crate) fn ad4m_data_directory() -> PathBuf {
    home_dir().unwrap().join(".ad4m")
}

pub fn find_port(start_port: u16, end_port: u16) -> Result<u16, String> {
    for x in start_port..end_port {
        if portpicker::is_free(x) {
            return Ok(x);
        }
    }

    Err(format!(
        "No open port found between: [{:?}, {:?}]",
        start_port, end_port
    ))
}

/// Constant-time comparison for security-sensitive values (prevents timing attacks)
/// This function always performs the same number of operations regardless of where
/// the first difference occurs, preventing timing-based side-channel attacks.
pub fn constant_time_eq(a: &str, b: &str) -> bool {
    let a_bytes = a.as_bytes();
    let b_bytes = b.as_bytes();

    // Always compare the maximum length to ensure constant-time behavior
    let max_len = a_bytes.len().max(b_bytes.len());
    let mut diff = 0u8;

    // Compare all bytes up to the maximum length
    // For indices beyond a string's length, XOR with 0 (no-op for padding)
    for i in 0..max_len {
        let a_byte = if i < a_bytes.len() { a_bytes[i] } else { 0 };
        let b_byte = if i < b_bytes.len() { b_bytes[i] } else { 0 };
        diff |= a_byte ^ b_byte;
    }

    // Also XOR the length difference to ensure different lengths are detected
    // Note: This only works if lengths fit in u8, but for tokens/hashes this is fine
    // For longer strings, we'd need a more sophisticated approach
    if a_bytes.len() <= 255 && b_bytes.len() <= 255 {
        diff |= (a_bytes.len() as u8) ^ (b_bytes.len() as u8);
    } else {
        // For very long strings, compare length bytes directly
        let len_diff = a_bytes.len() ^ b_bytes.len();
        diff |= (len_diff & 0xFF) as u8;
        diff |= ((len_diff >> 8) & 0xFF) as u8;
    }

    // Return true only if all bytes match and lengths are equal
    diff == 0
}
