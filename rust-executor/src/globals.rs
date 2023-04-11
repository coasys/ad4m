use lazy_static::lazy_static;

lazy_static! {
    /// The current version of AD4M
    pub static ref AD4M_VERSION: String = String::from("0.4.0");
}

/// Struct representing oldest supported version and indicator if state should be cleared if update is required
pub struct OldestVersion {
    pub version: String,
    pub clear_state: bool,
}

lazy_static! {
    /// The oldest version of the AD4M protocol that this executor supports
    pub static ref OLDEST_VERSION: OldestVersion = OldestVersion {
        version: String::from("0.4.0"),
        clear_state: true,
    };
}

/// Raw JSON data for the mainnet seed, included at buildtime from the mainnet_seed.json file
pub const MAINNET_JSON: &str = include_str!("mainnet_seed.json");

/// Binary data of holochain binary, included at buildtime from the holochain binary
pub const HOLOCHAIN_BIN: &[u8] = include_bytes!("../temp/holochain");

/// Binary data of hc binary, included at buildtime from the hc binary
pub const HC_BIN: &[u8] = include_bytes!("../temp/hc");

/// Binary data of swipl binary, included at buildtime from the swipl binary
pub const SWIPL_BIN: &[u8] = include_bytes!("../temp/swipl");
