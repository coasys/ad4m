use lazy_static::lazy_static;

lazy_static! {
    /// The current version of AD4M
    pub static ref AD4M_VERSION: String = String::from("0.10.1-release-candidate-5");
}

/// Struct representing oldest supported version and indicator if state should be cleared if update is required
pub struct OldestVersion {
    pub version: String,
    pub clear_state: bool,
}

lazy_static! {
    /// The oldest version of the AD4M protocol that this executor supports
    pub static ref OLDEST_VERSION: OldestVersion = OldestVersion {
        version: String::from("0.8.1"),
        clear_state: true,
    };
}

/// Raw JSON data for the mainnet seed, included at buildtime from the mainnet_seed.json file
pub const MAINNET_JSON: &str = include_str!("mainnet_seed.json");
