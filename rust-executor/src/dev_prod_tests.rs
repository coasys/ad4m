#[cfg(test)]
mod tests {
    use crate::config::Ad4mConfig;
    use fs2::FileExt;
    use std::fs;
    use std::path::PathBuf;

    fn temp_dir(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("ad4m-test-{}-{}", name, std::process::id()));
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn test_lockfile_acquire_and_conflict() {
        let dir = temp_dir("lockfile");
        let lock_path = dir.join(".ad4m-lock");

        // First lock should succeed
        let file1 = fs::File::create(&lock_path).unwrap();
        assert!(file1.try_lock_exclusive().is_ok());

        // Second lock should fail
        let file2 = fs::File::open(&lock_path).unwrap();
        assert!(file2.try_lock_exclusive().is_err());

        // Release first lock
        file1.unlock().unwrap();

        // Now second lock should succeed
        assert!(file2.try_lock_exclusive().is_ok());
        file2.unlock().unwrap();

        // Cleanup
        fs::remove_dir_all(&dir).ok();
    }

    #[test]
    fn test_network_mode_parsing() {
        use crate::config::NetworkMode;

        assert_eq!(
            "mainnet".parse::<NetworkMode>().unwrap(),
            NetworkMode::Mainnet
        );
        assert_eq!(
            "devnet".parse::<NetworkMode>().unwrap(),
            NetworkMode::Devnet
        );
        assert_eq!("local".parse::<NetworkMode>().unwrap(), NetworkMode::Local);
        assert_eq!(
            "MAINNET".parse::<NetworkMode>().unwrap(),
            NetworkMode::Mainnet
        );
        assert!("invalid".parse::<NetworkMode>().is_err());
    }

    #[test]
    fn test_production_flag_is_false_by_default() {
        assert!(!crate::globals::IS_PRODUCTION_BUILD);
    }

    #[test]
    fn test_devnet_seed_is_valid_and_uses_real_languages() {
        // Both should be valid JSON
        let mainnet: serde_json::Value =
            serde_json::from_str(crate::globals::MAINNET_JSON).expect("mainnet seed is valid JSON");
        let devnet: serde_json::Value =
            serde_json::from_str(crate::globals::DEVNET_JSON).expect("devnet seed is valid JSON");

        // Devnet uses the same real language addresses as mainnet —
        // isolation comes from separate data directories and Holochain conductors,
        // not from different language binaries.
        assert_eq!(
            mainnet["agentLanguage"], devnet["agentLanguage"],
            "devnet must use real agent language"
        );
        assert_eq!(
            mainnet["perspectiveLanguage"], devnet["perspectiveLanguage"],
            "devnet must use real perspective language"
        );
        assert_eq!(
            mainnet["neighbourhoodLanguage"], devnet["neighbourhoodLanguage"],
            "devnet must use real neighbourhood language"
        );
        assert_eq!(
            mainnet["knownLinkLanguages"], devnet["knownLinkLanguages"],
            "devnet must use real link languages"
        );
        // Language-language bundle must be present (not empty placeholder)
        let bundle = devnet["languageLanguageBundle"].as_str().unwrap_or("");
        assert!(
            bundle.len() > 1000,
            "devnet must include the real language-language bundle"
        );
    }

    #[test]
    fn test_dev_mode_config_defaults() {
        // Verify that when app_data_path is set to ~/.ad4m-dev style path, config accepts it
        let mut config = Ad4mConfig::default();
        config.app_data_path = Some("/tmp/test-ad4m-dev".to_string());
        config.prepare();
        assert_eq!(config.app_data_path.as_deref(), Some("/tmp/test-ad4m-dev"));
    }

    #[test]
    fn test_local_network_mode_config() {
        // Local mode should disable bootstrap and proxy, enable mDNS
        let mut config = Ad4mConfig::default();
        config.hc_use_mdns = Some(true);
        config.hc_use_bootstrap = Some(false);
        config.hc_use_proxy = Some(false);
        config.prepare();
        assert_eq!(config.hc_use_mdns, Some(true));
        assert_eq!(config.hc_use_bootstrap, Some(false));
        assert_eq!(config.hc_use_proxy, Some(false));
    }
}
