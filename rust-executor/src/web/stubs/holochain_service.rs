//! `crate::holochain_service` on wasm32: no Holochain conductor in the web executor. A
//! web executor's config sets `run_holochain = false`; if it does not, the conductor
//! startup fails with [`NotAvailable`](crate::web::stubs::NotAvailable).

pub mod conductor_startup {
    use crate::languages::error::LanguageError;

    pub struct ConductorStartup;

    impl ConductorStartup {
        pub async fn load_core_languages(
            self,
            _language_language_only: bool,
        ) -> Result<(), LanguageError> {
            Err(LanguageError::RuntimeError {
                address: "system languages".to_string(),
                message: crate::web::stubs::NotAvailable("Holochain").to_string(),
            })
        }
    }

    pub fn spawn_conductor_startup(_passphrase: String) -> ConductorStartup {
        ConductorStartup
    }
}
