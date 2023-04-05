use deno_core::ModuleSource;
use deno_core::ResolutionKind;
use deno_runtime::deno_core::error::AnyError;
use deno_core::{anyhow};
use deno_core::ModuleLoader;
use deno_core::ModuleSpecifier;
use std::collections::HashMap;
use std::pin::Pin;
use deno_core::ModuleSourceFuture;


pub struct StringModuleLoader {
    modules: HashMap<String, String>,
}

impl StringModuleLoader {
    pub fn new() -> Self {
        StringModuleLoader {
            modules: HashMap::new(),
        }
    }

    pub fn add_module(&mut self, specifier: &str, code: &str) {
        self.modules.insert(specifier.to_string(), code.to_string());
    }
}

impl ModuleLoader for StringModuleLoader {
    fn resolve(
        &self,
        specifier: &str,
        referrer: &str,
        _kind: ResolutionKind,
    ) -> Result<ModuleSpecifier, AnyError> {
        let module_specifier = deno_core::resolve_import(specifier, referrer)?;
        Ok(module_specifier)
    }

    fn load(
        &self,
        module_specifier: &ModuleSpecifier,
        _maybe_referrer: Option<ModuleSpecifier>,
        _is_dyn_import: bool,
    ) -> Pin<Box<ModuleSourceFuture>> {
        let module_code = self.modules.get(module_specifier.as_str()).cloned();
        let module_specifier = module_specifier.clone();
        let fut = async move {
            match module_code {
                Some(code) => Ok(ModuleSource {
                    code: code.into(),
                    module_type: deno_core::ModuleType::JavaScript,
                    module_url_specified: module_specifier.clone().to_string(),
                    module_url_found: module_specifier.clone().to_string(),
                }),
                None => Err(anyhow::anyhow!("Module not found: {}", module_specifier)),
            }
        };
        Box::pin(fut)
    }
}
