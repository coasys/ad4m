use deno_core::error::ModuleLoaderError;
use deno_core::ModuleLoadResponse;
use deno_core::ModuleLoader;
use deno_core::ModuleSource;
use deno_core::ModuleSourceCode;
use deno_core::ModuleSpecifier;
use deno_core::ModuleType;
use deno_core::ResolutionKind;
use deno_core::SourceCodeCacheInfo;
use deno_error::JsErrorClass;
use deno_lib::util::hash::FastInsecureHasher;
use deno_runtime::transpile::maybe_transpile_source;
use std::collections::HashMap;
use url::Url;

fn maybe_transpile(
    module_specifier: &Url,
    code: String,
) -> Result<ModuleSource, ModuleLoaderError> {
    // Handle TypeScript files
    match maybe_transpile_source(module_specifier.to_string().into(), code.into()) {
        Ok((js_code, maybe_source_map)) => {
            let maybe_code_cache = maybe_source_map.map(|code| {
                let code_hash = FastInsecureHasher::new_deno_versioned()
                    .write_hashable(code.clone())
                    .finish();
                SourceCodeCacheInfo {
                    hash: code_hash,
                    data: Some(code),
                }
            });
            Ok(ModuleSource::new(
                ModuleType::JavaScript,
                ModuleSourceCode::String(js_code),
                module_specifier,
                maybe_code_cache,
            ))
        }
        // deno v2.9: ModuleLoaderError = JsErrorBox (type alias). Neither
        // ::Core nor ::NotFound enum variants exist anymore — use the
        // JsErrorBox constructor helpers instead.
        Err(e) => Err(deno_error::JsErrorBox::new(e.get_class(), e.get_message())),
    }
}

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
    ) -> Result<ModuleSpecifier, ModuleLoaderError> {
        // deno v2.9: resolve_import returns ModuleResolutionError which
        // doesn't have a From<..> for JsErrorBox; wrap manually.
        let module_specifier = deno_core::resolve_import(specifier, referrer)
            .map_err(|e| deno_error::JsErrorBox::type_error(e.to_string()))?;
        Ok(module_specifier)
    }

    // deno v2.9 trait signature:
    //   fn load(&self, &url::Url, Option<&ModuleLoadReferrer>, ModuleLoadOptions)
    //     -> ModuleLoadResponse
    // The old (specifier, referrer, is_dyn_import, request_module_type) 4-arg
    // shape was collapsed — dyn_import + request_module_type moved onto the
    // ModuleLoadOptions struct.
    fn load(
        &self,
        module_specifier: &Url,
        _maybe_referrer: std::option::Option<&deno_core::ModuleLoadReferrer>,
        _options: deno_core::ModuleLoadOptions,
    ) -> ModuleLoadResponse {
        match module_specifier.to_file_path() {
            Ok(path) => match std::fs::read_to_string(&path) {
                Ok(code) => ModuleLoadResponse::Sync(maybe_transpile(module_specifier, code)),
                Err(e) => {
                    log::error!("Error reading file {:?}: {}", path, e);
                    // deno v2.9: ModuleLoaderError::NotFound gone. Use
                    // JsErrorBox::new with the standard NotFound class.
                    ModuleLoadResponse::Sync(Err(deno_error::JsErrorBox::new(
                        "NotFound",
                        format!("Module not found: {}", module_specifier),
                    )))
                }
            },
            Err(_err) => {
                let module_code = self.modules.get(module_specifier.as_str()).cloned();

                ModuleLoadResponse::Sync(match module_code {
                    Some(code) => {
                        // Check if the module specifier ends with .ts or .tsx
                        if module_specifier.as_str().ends_with(".ts")
                            || module_specifier.as_str().ends_with(".tsx")
                        {
                            maybe_transpile(module_specifier, code)
                        } else {
                            Ok(ModuleSource::new(
                                deno_core::ModuleType::JavaScript,
                                ModuleSourceCode::String(code.into()),
                                module_specifier,
                                None,
                            ))
                        }
                    }
                    // deno v2.9: ::NotFound variant gone; use JsErrorBox::new.
                    None => Err(deno_error::JsErrorBox::new(
                        "NotFound",
                        format!("Module not found: {}", module_specifier),
                    )),
                })
            }
        }
    }
}
