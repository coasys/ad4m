use deno_core::error::CoreError;
use deno_core::error::JsError;
use deno_core::error::ModuleLoaderError;
use deno_core::ModuleLoadResponse;
use deno_core::ModuleLoader;
use deno_core::ModuleSource;
use deno_core::ModuleSourceCode;
use deno_core::ModuleSpecifier;
use deno_core::ModuleType;
use deno_core::RequestedModuleType;
use deno_core::ResolutionKind;
use deno_core::SourceCodeCacheInfo;
use deno_error::JsErrorClass;
use deno_lib::util::hash::FastInsecureHasher;
use deno_runtime::transpile::maybe_transpile_source;
use log::info;
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
        Err(e) => Err(ModuleLoaderError::Core(CoreError::Js(JsError {
            name: Some(e.get_class().to_string()),
            message: Some(e.get_message().to_string()),
            stack: None,
            cause: None,
            exception_message: String::new(),
            frames: Vec::new(),
            source_line: None,
            source_line_frame_index: None,
            aggregated: None,
        }))),
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
        let module_specifier = deno_core::resolve_import(specifier, referrer)?;
        Ok(module_specifier)
    }

    fn load(
        &self,
        module_specifier: &ModuleSpecifier,
        _maybe_referrer: std::option::Option<&Url>,
        _is_dyn_import: bool,
        _request_module_type: RequestedModuleType,
    ) -> ModuleLoadResponse {
        match module_specifier.to_file_path() {
            Ok(path) => match std::fs::read_to_string(path) {
                Ok(code) => ModuleLoadResponse::Sync(maybe_transpile(module_specifier, code)),
                Err(e) => {
                    log::error!("Error reading file: {}", e);
                    ModuleLoadResponse::Sync(Err(ModuleLoaderError::NotFound))
                }
            },
            Err(_err) => {
                info!("Module is not a file path, importing as raw module string");
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
                    None => Err(ModuleLoaderError::NotFound),
                })
            }
        }
    }
}
