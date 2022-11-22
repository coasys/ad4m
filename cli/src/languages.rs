use ad4m_client::languages;
use anyhow::{Context, Result};
use clap::Subcommand;
use rustyline::Editor;

#[derive(Debug, Subcommand)]
pub enum LanguageFunctions {
    /// List all languages
    All,
    /// Show information about a language
    ByAddress { address: String },
    /// List all languages that implement a given interface
    ByFilter { filter: String },
    /// Set language settings (JSON) string
    WriteSettings { address: String, settings: String },
    /// Clone a source language, set template parameters and publish the result
    ApplyTemplateAndPublish {
        /// Address of the source language to clone
        source: String,
        /// JSON string of template parameters
        template_data: String,
    },
    /// Publish AD4M Language from local file path
    Publish { path: String },
    /// Show meta information about a language
    Meta { address: String },
    /// Show source code of a language
    Source { address: String },
    /// Uninstall given language
    Remove { address: String },
}

pub async fn run(cap_token: String, command: Option<LanguageFunctions>) -> Result<()> {
    if command.is_none() {
        let all_languages = languages::by_filter(cap_token, "".to_string()).await?;
        for language in all_languages {
            println!("\x1b[36mName: \x1b[97m{}", language.name);
            println!("\x1b[36mAddress: \x1b[97m{}", language.address);

            if let Some(settings) = language.settings {
                if settings != "{}" {
                    println!("\x1b[36mSettings: \x1b[97m{}", settings);
                } else {
                    println!("\x1b[36mSettings: \x1b[90m<empty>");
                }
            } else {
                println!("\x1b[36mSettings: \x1b[90m<undefined>");
            }
            println!()
        }
        return Ok(());
    }

    match command.unwrap() {
        LanguageFunctions::All => {
            let all_perspectives = languages::by_filter(cap_token, "".to_string()).await?;
            println!("{:#?}", all_perspectives);
        }
        LanguageFunctions::ByFilter { filter } => {
            let languages = languages::by_filter(cap_token, filter).await?;
            println!("{:#?}", languages);
        }
        LanguageFunctions::ByAddress { address } => {
            let maybe_language = languages::by_address(cap_token, address).await?;
            if let Some(language) = maybe_language {
                println!("{:#?}", language);
            } else {
                println!("Language not found");
            }
        }
        LanguageFunctions::WriteSettings { address, settings } => {
            languages::write_settings(cap_token, address, settings).await?;
            println!("Language settings written");
        }
        LanguageFunctions::ApplyTemplateAndPublish {
            source,
            template_data,
        } => {
            let new_language =
                languages::apply_template_and_publish(cap_token, source, template_data).await?;
            println!("Language template applied and published!");
            println!("Name: {}", new_language.name);
            println!("Address: {}", new_language.address);
        }
        LanguageFunctions::Meta { address } => {
            let meta = languages::meta(cap_token, address).await?;
            println!("{:#?}", meta);
        }
        LanguageFunctions::Publish { path } => {
            let _ = std::fs::read_to_string(path.clone())
                .with_context(|| format!("Could not read language file `{}`!", path))?;

            println!("Publishing language found in file `{}`...", path);
            println!("Please enter meta-information for this language: ");
            let mut rl = Editor::<()>::new()?;
            let name = rl.readline("Name (should match name in code): ")?;
            let description = rl.readline("Description: ")?;
            let possible_template_params_string =
                rl.readline("Template parameters (comma spearated list): ")?;
            let possible_template_params: Vec<String> = possible_template_params_string
                .split(',')
                .map(|s| s.trim().to_string())
                .collect();
            let source_code_link = rl.readline("Source code link: ")?;

            let description = if description.is_empty() {
                None
            } else {
                Some(description)
            };
            let possible_template_params = if possible_template_params_string.is_empty() {
                None
            } else {
                Some(possible_template_params)
            };
            let source_code_link = if source_code_link.is_empty() {
                None
            } else {
                Some(source_code_link)
            };

            let publish_result = languages::publish(
                cap_token,
                path,
                name,
                description,
                possible_template_params,
                source_code_link,
            )
            .await?;
            println!(
                "Language published with address: {}",
                publish_result.address
            );
        }
        LanguageFunctions::Source { address } => {
            let source = languages::source(cap_token, address).await?;
            println!("{}", source);
        }
        LanguageFunctions::Remove { address } => {
            languages::remove(cap_token, address).await?;
            println!("Language removed");
        }
    };
    Ok(())
}
