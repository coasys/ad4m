use ad4m_client::Ad4mClient;
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
    Publish {
        /// Path to the language file
        path: String,
        /// Name of the language
        #[arg(short, long)]
        name: Option<String>,
        /// Description of the language
        #[arg(short, long)]
        description: Option<String>,
        /// Template parameters (if any) of the language, comma separated list of strings
        #[arg(short, long)]
        possible_template_params: Option<String>,
        /// URL of public repository of the language source code
        #[arg(short, long)]
        source_code_link: Option<String>,
    },
    /// Show meta information about a language
    Meta { address: String },
    /// Show source code of a language
    Source { address: String },
    /// Uninstall given language
    Remove { address: String },
}

pub async fn run(ad4m_client: Ad4mClient, command: Option<LanguageFunctions>) -> Result<()> {
    if command.is_none() {
        let all_languages = ad4m_client.languages.by_filter(None).await?;
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
            let all_perspectives = ad4m_client.languages.by_filter(None).await?;
            println!("{:#?}", all_perspectives);
        }
        LanguageFunctions::ByFilter { filter } => {
            let languages = ad4m_client.languages.by_filter(Some(filter)).await?;
            println!("{:#?}", languages);
        }
        LanguageFunctions::ByAddress { address } => {
            let maybe_language = ad4m_client.languages.by_address(address).await?;
            if let Some(language) = maybe_language {
                println!("{:#?}", language);
            } else {
                println!("Language not found");
            }
        }
        LanguageFunctions::WriteSettings { address, settings } => {
            ad4m_client
                .languages
                .write_settings(address, settings)
                .await?;
            println!("Language settings written");
        }
        LanguageFunctions::ApplyTemplateAndPublish {
            source,
            template_data,
        } => {
            let new_language = ad4m_client
                .languages
                .apply_template_and_publish(source, template_data)
                .await?;
            println!("Language template applied and published!");
            println!("Name: {}", new_language.name);
            println!("Address: {}", new_language.address);
        }
        LanguageFunctions::Meta { address } => {
            let meta = ad4m_client.languages.meta(address).await?;
            println!("{:#?}", meta);
        }
        LanguageFunctions::Publish {
            path,
            name,
            description,
            possible_template_params,
            source_code_link,
        } => {
            let _ = std::fs::read_to_string(path.clone())
                .with_context(|| format!("Could not read language file `{}`!", path))?;

            println!("Publishing language found in file `{}`...", path);

            if name.is_none()
                || description.is_none()
                || possible_template_params.is_none()
                || source_code_link.is_none()
            {
                println!("Please enter meta-information for this language: ");
            };

            let mut rl = Editor::<()>::new()?;
            let name = name.unwrap_or_else(|| {
                rl.readline("Name (should match name in code): ")
                    .expect("Could not read name from stdin!")
            });
            let description = description.unwrap_or_else(|| {
                rl.readline("Description: ")
                    .expect("Could not read description from stdin!")
            });
            let possible_template_params_string = possible_template_params.unwrap_or_else(|| {
                rl.readline("Template parameters (comma spearated list): ")
                    .expect("Could not read template parameters from stdin!")
            });
            let possible_template_params: Vec<String> = possible_template_params_string
                .split(',')
                .map(|s| s.trim().to_string())
                .collect();
            let source_code_link = source_code_link.unwrap_or_else(|| {
                rl.readline("Source code link: ")
                    .expect("Could not read source code link from stdin!")
            });

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

            let publish_result = ad4m_client
                .languages
                .publish(
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
            let source = ad4m_client.languages.source(address).await?;
            print!("{}", source);
        }
        LanguageFunctions::Remove { address } => {
            ad4m_client.languages.remove(address).await?;
            println!("Language removed");
        }
    };
    Ok(())
}
