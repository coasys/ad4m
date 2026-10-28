use ad4m_client::Ad4mClient;
use anyhow::{bail, Context, Result};
use clap::Subcommand;
use rustyline::Editor;
use std::io::IsTerminal;

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
            let language = ad4m_client.languages.by_address(address).await?;
            println!("{:#?}", language);
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

            let interactive = std::io::stdin().is_terminal();
            let mut rl = None;
            let meta = publish_meta(
                PublishMetaArgs {
                    name,
                    description,
                    possible_template_params,
                    source_code_link,
                },
                interactive,
                |prompt| {
                    if rl.is_none() {
                        println!("Please enter meta-information for this language: ");
                        rl = Some(Editor::<()>::new()?);
                    }
                    rl.as_mut()
                        .expect("editor was just created")
                        .readline(prompt)
                        .with_context(|| format!("Could not read `{}` from stdin", prompt.trim()))
                },
            )?;

            let publish_result = ad4m_client
                .languages
                .publish(
                    path,
                    Some(meta.name),
                    meta.description,
                    meta.possible_template_params,
                    meta.source_code_link,
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

struct PublishMetaArgs {
    name: Option<String>,
    description: Option<String>,
    possible_template_params: Option<String>,
    source_code_link: Option<String>,
}

#[derive(Debug, PartialEq)]
struct PublishMeta {
    name: String,
    description: Option<String>,
    possible_template_params: Option<Vec<String>>,
    source_code_link: Option<String>,
}

/// Fills in the meta-information `languages publish` sends. On a terminal, each
/// value not given as a flag is asked for. Without one (a script, CI, a pipe)
/// nothing is asked: `--name` is required and the other values default to empty.
fn publish_meta(
    args: PublishMetaArgs,
    interactive: bool,
    mut ask: impl FnMut(&str) -> Result<String>,
) -> Result<PublishMeta> {
    if !interactive && args.name.is_none() {
        bail!("--name is required when stdin is not a terminal");
    }
    let mut value = |given: Option<String>, prompt: &str| match given {
        Some(v) => Ok(v),
        None if interactive => ask(prompt),
        None => Ok(String::new()),
    };
    let name = value(args.name, "Name (should match name in code): ")?;
    let description = value(args.description, "Description: ")?;
    let possible_template_params = value(
        args.possible_template_params,
        "Template parameters (comma separated list): ",
    )?;
    let source_code_link = value(args.source_code_link, "Source code link: ")?;

    let non_empty = |s: String| if s.is_empty() { None } else { Some(s) };
    Ok(PublishMeta {
        name,
        description: non_empty(description),
        possible_template_params: non_empty(possible_template_params)
            .map(|s| s.split(',').map(|p| p.trim().to_string()).collect()),
        source_code_link: non_empty(source_code_link),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn args(name: Option<&str>) -> PublishMetaArgs {
        PublishMetaArgs {
            name: name.map(String::from),
            description: None,
            possible_template_params: None,
            source_code_link: None,
        }
    }

    #[test]
    fn without_a_terminal_nothing_is_asked_and_missing_values_are_empty() {
        let meta = publish_meta(args(Some("note-store")), false, |prompt| {
            panic!("asked `{}` without a terminal", prompt)
        })
        .unwrap();
        assert_eq!(
            meta,
            PublishMeta {
                name: "note-store".into(),
                description: None,
                possible_template_params: None,
                source_code_link: None,
            }
        );
    }

    #[test]
    fn without_a_terminal_the_name_is_required() {
        let err = publish_meta(args(None), false, |_| Ok("asked".into())).unwrap_err();
        assert!(err.to_string().contains("--name is required"), "{err}");
    }

    #[test]
    fn on_a_terminal_only_the_missing_values_are_asked() {
        let mut asked = vec![];
        let meta = publish_meta(
            PublishMetaArgs {
                name: Some("note-store".into()),
                description: None,
                possible_template_params: Some("uid, name".into()),
                source_code_link: None,
            },
            true,
            |prompt| {
                asked.push(prompt.to_string());
                Ok(if prompt.starts_with("Description") {
                    "Notes".into()
                } else {
                    String::new()
                })
            },
        )
        .unwrap();
        assert_eq!(asked, ["Description: ", "Source code link: "]);
        assert_eq!(meta.description.as_deref(), Some("Notes"));
        assert_eq!(
            meta.possible_template_params,
            Some(vec!["uid".to_string(), "name".to_string()])
        );
        assert_eq!(meta.source_code_link, None);
    }

    #[test]
    fn on_a_terminal_a_failed_read_is_an_error_not_a_panic() {
        let err = publish_meta(args(None), true, |_| bail!("EOF")).unwrap_err();
        assert!(err.to_string().contains("EOF"), "{err}");
    }
}
