use crate::{formatting::*, repl::repl_loop, util::maybe_parse_datetime};
use ad4m_client::Ad4mClient;
use anyhow::{anyhow, Context, Result};
use clap::{Args, Subcommand};

#[derive(Args, Debug)]
pub struct QueryLinksArgs {
    /// Perspective ID
    id: String,

    /// Filter by source
    source: Option<String>,

    /// Filter by target
    target: Option<String>,

    /// Filter by predicate
    predicate: Option<String>,

    /// Get only links after this date (fromat: %Y-%m-%dT%H:%M:%S%.fZ)
    #[arg(short, long)]
    from_date: Option<String>,

    /// Get only links before this date (fromat: %Y-%m-%dT%H:%M:%S%.fZ)
    #[arg(short, long)]
    until_date: Option<String>,

    /// Get only the first n links
    #[arg(short, long)]
    limit: Option<f64>,
}

#[derive(Debug, Subcommand)]
pub enum PerspectiveFunctions {
    /// Add a perspective with given name
    Add { name: String },

    /// Remove perspective with given uuid
    Remove { id: String },

    /// Add link to perspective with given uuid
    AddLink {
        id: String,
        source: String,
        target: String,
        predicate: Option<String>,
        status: Option<String>,
    },

    /// Query links from perspective with given uuid
    QueryLinks(QueryLinksArgs),

    /// Retrieve snapshot of perspective with given uuid
    Snapshot { id: String },

    /// Run Prolog / SDNA query on perspective with given uuid
    Infer { id: String, query: String },

    /// Stay connected and print any changes (links added/removed) to the perspective
    Watch { id: String },

    /// Interactive Perspective shell based on Prolog/SDNA runtime
    Repl { id: String },

    /// Set Social DNA of given perspective with SDNA code from file
    AddDna {
        id: String,
        name: String,
        file: String,
        dna_type: String,
    },

    /// Get all defined Subject classes
    SubjectClasses { id: String },

    /// Construct a new Subject instance of given class over given base
    SubjectConstruct {
        id: String,
        class: String,
        base: String,
    },

    /// Get the value of the subject instance's given property
    SubjectGetProperty {
        id: String,
        base: String,
        property: String,
    },

    /// Set the value of the subject instance's given property
    SubjectSetProperty {
        id: String,
        base: String,
        property: String,
        value: String,
    },

    /// Add the given value to the subject instance's collection
    SubjectAddCollection {
        id: String,
        base: String,
        collection: String,
        value: String,
    },
}

pub async fn run(ad4m_client: Ad4mClient, command: Option<PerspectiveFunctions>) -> Result<()> {
    if command.is_none() {
        let all_perspectives = ad4m_client.perspectives.all().await?;
        for perspective in all_perspectives {
            println!("\x1b[36mName: \x1b[97m{}", perspective.name);
            println!("\x1b[36mID: \x1b[97m{}", perspective.uuid);
            if perspective.shared_url.is_some() {
                println!(
                    "\x1b[36mShared URL: \x1b[97m{}",
                    perspective.shared_url.unwrap()
                );
            } else {
                println!("\x1b[36mShared URL: \x1b[90m<not shared as Neighbourhood>");
            }

            if let Some(nh) = perspective.neighbourhood {
                println!(
                    "\x1b[36mNeighbourhood Link-Language: \x1b[97m{}",
                    nh.data.link_language
                );
                if nh.data.meta.links.is_empty() {
                    println!("\x1b[36mNeughbourhood meta: \x1b[90m<empty>");
                } else {
                    println!("\x1b[36mNeughbourhood meta:");
                    for link in nh.data.meta.links {
                        print_link(link.into());
                    }
                }
            }
            println!()
        }

        return Ok(());
    }

    match command.unwrap() {
        PerspectiveFunctions::Add { name } => {
            let new_perspective_id = ad4m_client.perspectives.add(name).await?;
            println!("{}", new_perspective_id);
        }
        PerspectiveFunctions::Remove { id } => {
            ad4m_client.perspectives.remove(id).await?;
        }
        PerspectiveFunctions::AddLink {
            id,
            source,
            target,
            predicate,
            status,
        } => {
            ad4m_client
                .perspectives
                .add_link(id, source, target, predicate, status)
                .await?;
        }
        PerspectiveFunctions::QueryLinks(args) => {
            let from_date = maybe_parse_datetime(args.from_date)?;
            let until_date = maybe_parse_datetime(args.until_date)?;
            let result = ad4m_client
                .perspectives
                .query_links(
                    args.id,
                    args.source,
                    args.target,
                    args.predicate,
                    from_date,
                    until_date,
                    args.limit,
                )
                .await?;
            for link in result {
                print_link(link.into());
            }
        }
        PerspectiveFunctions::Infer { id, query } => {
            let results = ad4m_client.perspectives.infer(id, query).await?;
            print_prolog_results(results)?;
        }
        PerspectiveFunctions::Watch { id } => {
            ad4m_client
                .perspectives
                .watch(
                    id,
                    Box::new(|link| {
                        print_link(link);
                    }),
                )
                .await?;
        }
        PerspectiveFunctions::Snapshot { id } => {
            let result = ad4m_client.perspectives.snapshot(id).await?;
            println!("{:#?}", result);
        }
        PerspectiveFunctions::Repl { id } => {
            //let _ = perspectives::run_watch(cap_token, id);
            repl_loop(ad4m_client.perspectives.get(id).await?).await?;
        }
        PerspectiveFunctions::AddDna {
            id,
            file,
            name,
            dna_type,
        } => {
            let dna = std::fs::read_to_string(file.clone())
                .with_context(|| anyhow!("Could not read provided SDNA file {}", file))?;
            let perspective = ad4m_client.perspectives.get(id).await?;
            perspective.add_dna(name, dna, dna_type).await?;
            println!("SDNA set successfully");
        }
        PerspectiveFunctions::SubjectClasses { id } => {
            let perspective = ad4m_client.perspectives.get(id).await?;
            let classes = perspective.subject_classes().await?;
            println!("{}", classes.join("\n"));
        }
        PerspectiveFunctions::SubjectConstruct { id, class, base } => {
            let perspective = ad4m_client.perspectives.get(id).await?;
            perspective.create_subject(&class, &base).await?;
        }
        PerspectiveFunctions::SubjectGetProperty { id, base, property } => {
            let perspective = ad4m_client.perspectives.get(id).await?;
            let classes = perspective.get_subject_classes(&base).await?;
            for class in &classes {
                if let Ok(subject) = perspective.get_subject(class, &base).await {
                    let props = subject.get_property_values().await?;
                    if let Some(value) = props.get(&property) {
                        println!("{:#?}", value);
                        return Ok(());
                    }
                }
            }

            println!(
                "\x1b[91mNone of the found classes have a property '{}'",
                property
            );
            println!("The found classes are: {}", classes.join(", "));
        }
        PerspectiveFunctions::SubjectSetProperty {
            id,
            base,
            property,
            value,
        } => {
            let perspective = ad4m_client.perspectives.get(id).await?;
            let classes = perspective.get_subject_classes(&base).await?;
            for class in &classes {
                if let Ok(subject) = perspective.get_subject(class, &base).await {
                    if subject.set_property(&property, &value).await.is_ok() {
                        return Ok(());
                    }
                }
            }

            println!(
                "\x1b[91mNone of the found classes have a property '{}'",
                property
            );
            println!("The found classes are: {}", classes.join(", "));
        }
        PerspectiveFunctions::SubjectAddCollection {
            id,
            base,
            collection,
            value,
        } => {
            let perspective = ad4m_client.perspectives.get(id).await?;
            let classes = perspective.get_subject_classes(&base).await?;
            for class in &classes {
                if let Ok(subject) = perspective.get_subject(class, &base).await {
                    if subject.add_collection(&collection, &value).await.is_ok() {
                        return Ok(());
                    }
                }
            }

            println!(
                "\x1b[91mNone of the found classes have a collection '{}'",
                collection
            );
            println!("The found classes are: {}", classes.join(", "));
        }
    }
    Ok(())
}
