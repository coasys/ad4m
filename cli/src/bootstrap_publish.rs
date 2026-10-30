use ad4m_client::Ad4mClient;
use colour::green_ln;
use serde::{Deserialize, Serialize};
use std::fs;
use std::process::exit;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct SeedProto {
    #[serde(rename = "languageLanguageRef")]
    pub language_language_ref: String,
    #[serde(rename = "linkLanguages")]
    pub link_languages: Vec<LanguageInstance>,
    #[serde(rename = "agentLanguage")]
    pub agent_language: LanguageInstance,
    #[serde(rename = "neighbourhoodLanguage")]
    pub neighbourhood_language: LanguageInstance,
    #[serde(rename = "perspectiveLanguage")]
    pub perspective_language: LanguageInstance,
    #[serde(rename = "alsoPublish")]
    pub also_publish: Vec<LanguageInstance>,
    /// The seed's `trustedAgents`. The publishing agent must be one of them.
    /// Without the field the seed trusts only the publishing agent.
    #[serde(rename = "trustedAgents", default)]
    pub trusted_agents: Vec<String>,
    /// The files the seed is written to, relative to the working directory.
    #[serde(rename = "seedOutputs", default = "default_seed_outputs")]
    pub seed_outputs: Vec<String>,
}

fn default_seed_outputs() -> Vec<String> {
    vec!["mainnet_seed.json".to_string()]
}

/// The seed's `trustedAgents` for a publish signed by `publisher`. Every
/// language in the seed is signed by the publisher, and a node installs only
/// languages signed by a trusted agent, so a list without the publisher is an
/// error.
pub fn seed_trusted_agents(proto: &[String], publisher: &str) -> Result<Vec<String>, String> {
    if proto.is_empty() {
        return Ok(vec![publisher.to_string()]);
    }
    if !proto.iter().any(|did| did == publisher) {
        return Err(format!(
            "the publishing agent {publisher} is not in the seed proto's trustedAgents {proto:?}: \
             nodes would refuse every language it publishes"
        ));
    }
    Ok(proto.to_vec())
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ProtoMeta {
    pub name: String,
    pub description: String,
    #[serde(rename = "sourceCodeLink")]
    pub source_code_link: String,
    #[serde(rename = "possibleTemplateParams")]
    pub possible_template_params: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct LanguageInstance {
    meta: ProtoMeta,
    resource: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct BootstrapSeed {
    #[serde(rename = "trustedAgents")]
    pub trusted_agents: Vec<String>,
    #[serde(rename = "knownLinkLanguages")]
    pub known_link_languages: Vec<String>,
    #[serde(rename = "directMessageLanguage")]
    pub direct_message_language: String,
    #[serde(rename = "agentLanguage")]
    pub agent_language: String,
    #[serde(rename = "perspectiveLanguage")]
    pub perspective_language: String,
    #[serde(rename = "neighbourhoodLanguage")]
    pub neighbourhood_language: String,
    #[serde(rename = "languageLanguageBundle")]
    pub language_language_bundle: String,
}

//Generates an ad4m client, unlocks the agent and then publishes the languages found in the seed proto.
//After that it will generate a new bootstrap seed and save to the current directory
pub async fn start_publishing(
    admin_credential: String,
    passphrase: String,
    seed_proto: SeedProto,
    language_language_bundle: String,
) {
    let ad4m_client = Ad4mClient::connect("http://localhost:12000".to_string(), admin_credential)
        .await
        .expect("could not connect to executor");

    let agent = ad4m_client
        .agent
        .unlock(passphrase, true)
        .await
        .expect("could not unlock agent");

    if let Some(error) = agent.error {
        println!("Error unlocking agent: {}", error);
        exit(1);
    }

    green_ln!("Unlocked agent\n");

    // Checked before anything is published: a published language can't be
    // taken back.
    let trusted_agents = match seed_trusted_agents(
        &seed_proto.trusted_agents,
        &agent.did.expect("an unlocked agent has a DID"),
    ) {
        Ok(trusted_agents) => trusted_agents,
        Err(error) => {
            colour::red_ln!("Not publishing: {}", error);
            exit(1);
        }
    };

    let languages = vec![
        seed_proto.agent_language,
        seed_proto.perspective_language,
        seed_proto.neighbourhood_language,
    ];

    let mut bootstrap_seed = BootstrapSeed {
        trusted_agents,
        known_link_languages: vec![],
        language_language_bundle,
        direct_message_language: String::from(""),
        agent_language: String::from(""),
        perspective_language: String::from(""),
        neighbourhood_language: String::from(""),
    };

    //Publish the core bootstrap languages
    for (i, language) in languages.into_iter().enumerate() {
        let publish_result = ad4m_client
            .languages
            .publish(
                language.resource,
                Some(language.meta.name.clone()),
                Some(language.meta.description),
                Some(language.meta.possible_template_params),
                Some(language.meta.source_code_link),
            )
            .await
            .expect("Could not publish language");

        match i {
            0 => bootstrap_seed
                .agent_language
                .clone_from(&publish_result.address),
            1 => bootstrap_seed
                .perspective_language
                .clone_from(&publish_result.address),
            2 => bootstrap_seed
                .neighbourhood_language
                .clone_from(&publish_result.address),
            _ => (),
        }
        green_ln!(
            "Published core bootstrap language: {} at address: {}\n",
            language.meta.name,
            publish_result.address
        );
    }

    //Publish the link languages
    for language in seed_proto.link_languages {
        let publish_result = ad4m_client
            .languages
            .publish(
                language.resource,
                Some(language.meta.name.clone()),
                Some(language.meta.description),
                Some(language.meta.possible_template_params),
                Some(language.meta.source_code_link),
            )
            .await
            .expect("Could not publish language");
        bootstrap_seed
            .known_link_languages
            .push(publish_result.address.clone());
        green_ln!(
            "Published link language: {} at address: {}",
            language.meta.name,
            publish_result.address
        );
    }

    //Publish other supplied languages
    for language in seed_proto.also_publish {
        let publish_result = ad4m_client
            .languages
            .publish(
                language.resource,
                Some(language.meta.name.clone()),
                Some(language.meta.description),
                Some(language.meta.possible_template_params),
                Some(language.meta.source_code_link),
            )
            .await
            .expect("Could not publish language");
        green_ln!(
            "Also published language: {} at address: {}",
            language.meta.name,
            publish_result.address
        );
    }

    //Save the bootstrap seed
    let bootstrap_seed_json = serde_json::to_string_pretty(&bootstrap_seed).unwrap();
    for output in &seed_proto.seed_outputs {
        fs::write(output, &bootstrap_seed_json).unwrap();
        green_ln!("Bootstrap seed saved to {}", output);
    }
    green_ln!("\n\nBootstrap seed generated. Finishing...");
    exit(0);
}

#[cfg(test)]
mod tests {
    use super::*;

    const PUBLISHER: &str = "did:key:publisher";

    #[test]
    fn a_proto_without_trusted_agents_trusts_the_publisher() {
        assert_eq!(
            seed_trusted_agents(&[], PUBLISHER).unwrap(),
            vec![PUBLISHER]
        );
    }

    #[test]
    fn the_proto_list_is_kept_whole_when_it_holds_the_publisher() {
        let proto = vec!["did:key:other".to_string(), PUBLISHER.to_string()];
        assert_eq!(seed_trusted_agents(&proto, PUBLISHER).unwrap(), proto);
    }

    #[test]
    fn a_publisher_outside_the_proto_list_is_refused() {
        let proto = vec!["did:key:other".to_string()];
        let error = seed_trusted_agents(&proto, PUBLISHER).unwrap_err();
        assert!(error.contains(PUBLISHER), "{error}");
    }

    /// The repo's seed proto writes both seed copies with the trusted agents
    /// of the committed seed, so a publish keeps them. `globals.rs` compiles
    /// the `rust-executor/src` copy into the binary.
    #[test]
    fn the_repo_seed_proto_matches_the_committed_seed() {
        let proto: SeedProto = serde_json::from_str(include_str!("../seed_proto.json")).unwrap();
        let seed: BootstrapSeed =
            serde_json::from_str(include_str!("../../rust-executor/src/mainnet_seed.json"))
                .unwrap();
        assert_eq!(proto.trusted_agents, seed.trusted_agents);
        assert_eq!(
            proto.seed_outputs,
            vec![
                "mainnet_seed.json",
                "../rust-executor/src/mainnet_seed.json"
            ]
        );
    }

    #[test]
    fn both_seed_copies_are_the_same() {
        assert_eq!(
            include_str!("../mainnet_seed.json"),
            include_str!("../../rust-executor/src/mainnet_seed.json")
        );
    }
}
