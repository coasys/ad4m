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
    #[serde(rename = "directMessageLanguage")]
    pub direct_message_language: LanguageInstance,
    #[serde(rename = "neighbourhoodLanguage")]
    pub neighbourhood_language: LanguageInstance,
    #[serde(rename = "perspectiveLanguage")]
    pub perspective_language: LanguageInstance,
    #[serde(rename = "alsoPublish")]
    pub also_publish: Vec<LanguageInstance>,
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
    passphrase: String,
    seed_proto: SeedProto,
    language_language_bundle: String,
) {
    let ad4m_client = Ad4mClient::new("http://localhost:4000/graphql".to_string(), "".to_string());

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

    let languages = vec![
        seed_proto.agent_language,
        seed_proto.direct_message_language,
        seed_proto.perspective_language,
        seed_proto.neighbourhood_language,
    ];

    let mut bootstrap_seed = BootstrapSeed {
        trusted_agents: vec![agent.did.unwrap()],
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
                language.meta.name.clone(),
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
                .direct_message_language
                .clone_from(&publish_result.address),
            2 => bootstrap_seed
                .perspective_language
                .clone_from(&publish_result.address),
            3 => bootstrap_seed
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
                language.meta.name.clone(),
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
                language.meta.name.clone(),
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
    fs::write("mainnet_seed.json", bootstrap_seed_json).unwrap();
    green_ln!("\n\nBootstrap seed generated and saved to mainnet_seed.json.. Finishing...");
    exit(0);
}
