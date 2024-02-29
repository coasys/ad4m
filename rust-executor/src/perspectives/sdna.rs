use crate::agent;
use crate::types::{DecoratedLinkExpression, ExpressionRef, LanguageRef, Link, LinkExpression};
use ad4m_client::literal::{Literal, LiteralValue};
use chrono::DateTime;
use deno_core::error::AnyError;
use kitsune_p2p_types::dependencies::proptest::prelude::prop;
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use log;

fn triple_fact(l: &DecoratedLinkExpression) -> String {
    format!("triple(\"{}\", \"{}\", \"{}\").", l.data.source, l.data.predicate.as_ref().unwrap_or(&"".to_string()), l.data.target)
}

fn link_fact(l: &DecoratedLinkExpression) -> String {
    format!(
        "link(\"{}\", \"{}\", \"{}\", {}, \"{}\").",
        l.data.source,
        l.data.predicate.as_ref().unwrap_or(&"".to_string()),
        l.data.target,
        DateTime::parse_from_rfc3339(&l.timestamp).unwrap().timestamp_millis(),
        l.author
    )
}

async fn node_facts(all_links: &Vec<&DecoratedLinkExpression>) -> Result<Vec<String>, AnyError> {
    let mut lang_addrs = Vec::new();
    let mut lang_names = Vec::new();
    let mut expr_addrs = Vec::new();

    let mut nodes = HashSet::new();
    for link in all_links.iter() {
        if !link.data.source.is_empty() {
            nodes.insert(link.data.source.clone());
        }
        if let Some(predicate) = link.data.predicate.as_ref() {
            if !predicate.is_empty() {
                nodes.insert(predicate.clone());
            }
        }
        if !link.data.target.is_empty() {
            nodes.insert(link.data.target.clone());
        }
    }

    lang_addrs.push(":- discontiguous(languageAddress/2).".to_string());
    lang_names.push(":- discontiguous(languageName/2).".to_string());
    expr_addrs.push(":- discontiguous(expressionAddress/2).".to_string());

    for node in nodes.iter() {
        match ExpressionRef::try_from(node.clone()) {
            Ok(expression_ref) => {
                let lang = if expression_ref.language.name.is_empty() {
                    //TODO wire up LanguageController
                    //language_controller.language_by_ref(&ref.language).await?
                    LanguageRef {
                        name: "unknown".to_string(),
                        address: "unknown".to_string(),
                    }
                } else {
                    expression_ref.language.clone()
                };

                lang_addrs.push(format!("languageAddress(\"{}\", \"{}\").", node, expression_ref.language.address));
                lang_names.push(format!("languageName(\"{}\", \"{}\").", node, lang.name));
                expr_addrs.push(format!("expressionAddress(\"{}\", \"{}\").", node, expression_ref.expression));
            },
            Err(e) => {
                if !e.to_string().contains("Language not found by reference") {
                    log::debug!("While creating expressionLanguageFacts: {:?}", e);
                }
            }
        }
    }

    Ok(lang_addrs.into_iter().chain(lang_names).chain(expr_addrs).collect())
}


fn is_sdna_link(link: &Link) -> bool {
    link.source == "ad4m://self" && ["ad4m://has_subject_class", "ad4m://has_flow", "ad4m://has_custom_sdna"].contains(&link.predicate.as_deref().unwrap_or(""))
}



pub async fn init_engine_facts(all_links: Vec<DecoratedLinkExpression>, neighbourhood_author: Option<String>) -> Result<Vec<String>, AnyError> {
    let mut lines: Vec<String> = Vec::new();

    // triple/3
    // link/5
    lines.push(":- discontiguous(triple/3).".to_string());
    lines.push(":- discontiguous(link/5).".to_string());
    
    let links_without_sdna: Vec<_> = all_links.iter().filter(|l| !is_sdna_link(&l.data)).collect();

    for link in &links_without_sdna {
        lines.push(triple_fact(link));
    }
    for link in &links_without_sdna {
        lines.push(link_fact(link));
    }

    // reachable/2
    lines.push(":- discontiguous(reachable/2).".to_string());  
    lines.push("reachable(A,B) :- triple(A,_,B).".to_string());
    lines.push("reachable(A,B) :- triple(A,_,X), reachable(X,B).".to_string());

    // hiddenExpression/1
    lines.push(":- discontiguous(hiddenExpression/1).".to_string());

    lines.extend(node_facts(&links_without_sdna).await?);

    // Social DNA zomes
    lines.push(":- discontiguous(register_sdna_flow/2).".to_string());
    lines.push(":- discontiguous(flowable/2).".to_string());
    lines.push(":- discontiguous(flow_state/3).".to_string());
    lines.push(":- discontiguous(start_action/2).".to_string());
    lines.push(":- discontiguous(action/4).".to_string());

    lines.push(":- discontiguous(subject_class/2).".to_string());
    lines.push(":- discontiguous(constructor/2).".to_string());
    lines.push(":- discontiguous(destructor/2).".to_string());
    lines.push(":- discontiguous(instance/2).".to_string());

    lines.push(":- discontiguous(property/2).".to_string());
    lines.push(":- discontiguous(property_getter/4).".to_string());
    lines.push(":- discontiguous(property_setter/3).".to_string());
    lines.push(":- discontiguous(property_resolve/2).".to_string());
    lines.push(":- discontiguous(property_resolve_language/3).".to_string());
    lines.push(":- discontiguous(property_named_option/4).".to_string());
    
    lines.push(":- discontiguous(collection/2).".to_string());
    lines.push(":- discontiguous(collection_getter/4).".to_string());
    lines.push(":- discontiguous(collection_setter/3).".to_string());
    lines.push(":- discontiguous(collection_remover/3).".to_string());
    lines.push(":- discontiguous(collection_adder/3).".to_string());

    lines.push(":- discontiguous(p3_class_icon/2).".to_string());
    lines.push(":- discontiguous(p3_class_color/2).".to_string());
    lines.push(":- discontiguous(p3_instance_color/3).".to_string());

    lines.push(":- use_module(library(lists)).".to_string());

    let lib = r#"
:- discontiguous(paginate/4).
paginate(Data, PageNumber, PageSize, PageData) :-
PageNumber > 0,
PageSize > 0,
length(Data, DataLength),
MaxSkip is max(0, DataLength - PageSize),
SkipCount is min((PageNumber - 1) * PageSize, MaxSkip),
skipN(Data, SkipCount, SkippedData),
takeN(SkippedData, PageSize, PageData).

:- discontiguous(skipN/3).
skipN(Data, 0, Data).
skipN([_|Rest], N, SkippedData) :-
N > 0,
NextN is N - 1,
skipN(Rest, NextN, SkippedData).

:- discontiguous(takeN/3).
takeN(_, 0, []).
takeN([Item|Rest], N, [Item|PageRest]) :-
N > 0,
NextN is N - 1,
takeN(Rest, NextN, PageRest).        
    "#;
    
    lines.extend(lib.split('\n').map(|s| s.to_string()));

    let mut author_agents = vec![agent::did()];
    if let Some(neughbourhood_author) = neighbourhood_author {
        author_agents.push(neughbourhood_author);
    }

    let mut seen_subject_classes = HashMap::new();
    for link_expression in all_links {
        let link = &link_expression.data;

        if link_expression.proof.valid && author_agents.contains(&link_expression.author) {
            if is_sdna_link(link) {
                let name = Literal::from_url(link.target.clone())?
                    .get()
                    .expect("must work")
                    .to_string();

                let entry = seen_subject_classes.entry(name.clone()).or_insert_with(|| HashMap::new());
                entry.insert("type".to_string(), link.predicate.as_ref().expect("sdna link must have predicate").clone());
            }

            if link.predicate == Some("ad4m://sdna".to_string()) {
                let name = Literal::from_url(link.source.clone())?.get().expect("must work").to_string();
                let code = Literal::from_url(link.target.clone())?.get().expect("must work").to_string();

                let subject_class = seen_subject_classes.entry(name.clone()).or_insert_with(|| HashMap::new());
                let existing_timestamp = subject_class.get("timestamp").and_then(|t| t.parse::<i64>().ok());
                let current_timestamp = link_expression.timestamp.parse::<i64>().ok();

                if let (Some(existing), Some(current)) = (existing_timestamp, current_timestamp) {
                    if current > existing {
                        subject_class.insert("code".to_string(), code);
                        subject_class.insert("timestamp".to_string(), link_expression.timestamp.clone());
                    }
                } else {
                    subject_class.insert("code".to_string(), code);
                    subject_class.insert("timestamp".to_string(), link_expression.timestamp.clone());
                }
            }
        }
    }

    for (name, properties) in seen_subject_classes.iter() {
        if let Some(code) = properties.get(&"code".to_string()) {
            lines.extend(code.split('\n').map(|s| s.to_string()));
        }
    }

    Ok(lines)
}
