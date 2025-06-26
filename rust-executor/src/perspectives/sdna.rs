use crate::agent;
use crate::types::{DecoratedLinkExpression, ExpressionRef, LanguageRef, Link};
use ad4m_client::literal::Literal;
use chrono::DateTime;
use deno_core::error::AnyError;
use log;
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;

fn triple_fact(l: &DecoratedLinkExpression) -> String {
    format!(
        "triple(\"{}\", \"{}\", \"{}\")",
        l.data.source,
        l.data.predicate.as_ref().unwrap_or(&"".to_string()),
        l.data.target
    )
}

fn link_fact(l: &DecoratedLinkExpression) -> String {
    generic_link_fact("link", l)
}

pub fn generic_link_fact(predicate_name: &str, l: &DecoratedLinkExpression) -> String {
    format!(
        "{}(\"{}\", \"{}\", \"{}\", {}, \"{}\")",
        predicate_name,
        l.data.source,
        l.data.predicate.as_ref().unwrap_or(&"".to_string()),
        l.data.target,
        DateTime::parse_from_rfc3339(&l.timestamp)
            .unwrap()
            .timestamp_millis(),
        l.author
    )
}

async fn _node_facts(all_links: &[&DecoratedLinkExpression]) -> Result<Vec<String>, AnyError> {
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
        if node == "false" || node == "true" {
            continue;
        }
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

                lang_addrs.push(format!(
                    "languageAddress(\"{}\", \"{}\").",
                    node, expression_ref.language.address
                ));
                lang_names.push(format!("languageName(\"{}\", \"{}\").", node, lang.name));
                expr_addrs.push(format!(
                    "expressionAddress(\"{}\", \"{}\").",
                    node, expression_ref.expression
                ));
            }
            Err(e) => {
                if !e.to_string().contains("Language not found by reference") {
                    log::debug!("While creating expressionLanguageFacts: {:?}", e);
                }
            }
        }
    }

    Ok(lang_addrs
        .into_iter()
        .chain(lang_names)
        .chain(expr_addrs)
        .collect())
}

pub fn is_sdna_link(link: &Link) -> bool {
    link.source == "ad4m://self"
        && [
            "ad4m://has_subject_class",
            "ad4m://has_flow",
            "ad4m://has_custom_sdna",
        ]
        .contains(&link.predicate.as_deref().unwrap_or(""))
}

/// Get static infrastructure facts that are the same for all engines
/// This includes setup directives, library imports, and built-in predicates
pub fn get_static_infrastructure_facts() -> Vec<String> {
    let mut lines: Vec<String> = vec![
        // triple/3
        // link/5
        ":- discontiguous(triple/3).".to_string(),
        ":- discontiguous(link/5).".to_string(),
        ":- dynamic(triple/3).".to_string(),
        ":- dynamic(link/5).".to_string(),
    ];

    // reachable/2
    lines.push(":- discontiguous(reachable/2).".to_string());
    lines.push("reachable(A,B) :- triple(A,_,B).".to_string());
    lines.push("reachable(A,B) :- triple(A,_,X), reachable(X,B).".to_string());

    // hiddenExpression/1
    lines.push(":- discontiguous(hiddenExpression/1).".to_string());

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

    // library modules
    lines.push(":- use_module(library(lists)).".to_string());
    lines.push(":- use_module(library(dcgs)).".to_string());
    lines.push(":- use_module(library(charsio)).".to_string());
    lines.push(":- use_module(library(format)).".to_string());
    lines.push(":- use_module(library(assoc)).".to_string());
    lines.push(":- use_module(library(dif)).".to_string());

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
takeN([], _, []).  % Base case: If the list is empty, return an empty list.
takeN(_, 0, []).   % Base case: If N is 0, return an empty list.
takeN([Item|Rest], N, [Item|PageRest]) :-
N > 0,
NextN is N - 1,
takeN(Rest, NextN, PageRest).
    "#;

    lines.extend(lib.split('\n').map(|s| s.to_string()));

    let sorting_predicates = r#"
:- discontiguous(decorate/4).
decorate(List, SortKey, Direction, Decorated) :-
    maplist(decorate_item(SortKey), List, Decorated),
    (Direction == "ASC" ; Direction == "DESC").

:- discontiguous(decorate_item/2).
decorate_item("timestamp", [Base, Properties, Collections, Timestamp, Author], [Timestamp, Base, Properties, Collections, Timestamp, Author]) :- !.
decorate_item(PropertyKey, [Base, Properties, Collections, Timestamp, Author], [KeyValue, Base, Properties, Collections, Timestamp, Author]) :-
    member([PropertyKey, KeyValue, _], Properties),
    !.
decorate_item(_, Item, [0|Item]).  % Default to 0 if key not found

:- discontiguous(undecorate/2).
undecorate(Decorated, Undecorated) :-
    maplist(arg(2), Decorated, Undecorated).

:- discontiguous(merge_sort/3).
merge_sort([], [], _).
merge_sort([X], [X], _).
merge_sort(List, Sorted, Direction) :-
    length(List, Len),
    Len > 1,
    split(List, Left, Right),
    merge_sort(Left, SortedLeft, Direction),
    merge_sort(Right, SortedRight, Direction),
    merge(SortedLeft, SortedRight, Sorted, Direction).

:- discontiguous(split/3).
split(List, Left, Right) :-
    length(List, Len),
    Half is Len // 2,
    length(Left, Half),
    append(Left, Right, List).

:- discontiguous(merge/4).
merge([], Right, Right, _).
merge(Left, [], Left, _).
merge([X|Xs], [Y|Ys], [X|Merged], "ASC") :-
    X = [KeyX|_], Y = [KeyY|_],
    KeyX =< KeyY,
    merge(Xs, [Y|Ys], Merged, "ASC").
merge([X|Xs], [Y|Ys], [Y|Merged], "ASC") :-
    X = [KeyX|_], Y = [KeyY|_],
    KeyX > KeyY,
    merge([X|Xs], Ys, Merged, "ASC").
merge([X|Xs], [Y|Ys], [X|Merged], "DESC") :-
    X = [KeyX|_], Y = [KeyY|_],
    KeyX >= KeyY,
    merge(Xs, [Y|Ys], Merged, "DESC").
merge([X|Xs], [Y|Ys], [Y|Merged], "DESC") :-
    X = [KeyX|_], Y = [KeyY|_],
    KeyX < KeyY,
    merge([X|Xs], Ys, Merged, "DESC").

% New sort_instances predicate
:- discontiguous(sort_instances/4).
sort_instances(UnsortedInstances, SortKey, Direction, SortedInstances) :-
    decorate(UnsortedInstances, SortKey, Direction, Decorated),
    merge_sort(Decorated, SortedDecorated, Direction),
    undecorate(SortedDecorated, SortedInstances).
"#;

    lines.extend(sorting_predicates.split('\n').map(|s| s.to_string()));

    let literal_html_string_predicates = r#"
% Main predicate to remove HTML tags
remove_html_tags(Input, Output) :-
    phrase(strip_html(Output), Input).

% DCG rule to strip HTML tags
strip_html([]) --> [].
strip_html(Result) -->
    "<", !, skip_tag, strip_html(Result).
strip_html([Char|Result]) -->
    [Char],
    strip_html(Result).

% DCG rule to skip HTML tags
skip_tag --> ">", !.
skip_tag --> [_], skip_tag.

% Main predicate to check if Substring is included in String
string_includes(String, Substring) :-
    phrase((..., string(Substring), ...), String).

% DCG rule for any sequence of characters
... --> [].
... --> [_], ... .

% DCG rule for matching a specific string
string([]) --> [].
string([C|Cs]) --> [C], string(Cs).


literal_from_url(Url, Decoded, Scheme) :-
    phrase(parse_url(Scheme, Encoded), Url),
    phrase(url_decode(StringValue), Encoded),
    (   Scheme = number 
    ->  number_chars(Decoded, StringValue)
    ;   (   Scheme = boolean
        ->  (   StringValue = "true"
            ->  Decoded = true
            ;   StringValue = "false"
            ->  Decoded = false
            ;   Decoded = false  % Default for invalid boolean
            )
        ;   Decoded = StringValue  % For all other schemes
        )
    ).

% DCG rule to parse the URL
parse_url(Scheme, Encoded) -->
    "literal://", scheme(Scheme), ":", string(Encoded).

scheme(string) --> "string".
scheme(number) --> "number".
scheme(boolean) --> "boolean".
scheme(json) --> "json".

url_decode([]) --> [].
url_decode([H|T]) --> url_decode_char(H), url_decode(T).

url_decode_char(' ') --> "%20".
url_decode_char('!') --> "%21".
url_decode_char('"') --> "%22".
url_decode_char('#') --> "%23".
url_decode_char('$') --> "%24".
url_decode_char('%') --> "%25".
url_decode_char('&') --> "%26".
url_decode_char('\'') --> "%27".
url_decode_char('(') --> "%28".
url_decode_char(')') --> "%29".
url_decode_char('*') --> "%2A".
url_decode_char('+') --> "%2B".
url_decode_char(',') --> "%2C".
url_decode_char('/') --> "%2F".
url_decode_char(':') --> "%3A".
url_decode_char(';') --> "%3B".
url_decode_char('=') --> "%3D".
url_decode_char('?') --> "%3F".
url_decode_char('@') --> "%40".
url_decode_char('[') --> "%5B".
url_decode_char(']') --> "%5D".
url_decode_char('{') --> "%7B".
url_decode_char('}') --> "%7D".
url_decode_char('<') --> "%3C".
url_decode_char('>') --> "%3E".
url_decode_char('\\') --> "%5C".
url_decode_char('^') --> "%5E".
url_decode_char('_') --> "%5F".
url_decode_char('|') --> "%7C".
url_decode_char('~') --> "%7E".
url_decode_char('`') --> "%60".
url_decode_char('-') --> "%2D".
url_decode_char('.') --> "%2E".

url_decode_char(Char) --> [Char], { \+ member(Char, "%") }.
    "#;

    lines.extend(
        literal_html_string_predicates
            .split('\n')
            .map(|s| s.to_string()),
    );

    let json_parser = r#"
    % Main predicate to parse JSON and extract a property
    json_property(JsonString, Property, Value) :-
        phrase(json_dict(Dict), JsonString),
        get_assoc(Property, Dict, Value).
    
    % DCG rules to parse JSON
    json_dict(Dict) -->
        ws, "{", ws, key_value_pairs(Pairs), ws, "}", ws,
        { list_to_assoc(Pairs, Dict) }.
    
    key_value_pairs([Key-Value|Pairs]) -->
        ws, json_string(Key), ws, ":", ws, json_value(Value), ws, ("," -> key_value_pairs(Pairs) ; {Pairs=[]}).
    
    json_value(Value) --> json_dict(Value).
    json_value(Value) --> json_array(Value).
    json_value(Value) --> json_string(Value).
    json_value(Value) --> json_number(Value).
    json_value(true) --> "true".
    json_value(false) --> "false".
    json_value(null) --> "null".
    
    json_array([Value|Values]) -->
        "[", ws, json_value(Value), ws, ("," -> json_value_list(Values) ; {Values=[]}), ws, "]".
    json_value_list([Value|Values]) --> json_value(Value), ws, ("," -> json_value_list(Values) ; {Values=[]}).
    
    json_string(String) -->
    "\"", json_string_chars(String), "\"".

    json_string_chars([]) --> [].
    json_string_chars([C|Cs]) --> json_string_char(C), json_string_chars(Cs).

    json_string_char(C) --> [C], { dif(C, '"'), dif(C, '\\') }.
    json_string_char('"') --> ['\\', '"'].
    json_string_char('\\') --> ['\\', '\\'].
    json_string_char('/') --> ['\\', '/'].
    json_string_char('\b') --> ['\\', 'b'].
    json_string_char('\f') --> ['\\', 'f'].
    json_string_char('\n') --> ['\\', 'n'].
    json_string_char('\r') --> ['\\', 'r'].
    json_string_char('\t') --> ['\\', 't'].
    
    json_number(Number) -->
        number_sequence(Chars),
        { number_chars(Number, Chars) }.
    
    string_chars([]) --> [].
    string_chars([C|Cs]) --> [C], { dif(C, '"') }, string_chars(Cs).
    
    % Simplified number_sequence to handle both integer and fractional parts
    number_sequence([D|Ds]) --> digit(D), number_sequence_rest(Ds).
    number_sequence_rest([D|Ds]) --> digit(D), number_sequence_rest(Ds).
    number_sequence_rest([]) --> [].
    
    digit(D) --> [D], { member(D, "0123456789.") }.
    
    ws --> ws_char, ws.
    ws --> [].
    
    ws_char --> [C], { C = ' ' ; C = '\t' ; C = '\n' ; C = '\r' }.
    "#;

    lines.extend(json_parser.split('\n').map(|s| s.to_string()));

    let resolve_property = r#"
    % Retrieve a property from a subject class
    % If the property is resolvable, this will try to do the resolution here
    % which works if it is a literal, otherwise it will pass through the URI to JS
    % to be resolved later
    % Resolve is a boolean that tells JS whether to resolve the property or not
    % If it is false, then the property value is a literal and we did resolve it here
    % If it is true, then the property value is a URI and it still needs to be resolved
    resolve_property(SubjectClass, Base, PropertyName, PropertyValue, Resolve) :-
        % Get the property name and resolve boolean
        property(SubjectClass, PropertyName),
        property_getter(SubjectClass, Base, PropertyName, PropertyUri),
        ( property_resolve(SubjectClass, PropertyName)
            % If the property is resolvable, try to resolve it
            -> (
            append("literal://", _, PropertyUri)
            % If the property is a literal, we can resolve it here
            -> (
                % so tell JS to not resolve it
                Resolve = false,
                literal_from_url(PropertyUri, LiteralValue, Scheme),
                (
                json_property(LiteralValue, "data", Data)
                % If it is a JSON literal, and it has a 'data' field, use that
                -> PropertyValue = Data
                % Otherwise, just use the literal value
                ; PropertyValue = LiteralValue 
                )
            )
            ;
            % else (it should be resolved but is not a literal),
            % pass through URI to JS and tell JS to resolve it
            (Resolve = true, PropertyValue = PropertyUri)
            )
            ;
            % else (no property resolve), just return the URI as the value
            (Resolve = false, PropertyValue = PropertyUri)
        )."#;

    lines.extend(resolve_property.split('\n').map(|s| s.to_string()));

    let assert_link = r#"
    assert_link(Source, Predicate, Target, Timestamp, Author) :-
        \+ link(Source, Predicate, Target, Timestamp, Author),
        assertz(link(Source, Predicate, Target, Timestamp, Author)).

    assert_triple(Source, Predicate, Target) :-
        \+ triple(Source, Predicate, Target),
        assertz(triple(Source, Predicate, Target)).

    assert_link_and_triple(Source, Predicate, Target, Timestamp, Author) :-
        (assert_link(Source, Predicate, Target, Timestamp, Author) ; true),
        (assert_triple(Source, Predicate, Target) ; true).
"#;
    lines.extend(assert_link.split('\n').map(|s| s.to_string()));

    lines.push(format!("agent_did(\"{}\").", agent::did()));

    lines
}

/// Get just the data facts (triple and link facts) from the links
pub fn get_data_facts(links: &[DecoratedLinkExpression]) -> Vec<String> {
    let mut lines = Vec::new();
    
    let links_without_sdna: Vec<_> = links
        .iter()
        .filter(|l| !is_sdna_link(&l.data))
        .collect();

    for link in &links_without_sdna {
        lines.push(format!("{}.", triple_fact(link)));
    }
    for link in &links_without_sdna {
        lines.push(format!("{}.", link_fact(link)));
    }
    
    lines
}

/// Get SDNA facts extracted from the links
pub fn get_sdna_facts(
    all_links: &[DecoratedLinkExpression],
    neighbourhood_author: Option<String>,
) -> Result<Vec<String>, AnyError> {
    let mut lines = Vec::new();

    let mut author_agents = vec![agent::did()];
    if let Some(neughbourhood_author) = neighbourhood_author {
        author_agents.push(neughbourhood_author);
    }

    let mut seen_subject_classes = HashMap::new();
    for link_expression in all_links {
        let link = &link_expression.data;

        if link_expression.proof.valid.unwrap_or(false)
            && author_agents.contains(&link_expression.author)
        {
            if is_sdna_link(link) {
                let name = Literal::from_url(link.target.clone())?
                    .get()
                    .expect("must work")
                    .to_string();

                let entry = seen_subject_classes
                    .entry(name.clone())
                    .or_insert_with(HashMap::new);
                entry.insert(
                    "type".to_string(),
                    link.predicate
                        .as_ref()
                        .expect("sdna link must have predicate")
                        .clone(),
                );
            }

            if link.predicate == Some("ad4m://sdna".to_string()) {
                let name = Literal::from_url(link.source.clone())?
                    .get()
                    .expect("must work")
                    .to_string();
                let code = Literal::from_url(link.target.clone())?
                    .get()
                    .expect("must work")
                    .to_string();

                let subject_class = seen_subject_classes
                    .entry(name.clone())
                    .or_insert_with(HashMap::new);
                let existing_timestamp = subject_class
                    .get("timestamp")
                    .and_then(|t| t.parse::<i64>().ok());
                let current_timestamp = link_expression.timestamp.parse::<i64>().ok();

                if let (Some(existing), Some(current)) = (existing_timestamp, current_timestamp) {
                    if current > existing {
                        subject_class.insert("code".to_string(), code);
                        subject_class
                            .insert("timestamp".to_string(), link_expression.timestamp.clone());
                    }
                } else {
                    subject_class.insert("code".to_string(), code);
                    subject_class
                        .insert("timestamp".to_string(), link_expression.timestamp.clone());
                }
            }
        }
    }

    for (_name, properties) in seen_subject_classes.iter() {
        if let Some(code) = properties.get(&"code".to_string()) {
            lines.extend(code.split('\n').map(|s| s.to_string()));
        }
    }

    Ok(lines)
}

pub async fn init_engine_facts(
    all_links: Vec<DecoratedLinkExpression>,
    neighbourhood_author: Option<String>,
) -> Result<Vec<String>, AnyError> {
    let mut lines = get_static_infrastructure_facts();
    
    // Add data facts
    lines.extend(get_data_facts(&all_links));
    
    // Add SDNA facts
    lines.extend(get_sdna_facts(&all_links, neighbourhood_author)?);

    Ok(lines)
}
