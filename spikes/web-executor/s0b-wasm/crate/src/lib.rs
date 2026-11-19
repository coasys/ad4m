//! S0b step 1: Oxigraph in-memory store with AD4M's reified-link shape
//! (rust-executor/src/perspectives/sparql_store.rs `write_link_quads_proof`).
use oxigraph::model::*;
use oxigraph::sparql::{QueryResults, SparqlEvaluator};
use oxigraph::store::Store;

const RDF_REIFIES: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies";
const XSD_INTEGER: &str = "http://www.w3.org/2001/XMLSchema#integer";
pub const LINKS_PER_TODO: usize = 5;
pub const CHANNELS: usize = 100;

/// One synthetic link: (source, predicate, target term, author, timestamp).
fn link(i: usize) -> (String, &'static str, Term) {
    let todo = i / LINKS_PER_TODO;
    let s = format!("todo://t{todo}");
    match i % LINKS_PER_TODO {
        0 => (s, "ad4m://type", NamedNode::new_unchecked("todo://Todo").into()),
        1 => (
            s,
            "todo://state",
            NamedNode::new_unchecked(if todo % 3 == 0 { "todo://done" } else { "todo://ready" }).into(),
        ),
        // `literal:string:` targets are stored as simple literals.
        2 => (s, "todo://title", Literal::new_simple_literal(format!("Task number {} {}", (todo * 7919) % 10007, "lorem ipsum")).into()),
        // `literal:number:` integers are xsd:integer typed literals.
        3 => (s, "todo://priority", Literal::new_typed_literal(format!("{}", todo % 5), NamedNode::new_unchecked(XSD_INTEGER)).into()),
        // The parent edge: channel ad4m://has_child todo.
        _ => (format!("flux://ch{}", todo % CHANNELS), "ad4m://has_child", NamedNode::new_unchecked(s).into()),
    }
}

fn hex(n: u64, len: usize) -> String {
    let mut out = String::with_capacity(len);
    let mut x = n.wrapping_mul(0x9E3779B97F4A7C15) | 1;
    while out.len() < len {
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        out.push_str(&format!("{:016x}", x));
    }
    out.truncate(len);
    out
}

/// The quads of link `i`, as `write_link_quads_proof` writes them: direct
/// triple, reifier `rdf:reifies <<( s p o )>>`, and six annotations
/// (author, timestamp, status, proofKey, proofSignature, proofValid).
fn link_quads(i: usize) -> Vec<Quad> {
    let (s, p, o) = link(i);
    let s = NamedNode::new_unchecked(s);
    let p = NamedNode::new_unchecked(p);
    let reifier = NamedNode::new_unchecked(format!("link:{}", hex(i as u64, 32)));
    let author = format!("did:key:z6Mk{}", hex((i % 5) as u64 + 1_000_000, 44));
    // Distinct, increasing ISO timestamps (one per second from a fixed start).
    let secs = i as u64;
    let ts = format!(
        "2026-{:02}-{:02}T{:02}:{:02}:{:02}.000Z",
        1 + (secs / 2_419_200) % 12,
        1 + (secs / 86_400) % 28,
        (secs / 3600) % 24,
        (secs / 60) % 60,
        secs % 60
    );
    let g = GraphName::DefaultGraph;
    let lit = |v: &str| Term::from(Literal::new_simple_literal(v));
    let ann = |pred: &str, v: Term| Quad::new(reifier.clone(), NamedNode::new_unchecked(pred), v, g.clone());
    vec![
        Quad::new(s.clone(), p.clone(), o.clone(), g.clone()),
        Quad::new(reifier.clone(), NamedNode::new_unchecked(RDF_REIFIES), Triple::new(s, p, o), g.clone()),
        ann("ad4m://ontology/author", lit(&author)),
        ann("ad4m://ontology/timestamp", lit(&ts)),
        ann("ad4m://ontology/status", lit("shared")),
        ann("ad4m://ontology/proofKey", lit(&format!("{author}#{}", hex(i as u64 + 7, 44)))),
        ann("ad4m://ontology/proofSignature", lit(&hex(i as u64 + 13, 128))),
        ann("ad4m://ontology/proofValid", lit("true")),
    ]
}

/// Load `n` links. `per_link` = one transaction per link (the executor's
/// `add_link` path); otherwise one transaction per 1000 links (`add_links`).
pub fn load(store: &Store, n: usize, per_link: bool) {
    let batch = if per_link { 1 } else { 1000 };
    let mut i = 0;
    while i < n {
        let end = (i + batch).min(n);
        let mut tx = store.start_transaction().unwrap();
        for j in i..end {
            for q in link_quads(j) {
                tx.insert(&q);
            }
        }
        tx.commit().unwrap();
        i = end;
    }
}

/// `parse_literal`: the executor's custom function; for plain literals it is
/// the literal itself.
fn parse_literal_fn(args: &[Term]) -> Option<Term> {
    match args.first()? {
        Term::Literal(l) => Some(Term::Literal(l.clone())),
        _ => None,
    }
}

pub fn queries(hydrate_ids: &[String]) -> Vec<(&'static str, String)> {
    let values = hydrate_ids.iter().map(|s| format!("<{s}>")).collect::<Vec<_>>().join(" ");
    vec![
        // Q1: model_query TwoPhase id phase, sort by timestamp
        // (sparql_builder.rs `SortKey::Timestamp` + `build_timestamp_probe`, flag
        // property `ad4m://type = todo://Todo`, required property `todo://state`).
        ("Q1 id-phase ts-sort LIMIT 50", r#"SELECT DISTINCT ?source (MIN(?_first_ts_v) AS ?_first_ts) WHERE {
            ?source <ad4m://type> <todo://Todo> . ?_r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source <ad4m://type> <todo://Todo> )>> . ?_r <ad4m://ontology/timestamp> ?_first_ts_v .
    ?source <ad4m://type> <todo://Todo> .
    ?source <todo://state> ?_cf_state .
        } GROUP BY ?source
    ORDER BY DESC(?_first_ts) ASC(?source)
    LIMIT 50"#.to_string()),
        // Q2: hydration phase, `decorated_links_query` with a VALUES block of the page ids.
        ("Q2 hydrate 50 ids (decorated links)", format!(r#"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT ?source ?predicate ?target ?wireTarget ?author ?timestamp ?proofKey ?proofSig ?proofValid ?status WHERE {{
    VALUES ?source {{ {values} }}
    ?source ?predicate ?target .
    ?reifier rdf:reifies <<( ?source ?predicate ?target )>> .
    FILTER(isIRI(?source) && (true))
    ?reifier <ad4m://ontology/author> ?author .
    ?reifier <ad4m://ontology/timestamp> ?timestamp .
    OPTIONAL {{ ?reifier <ad4m://ontology/proofKey> ?proofKey . }}
    OPTIONAL {{ ?reifier <ad4m://ontology/proofSignature> ?proofSig . }}
    OPTIONAL {{ ?reifier <ad4m://ontology/proofValid> ?proofValid . }}
    OPTIONAL {{ ?reifier <ad4m://ontology/status> ?status . }}
    OPTIONAL {{ ?reifier <ad4m://ontology/wireTarget> ?wireTarget . }}
}}"#)),
        // Q3: TwoPhase id phase, `SortKey::Property(title)` with a where filter state = done.
        ("Q3 id-phase where+property-sort LIMIT 50", r#"SELECT DISTINCT ?source (SAMPLE(?_nv) AS ?_sort_num) (SAMPLE(?_sv) AS ?_sort_str) WHERE {
    ?source <ad4m://type> <todo://Todo> .
    ?source <todo://state> <todo://done> .
            OPTIONAL { ?source <todo://title> ?_sort_raw . BIND(STR(<ad4m://fn/parse_literal>(?_sort_raw)) AS ?_sv) BIND(<http://www.w3.org/2001/XMLSchema#double>(STR(<ad4m://fn/parse_literal>(?_sort_raw))) AS ?_nv) }
        } GROUP BY ?source
    ORDER BY ASC(IF(BOUND(?_sort_str), 0, 1)) ASC(?_sort_num) ASC(?_sort_str) ASC(?source)
    LIMIT 50"#.to_string()),
        // Q4: `build_count_sparql` with the same where filter.
        ("Q4 count where state=done", r#"SELECT (COUNT(DISTINCT ?source) AS ?cnt) WHERE {
    ?source <ad4m://type> <todo://Todo> .
    ?source <todo://state> <todo://done> .
}"#.to_string()),
        // Q5: `InstanceQueryPlan::Single` (no pagination): every link of every
        // conforming instance, joined to its reifier. The heavy one.
        ("Q5 single-plan all instances+links", r#"SELECT DISTINCT ?source ?predicate ?target ?author ?timestamp WHERE {
    ?source <ad4m://type> <todo://Todo> .
    ?source <todo://state> ?_cf_state .
    ?source ?predicate ?target .
    ?_reifier <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?source ?predicate ?target )>> .
    FILTER(isIRI(?source) && isIRI(?predicate))
    ?_reifier <ad4m://ontology/author> ?author .
    ?_reifier <ad4m://ontology/timestamp> ?timestamp .
}"#.to_string()),
        // Q6: real test query from sparql_store.rs tests (flux recentConversations shape),
        // adapted to has_child -> todo.
        ("Q6 channel last-activity GROUP BY (sparql_store test shape)", r#"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT ?channelId (MAX(?ts) AS ?lastActivity) (COUNT(?item) AS ?n) WHERE {
    ?channelId <ad4m://has_child> ?item .
    ?_itemReifier rdf:reifies <<( ?channelId <ad4m://has_child> ?item )>> .
    ?_itemReifier <ad4m://ontology/timestamp> ?ts .
    ?item <ad4m://type> <todo://Todo> .
}
GROUP BY ?channelId
ORDER BY DESC(?lastActivity)
LIMIT 20"#.to_string()),
    ]
}

/// Run a SELECT and return the row count (fully drains the iterator).
pub fn run(store: &Store, q: &str) -> usize {
    let res = SparqlEvaluator::new()
        .with_custom_function(NamedNode::new_unchecked("ad4m://fn/parse_literal"), parse_literal_fn)
        .parse_query(q)
        .unwrap()
        .on_store(store)
        .execute()
        .unwrap();
    match res {
        QueryResults::Solutions(sols) => sols.map(|s| s.unwrap()).count(),
        _ => 0,
    }
}

/// Q1's result ids, fed into Q2.
pub fn page_ids(store: &Store) -> Vec<String> {
    let q = &queries(&[])[0].1;
    let res = SparqlEvaluator::new().parse_query(q).unwrap().on_store(store).execute().unwrap();
    let mut out = vec![];
    if let QueryResults::Solutions(sols) = res {
        for s in sols {
            if let Some(Term::NamedNode(n)) = s.unwrap().get("source") {
                out.push(n.as_str().to_string());
            }
        }
    }
    out
}

#[cfg(target_arch = "wasm32")]
mod wasm {
    use super::*;
    use std::cell::RefCell;
    use wasm_bindgen::prelude::*;

    thread_local! {
        static STORE: RefCell<Option<Store>> = const { RefCell::new(None) };
        static QS: RefCell<Vec<(&'static str, String)>> = const { RefCell::new(Vec::new()) };
    }

    #[wasm_bindgen]
    pub fn memory_bytes() -> f64 {
        (core::arch::wasm32::memory_size(0) * 65536) as f64
    }

    #[wasm_bindgen]
    pub fn reset() {
        STORE.with(|s| *s.borrow_mut() = Some(Store::new().unwrap()));
    }

    #[wasm_bindgen]
    pub fn load_links(n: usize, per_link: bool) -> usize {
        STORE.with(|s| {
            let s = s.borrow();
            let st = s.as_ref().unwrap();
            super::load(st, n, per_link);
            st.len().unwrap()
        })
    }

    #[wasm_bindgen]
    pub fn prepare_queries() -> usize {
        let ids = STORE.with(|s| page_ids(s.borrow().as_ref().unwrap()));
        QS.with(|q| *q.borrow_mut() = queries(&ids));
        QS.with(|q| q.borrow().len())
    }

    #[wasm_bindgen]
    pub fn query_name(i: usize) -> String {
        QS.with(|q| q.borrow()[i].0.to_string())
    }

    #[wasm_bindgen]
    pub fn run_sparql(q: String) -> usize {
        STORE.with(|s| run(s.borrow().as_ref().unwrap(), &q))
    }

    #[wasm_bindgen]
    pub fn query_text(i: usize) -> String {
        QS.with(|q| q.borrow()[i].1.clone())
    }

    #[wasm_bindgen]
    pub fn run_query(i: usize) -> usize {
        let q = QS.with(|q| q.borrow()[i].1.clone());
        STORE.with(|s| run(s.borrow().as_ref().unwrap(), &q))
    }
}
