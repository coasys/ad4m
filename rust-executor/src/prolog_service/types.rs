use scryer_prolog::{LeafAnswer, Term};

#[derive(Clone, Debug, PartialEq)]
pub struct QueryMatch {
    pub bindings: std::collections::BTreeMap<String, Term>,
}

impl From<std::collections::BTreeMap<String, Term>> for QueryMatch {
    fn from(bindings: std::collections::BTreeMap<String, Term>) -> Self {
        QueryMatch { bindings }
    }
}

impl From<std::collections::BTreeMap<&str, Term>> for QueryMatch {
    fn from(bindings: std::collections::BTreeMap<&str, Term>) -> Self {
        QueryMatch {
            bindings: bindings
                .into_iter()
                .map(|(k, v)| (k.to_string(), v))
                .collect(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum QueryResolution {
    True,
    False,
    Matches(Vec<QueryMatch>),
}

impl From<Vec<LeafAnswer>> for QueryResolution {
    fn from(answers: Vec<LeafAnswer>) -> Self {
        if answers.is_empty() {
            return QueryResolution::False;
        }

        let mut found_true = false;

        let mut matches = Vec::new();
        for answer in answers {
            match answer {
                LeafAnswer::True => {
                    found_true = true;
                }
                LeafAnswer::LeafAnswer { bindings, .. } => {
                    matches.push(QueryMatch { bindings });
                }
                _ => {
                    continue;
                }
            }
        }

        if found_true {
            QueryResolution::True
        } else if matches.is_empty() {
            QueryResolution::False
        } else {
            QueryResolution::Matches(matches)
        }
    }
}

pub type QueryResult = Result<QueryResolution, String>;

pub fn query_result_from_leaf_answer(result: Result<Vec<LeafAnswer>, Term>) -> QueryResult {
    result
        .map(QueryResolution::from)
        .map_err(term_to_string)
}

pub fn term_to_string(value: Term) -> String {
    match value {
        Term::Integer(i) => format!("{}", i),
        Term::Float(f) => format!("{}", f),
        Term::Rational(r) => format!("{}", r),
        Term::Atom(a) => format!("'{}'", a),
        Term::String(s) => s.to_string(),
        Term::List(l) => {
            let mut string_result = "[".to_string();
            for (i, v) in l.iter().enumerate() {
                if i > 0 {
                    string_result.push_str(", ");
                }
                string_result.push_str(&term_to_string(v.clone()));
            }
            string_result.push(']');
            string_result
        }
        Term::Compound(s, l) => {
            let mut string_inner = String::new();
            for (i, v) in l.iter().enumerate() {
                if i > 0 {
                    string_inner.push_str(", ");
                }
                string_inner.push_str(&term_to_string(v.clone()));
            }

            format!("{{ '{}': [{}] }}", s.as_str(), string_inner)
        }
        Term::Var(v) => format!("Var: {}", v),
        _ => "null".to_string(),
    }
}
