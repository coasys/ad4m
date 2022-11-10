use crate::perspectives::query_links::QueryLinksPerspectiveQueryLinks;
use crate::perspectives::subscription_link_added::SubscriptionLinkAddedPerspectiveLinkAdded;
pub struct Link {
    pub predicate: Option<String>,
    pub source: String,
    pub target: String,
}

pub struct ExpressionProof {
    pub invalid: Option<bool>,
    pub key: String,
    pub signature: String,
    pub valid: Option<bool>,
}

pub struct LinkExpression {
    pub author: String,
    pub data: Link,
    pub proof: ExpressionProof,
    pub timestamp: String,
}

impl From<QueryLinksPerspectiveQueryLinks> for LinkExpression {
    fn from(link: QueryLinksPerspectiveQueryLinks) -> Self {
        Self {
            author: link.author,
            timestamp: link.timestamp,
            data: Link {
                predicate: link.data.predicate,
                source: link.data.source,
                target: link.data.target,
            },
            proof: ExpressionProof {
                invalid: link.proof.invalid,
                key: link.proof.key,
                signature: link.proof.signature,
                valid: link.proof.valid,
            },
        }
    }
}

impl From<SubscriptionLinkAddedPerspectiveLinkAdded> for LinkExpression {
    fn from(link: SubscriptionLinkAddedPerspectiveLinkAdded) -> Self {
        Self {
            author: link.author,
            timestamp: link.timestamp,
            data: Link {
                predicate: link.data.predicate,
                source: link.data.source,
                target: link.data.target,
            },
            proof: ExpressionProof {
                invalid: link.proof.invalid,
                key: link.proof.key,
                signature: link.proof.signature,
                valid: link.proof.valid,
            },
        }
    }
}