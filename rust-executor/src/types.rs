use std::default;

use serde::{Deserialize, Serialize};
use juniper::{
    GraphQLEnum, GraphQLObject, GraphQLValue,
};

use crate::agent::signatures::verify;

#[derive(Default, Debug, Deserialize, Serialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Expression<T: Serialize> {
    pub author: String,
    pub timestamp: String,
    pub data: T,
    pub proof: ExpressionProof,
}

#[derive(Default, Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct ExpressionProof {
    pub key: String,
    pub signature: String,
}

#[derive(Debug, Deserialize, Serialize, Clone, PartialEq)]
pub struct VerifiedExpression<T: GraphQLValue + Serialize> {
    pub author: String,
    pub timestamp: String,
    pub data: T,
    pub proof: DecoratedExpressionProof,
}

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone, PartialEq)]

pub struct DecoratedExpressionProof {
    pub key: String,
    pub signature: String,
    pub valid: bool,
    pub invalid: bool,
}

impl<T: GraphQLValue + Serialize> From<Expression<T>> for VerifiedExpression<T> {
    fn from(expr: Expression<T>) -> Self {
        let valid = match verify(&expr) {
            Ok(valid) => valid,
            Err(_) => false,
        };
        let invalid = !valid;
        VerifiedExpression {
            author: expr.author,
            timestamp: expr.timestamp,
            data: expr.data,
            proof: DecoratedExpressionProof {
                key: expr.proof.key,
                signature: expr.proof.signature,
                valid,
                invalid,
            },
        }
    }
}



#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Link {
    pub predicate: Option<String>,
    pub source: String,
    pub target: String,
}


pub type LinkExpression = Expression<Link>;

#[derive(GraphQLObject, Default, Debug, Deserialize, Serialize, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct DecoratedLinkExpression {
    pub author: String,
    pub timestamp: String,
    pub data: Link,
    pub proof: DecoratedExpressionProof,
    pub status: LinkStatus,
}

impl From<(LinkExpression, LinkStatus)> for DecoratedLinkExpression {
    fn from((expr, status): (LinkExpression, LinkStatus)) -> Self {
        let verified_expr: VerifiedExpression<Link> = expr.into();
        DecoratedLinkExpression {
            author: verified_expr.author,
            timestamp: verified_expr.timestamp,
            data: verified_expr.data,
            proof: verified_expr.proof,
            status,
        }
    }
}

impl From<DecoratedLinkExpression> for LinkExpression {
    fn from(decorated: DecoratedLinkExpression) -> Self {
        LinkExpression {
            author: decorated.author,
            timestamp: decorated.timestamp,
            data: decorated.data,
            proof: ExpressionProof {
                key: decorated.proof.key,
                signature: decorated.proof.signature,
            }
        }
    }
}

            



#[derive(GraphQLEnum, Debug, Default, Deserialize, Serialize, Clone, PartialEq)]
pub enum LinkStatus {
    #[default]
    #[serde(rename = "shared")]
    Shared,
    #[serde(rename = "local")]
    Local,
}


pub struct LinkMutations {
    pub additions: Vec<Link>,
    pub removals: Vec<Link>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct PerspectiveDiff {
    pub additions: Vec<LinkExpression>,
    pub removals: Vec<LinkExpression>,
}


#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]

pub struct Perspective {
    pub links: Vec<LinkExpression>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Neighbourhood {
    pub link_language: String,
    pub meta: Perspective,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct NeighbourhoodExpression {
    pub author: String,
    pub data: Neighbourhood,
    pub proof: ExpressionProof,
    pub timestamp: String,
}
