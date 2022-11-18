use crate::agent::by_did::{ByDidAgentByDid, ByDidAgentByDidPerspectiveLinks};
use crate::agent::me::{MeAgent, MeAgentPerspectiveLinks};
use crate::perspectives::query_links::QueryLinksPerspectiveQueryLinks;
use crate::perspectives::subscription_link_added::SubscriptionLinkAddedPerspectiveLinkAdded;

#[derive(Debug)]
pub struct Link {
    pub predicate: Option<String>,
    pub source: String,
    pub target: String,
}

#[derive(Debug)]
pub struct ExpressionProof {
    pub invalid: Option<bool>,
    pub key: String,
    pub signature: String,
    pub valid: Option<bool>,
}

#[derive(Debug)]
pub struct LinkExpression {
    pub author: String,
    pub data: Link,
    pub proof: ExpressionProof,
    pub timestamp: String,
}

#[derive(Debug)]
pub struct PerspectiveExpression {
    pub author: String,
    pub data: Perspective,
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

impl From<MeAgentPerspectiveLinks> for LinkExpression {
    fn from(link: MeAgentPerspectiveLinks) -> Self {
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

impl From<ByDidAgentByDidPerspectiveLinks> for LinkExpression {
    fn from(link: ByDidAgentByDidPerspectiveLinks) -> Self {
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

use crate::perspectives::snapshot::SnapshotPerspectiveSnapshotLinks;
use crate::perspectives::snapshot::SnapshotPerspectiveSnapshotLinksData;
use crate::perspectives::snapshot::SnapshotPerspectiveSnapshotLinksProof;

impl From<SnapshotPerspectiveSnapshotLinks> for LinkExpression {
    fn from(link: SnapshotPerspectiveSnapshotLinks) -> Self {
        Self {
            author: link.author,
            timestamp: link.timestamp,
            data: Link {
                predicate: link.data.predicate,
                source: link.data.source,
                target: link.data.target,
            },
            proof: ExpressionProof {
                invalid: None,
                key: link.proof.key,
                signature: link.proof.signature,
                valid: None,
            },
        }
    }
}

impl From<LinkExpression> for SnapshotPerspectiveSnapshotLinks {
    fn from(link: LinkExpression) -> Self {
        Self {
            author: link.author,
            timestamp: link.timestamp,
            data: SnapshotPerspectiveSnapshotLinksData {
                predicate: link.data.predicate,
                source: link.data.source,
                target: link.data.target,
            },
            proof: SnapshotPerspectiveSnapshotLinksProof {
                key: link.proof.key,
                signature: link.proof.signature,
            },
        }
    }
}

impl From<LinkExpression> for LinkExpressionInput {
    fn from(link: LinkExpression) -> Self {
        Self {
            author: link.author,
            timestamp: link.timestamp,
            data: LinkInput {
                predicate: link.data.predicate,
                source: link.data.source,
                target: link.data.target,
            },
            proof: ExpressionProofInput {
                key: link.proof.key,
                signature: link.proof.signature,
                invalid: link.proof.invalid,
                valid: link.proof.valid,
            },
        }
    }
}

impl From<LinkExpression> for friend_send_message::LinkExpressionInput {
    fn from(link: LinkExpression) -> Self {
        Self {
            author: link.author,
            timestamp: link.timestamp,
            data: friend_send_message::LinkInput {
                predicate: link.data.predicate,
                source: link.data.source,
                target: link.data.target,
            },
            proof: friend_send_message::ExpressionProofInput {
                key: link.proof.key,
                signature: link.proof.signature,
                invalid: link.proof.invalid,
                valid: link.proof.valid,
            },
        }
    }
}

use crate::perspectives::all::AllPerspectivesNeighbourhoodMetaLinks;

impl From<AllPerspectivesNeighbourhoodMetaLinks> for LinkExpression {
    fn from(link: AllPerspectivesNeighbourhoodMetaLinks) -> Self {
        Self {
            author: link.author,
            timestamp: link.timestamp,
            data: Link {
                predicate: link.data.predicate,
                source: link.data.source,
                target: link.data.target,
            },
            proof: ExpressionProof {
                invalid: None,
                key: link.proof.key,
                signature: link.proof.signature,
                valid: None,
            },
        }
    }
}

#[derive(Debug)]
pub struct Perspective {
    pub links: Vec<LinkExpression>,
}

use crate::perspectives::snapshot::SnapshotPerspectiveSnapshot;
use crate::runtime::set_status::ExpressionProofInput;
use crate::runtime::set_status::LinkExpressionInput;
use crate::runtime::set_status::LinkInput;
use crate::runtime::set_status::PerspectiveInput;

impl From<SnapshotPerspectiveSnapshot> for Perspective {
    fn from(perspective: SnapshotPerspectiveSnapshot) -> Self {
        Self {
            links: perspective
                .links
                .into_iter()
                .map(LinkExpression::from)
                .collect(),
        }
    }
}

impl From<Perspective> for PerspectiveInput {
    fn from(perspective: Perspective) -> Self {
        Self {
            links: perspective
                .links
                .into_iter()
                .map(LinkExpressionInput::from)
                .collect(),
        }
    }
}

use crate::runtime::friend_send_message;

impl From<Perspective> for friend_send_message::PerspectiveInput {
    fn from(perspective: Perspective) -> Self {
        Self {
            links: perspective
                .links
                .into_iter()
                .map(friend_send_message::LinkExpressionInput::from)
                .collect(),
        }
    }
}

use crate::runtime::message_inbox;

impl From<message_inbox::MessageInboxRuntimeMessageInbox> for PerspectiveExpression {
    fn from(perspective_expression: message_inbox::MessageInboxRuntimeMessageInbox) -> Self {
        Self {
            author: perspective_expression.author,
            timestamp: perspective_expression.timestamp,
            data: Perspective {
                links: perspective_expression
                    .data
                    .links
                    .into_iter()
                    .map(LinkExpression::from)
                    .collect(),
            },
            proof: ExpressionProof {
                invalid: perspective_expression.proof.invalid,
                key: perspective_expression.proof.key,
                signature: perspective_expression.proof.signature,
                valid: perspective_expression.proof.valid,
            },
        }
    }
}

use crate::runtime::message_inbox::MessageInboxRuntimeMessageInboxDataLinks;

impl From<MessageInboxRuntimeMessageInboxDataLinks> for LinkExpression {
    fn from(link: MessageInboxRuntimeMessageInboxDataLinks) -> Self {
        Self {
            author: link.author,
            timestamp: link.timestamp,
            data: Link {
                predicate: link.data.predicate,
                source: link.data.source,
                target: link.data.target,
            },
            proof: ExpressionProof {
                invalid: None,
                key: link.proof.key,
                signature: link.proof.signature,
                valid: None,
            },
        }
    }
}

use crate::runtime::message_outbox::MessageOutboxRuntimeMessageOutbox;
use crate::runtime::message_outbox::MessageOutboxRuntimeMessageOutboxMessageDataLinks;

impl From<MessageOutboxRuntimeMessageOutboxMessageDataLinks> for LinkExpression {
    fn from(link: MessageOutboxRuntimeMessageOutboxMessageDataLinks) -> Self {
        Self {
            author: link.author,
            timestamp: link.timestamp,
            data: Link {
                predicate: link.data.predicate,
                source: link.data.source,
                target: link.data.target,
            },
            proof: ExpressionProof {
                invalid: None,
                key: link.proof.key,
                signature: link.proof.signature,
                valid: None,
            },
        }
    }
}
pub struct SentPerspectiveMessage {
    pub recipient: String,
    pub message: PerspectiveExpression,
}

impl From<MessageOutboxRuntimeMessageOutbox> for SentPerspectiveMessage {
    fn from(message: MessageOutboxRuntimeMessageOutbox) -> Self {
        Self {
            recipient: message.recipient,
            message: PerspectiveExpression {
                author: message.message.author,
                timestamp: message.message.timestamp,
                data: Perspective {
                    links: message
                        .message
                        .data
                        .links
                        .into_iter()
                        .map(LinkExpression::from)
                        .collect(),
                },
                proof: ExpressionProof {
                    invalid: message.message.proof.invalid,
                    key: message.message.proof.key,
                    signature: message.message.proof.signature,
                    valid: message.message.proof.valid,
                },
            },
        }
    }
}

pub struct Agent {
    pub did: String,
    pub direct_message_language: Option<String>,
    pub perspective: Option<Perspective>,
}

impl From<MeAgent> for Agent {
    fn from(me: MeAgent) -> Self {
        Self {
            did: me.did,
            direct_message_language: me.direct_message_language,
            perspective: me.perspective.map(|perspective| Perspective {
                links: perspective
                    .links
                    .into_iter()
                    .map(|link| link.into())
                    .collect(),
            }),
        }
    }
}

impl From<ByDidAgentByDid> for Agent {
    fn from(agent: ByDidAgentByDid) -> Self {
        Self {
            did: agent.did,
            direct_message_language: agent.direct_message_language,
            perspective: agent.perspective.map(|perspective| Perspective {
                links: perspective
                    .links
                    .into_iter()
                    .map(|link| link.into())
                    .collect(),
            }),
        }
    }
}
