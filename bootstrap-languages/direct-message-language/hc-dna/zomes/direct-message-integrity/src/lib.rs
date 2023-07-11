use hdi::prelude::*;

pub mod ad4m;

use ad4m::*;

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct StatusUpdate(pub PerspectiveExpression);

app_entry!(StatusUpdate);

impl Into<PerspectiveExpression> for StatusUpdate {
    fn into(self) -> PerspectiveExpression {
        self.0
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct StoredMessage(pub PerspectiveExpression);

app_entry!(StoredMessage);

impl Into<PerspectiveExpression> for StoredMessage {
    fn into(self) -> PerspectiveExpression {
        self.0
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, SerializedBytes)]
pub struct PublicMessage(pub PerspectiveExpression);

app_entry!(PublicMessage);

impl Into<PerspectiveExpression> for PublicMessage {
    fn into(self) -> PerspectiveExpression {
        self.0
    }
}

#[derive(Serialize, Deserialize, Debug, SerializedBytes)]
pub struct Properties {
    pub recipient_hc_agent_pubkey: String,
}

#[derive(Serialize, Deserialize, Debug, SerializedBytes)]
pub struct Signal {
    pub json: String,
}

#[derive(Serialize, Deserialize, Debug, SerializedBytes, PartialEq, Eq)]
pub struct Recipient(pub AgentPubKey);

app_entry!(Recipient);

impl Recipient {
    pub fn get(&self) -> AgentPubKey {
        return self.0.clone();
    }
}

#[hdk_entry_defs]
#[unit_enum(UnitEntryTypes)]
pub enum EntryTypes {
    #[entry_def(visibility = "private")]
    StatusUpdate(StatusUpdate),
    #[entry_def(visibility = "private")]
    StoredMessage(StoredMessage),
    #[entry_def(visibility = "public")]
    PublicMessage(PublicMessage),
    #[entry_def(visibility = "private")]
    Recipient(Recipient),
}

#[hdk_link_types]
pub enum LinkTypes {
    Message,
}
