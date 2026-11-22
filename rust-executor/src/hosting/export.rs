//! A person's export and import that keep signatures (AR-06 4.10, AR-05 4.9.1).
//!
//! The export holds every perspective the person owns, each link exactly as stored: author,
//! timestamp, data, and the proof (key, signature and anchor). Nothing here signs. The import
//! writes a private perspective's links as they are through [`LinkSink`]; the store checks
//! each signature as it checks a link that arrives by sync, and a link that fails is kept and
//! counted, never dropped and never re-signed. A space's links are not written: its
//! `neighbourhoodUrl` is returned, and the app joins it.

use async_trait::async_trait;
use serde::{Deserialize, Serialize};

use crate::types::{DecoratedLinkExpression, ExpressionProof, LinkExpression};

/// The only format name.
pub const FORMAT: &str = "coasys-export";
/// The only version.
pub const VERSION: u32 = 1;

/// The file a person saves (gzip JSON; plain, with the warning of AR-06 4.14).
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ExportFile {
    pub format: String,
    pub version: u32,
    pub created_at: String,
    pub executor_did: String,
    pub person_did_scid: Option<String>,
    pub device_did: String,
    pub perspectives: Vec<ExportedPerspective>,
}

/// One perspective of the file.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ExportedPerspective {
    pub uuid: String,
    pub name: Option<String>,
    /// `private` or `space`.
    pub kind: String,
    pub neighbourhood_url: Option<String>,
    pub links: Vec<LinkExpression>,
}

/// The link as stored, without this executor's verdict: the proof is copied, never made.
pub fn signed_link(link: &DecoratedLinkExpression) -> LinkExpression {
    LinkExpression {
        author: link.author.clone(),
        timestamp: link.timestamp.clone(),
        data: link.data.clone(),
        proof: ExpressionProof {
            key: link.proof.key.clone(),
            signature: link.proof.signature.clone(),
            anchor: link.proof.anchor,
        },
        status: link.status.clone(),
    }
}

/// Builds one entry from a perspective's links. A space carries its URL and no links.
pub fn exported_perspective(
    uuid: &str,
    name: Option<String>,
    neighbourhood_url: Option<String>,
    links: &[DecoratedLinkExpression],
) -> ExportedPerspective {
    let space = neighbourhood_url.is_some();
    ExportedPerspective {
        uuid: uuid.to_string(),
        name,
        kind: if space { "space" } else { "private" }.into(),
        neighbourhood_url,
        links: if space {
            Vec::new()
        } else {
            links.iter().map(signed_link).collect()
        },
    }
}

/// Why a file is refused.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ImportRefusal {
    WrongFormat,
    UnsupportedVersion(u32),
    TooLarge { bytes: u64, max: u64 },
}

impl std::fmt::Display for ImportRefusal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImportRefusal::WrongFormat => write!(f, "invalid: not a {FORMAT} file"),
            ImportRefusal::UnsupportedVersion(v) => {
                write!(f, "invalid: export version {v} is not {VERSION}")
            }
            ImportRefusal::TooLarge { bytes, max } => {
                write!(f, "too-large: {bytes} bytes is over the limit of {max}")
            }
        }
    }
}

/// Step 1 of the import: the format, the version and the executor's own size limit (no
/// per-tenant limit applies).
pub fn check_file(file: &ExportFile, bytes: u64, max_bytes: u64) -> Result<(), ImportRefusal> {
    if file.format != FORMAT {
        return Err(ImportRefusal::WrongFormat);
    }
    if file.version != VERSION {
        return Err(ImportRefusal::UnsupportedVersion(file.version));
    }
    if bytes > max_bytes {
        return Err(ImportRefusal::TooLarge {
            bytes,
            max: max_bytes,
        });
    }
    Ok(())
}

/// Where an import writes: a new private perspective of the importing person, and its links
/// as they are.
#[async_trait]
pub trait LinkSink: Send {
    /// Makes a perspective named `name`, owned by the importing person; answers its uuid.
    async fn new_perspective(&mut self, name: Option<String>) -> Result<String, String>;
    /// Writes `link` unchanged; answers whether the store judged its signature valid.
    async fn write(&mut self, perspective: &str, link: LinkExpression) -> Result<bool, String>;
}

/// What an import did.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ImportReport {
    /// `(file uuid, new uuid)` per private perspective.
    pub perspectives: Vec<(String, String)>,
    pub links_written: u64,
    /// Written with a failed signature check, and counted.
    pub links_invalid: u64,
    /// The spaces of the file: the app joins each.
    pub neighbourhood_urls: Vec<String>,
}

/// Steps 2 and 3 of the import.
pub async fn import_into(
    sink: &mut dyn LinkSink,
    file: &ExportFile,
) -> Result<ImportReport, String> {
    let mut report = ImportReport::default();
    for p in &file.perspectives {
        if p.kind == "space" {
            if let Some(url) = &p.neighbourhood_url {
                report.neighbourhood_urls.push(url.clone());
            }
            continue;
        }
        let uuid = sink.new_perspective(p.name.clone()).await?;
        for link in &p.links {
            report.links_written += 1;
            if !sink.write(&uuid, link.clone()).await? {
                report.links_invalid += 1;
            }
        }
        report.perspectives.push((p.uuid.clone(), uuid));
    }
    Ok(report)
}

/// The executor's real sink: perspectives owned by `owner` (`None`: the main agent, whose
/// perspectives have no owner), links through `PerspectiveInstance::add_link_expression` (a
/// proof that arrived with the link is judged as a peer's is).
pub struct LivePerspectives {
    pub owner: Option<String>,
}

#[async_trait]
impl LinkSink for LivePerspectives {
    async fn new_perspective(&mut self, name: Option<String>) -> Result<String, String> {
        let name = name.unwrap_or_default();
        let handle = match &self.owner {
            Some(owner) => crate::types::PerspectiveHandle::new_with_owner(name, owner.clone()),
            None => crate::types::PerspectiveHandle::new(
                uuid::Uuid::new_v4().to_string(),
                Some(name),
                None,
                None,
                crate::types::PerspectiveState::Private,
            ),
        };
        let uuid = handle.uuid.clone();
        crate::perspectives::add_perspective(handle, None).await?;
        Ok(uuid)
    }

    async fn write(&mut self, perspective: &str, link: LinkExpression) -> Result<bool, String> {
        let mut instance = crate::perspectives::get_perspective(perspective)
            .ok_or_else(|| format!("no perspective {perspective}"))?;
        let status = link
            .status
            .clone()
            .unwrap_or(crate::types::LinkStatus::Local);
        let written = instance
            .add_link_expression(link, status, None)
            .await
            .map_err(|e| e.to_string())?;
        Ok(!matches!(
            written.verdict,
            Some(crate::identity::verdict::LinkVerdict::Invalid(_))
        ))
    }
}

/// The export of every perspective `owner` owns (`None`: the main agent's, which have no
/// owner), as the file holds it. `only` narrows it to those uuids; naming one `owner` does not
/// own is `Err(uuid)`, so a person's export never holds another's perspective.
pub async fn export_owned(
    owner: Option<&str>,
    only: Option<&[String]>,
    executor_did: &str,
    person_did_scid: Option<String>,
    device_did: &str,
    now_iso: String,
) -> Result<ExportFile, ExportError> {
    let mut mine = Vec::new();
    for instance in crate::perspectives::all_perspectives() {
        let handle = instance.persisted.lock().await.clone();
        let owned = match owner {
            Some(owner) => handle.is_owned_by(owner),
            None => handle.is_unowned(),
        };
        if owned {
            mine.push((instance, handle));
        }
    }
    if let Some(only) = only {
        if let Some(foreign) = only
            .iter()
            .find(|uuid| !mine.iter().any(|(_, h)| &h.uuid == *uuid))
        {
            return Err(ExportError::NotYours(foreign.clone()));
        }
        mine.retain(|(_, h)| only.contains(&h.uuid));
    }
    let mut perspectives = Vec::new();
    for (instance, handle) in mine {
        let links = instance
            .get_links(&crate::types::LinkQuery {
                source: None,
                target: None,
                predicate: None,
                from_date: None,
                until_date: None,
                limit: None,
                include_unverified: Some(true),
            })
            .await
            .map_err(|e| ExportError::Failed(e.to_string()))?;
        perspectives.push(exported_perspective(
            &handle.uuid,
            handle.name.clone(),
            handle.shared_url.clone(),
            &links,
        ));
    }
    Ok(ExportFile {
        format: FORMAT.into(),
        version: VERSION,
        created_at: now_iso,
        executor_did: executor_did.to_string(),
        person_did_scid,
        device_did: device_did.to_string(),
        perspectives,
    })
}

/// Why an export is refused.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExportError {
    /// The caller named a perspective it does not own (or that does not exist).
    NotYours(String),
    Failed(String),
}

#[cfg(test)]
mod tests;
