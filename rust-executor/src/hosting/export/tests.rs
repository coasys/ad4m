use super::*;
use crate::types::{Link, LinkStatus};

fn link(author: &str, target: &str, sig: &str, anchor: Option<u64>) -> LinkExpression {
    LinkExpression {
        author: author.into(),
        timestamp: "2026-01-02T03:04:05.000Z".into(),
        data: Link {
            source: "ad4m://self".into(),
            predicate: Some("sioc://likes".into()),
            target: target.into(),
        },
        proof: ExpressionProof {
            key: format!("{author}#key"),
            signature: sig.into(),
            anchor,
        },
        status: Some(LinkStatus::Shared),
    }
}

fn file_of(links: Vec<LinkExpression>) -> ExportFile {
    let decorated: Vec<DecoratedLinkExpression> = links
        .iter()
        .map(|l| DecoratedLinkExpression::from((l.clone(), LinkStatus::Shared)))
        .collect();
    ExportFile {
        format: FORMAT.into(),
        version: VERSION,
        created_at: "2026-01-02T00:00:00Z".into(),
        executor_did: "did:key:zExecutorA".into(),
        person_did_scid: Some("did:scid:coasys:1:Qm".into()),
        device_did: "did:key:zTenant".into(),
        perspectives: vec![
            exported_perspective("p1", Some("notes".into()), None, &decorated),
            exported_perspective(
                "p2",
                Some("club".into()),
                Some("neighbourhood://QmClub".into()),
                &decorated,
            ),
        ],
    }
}

/// Executor B: records what it is asked to write, and judges the signature `bad` invalid.
struct SinkB {
    written: Vec<(String, LinkExpression)>,
    made: u32,
}

#[async_trait]
impl LinkSink for SinkB {
    async fn new_perspective(&mut self, _name: Option<String>) -> Result<String, String> {
        self.made += 1;
        Ok(format!("b-{}", self.made))
    }
    async fn write(&mut self, perspective: &str, link: LinkExpression) -> Result<bool, String> {
        let valid = link.proof.signature != "bad";
        self.written.push((perspective.to_string(), link));
        Ok(valid)
    }
}

#[tokio::test]
async fn an_export_imported_elsewhere_keeps_every_signature_and_author() {
    let a = vec![
        link("did:key:zAlice", "a", "sigA", Some(3)),
        link("did:key:zBob", "b", "sigB", None),
    ];
    // Through the file's JSON, as a person saves and carries it.
    let json = serde_json::to_vec(&file_of(a.clone())).unwrap();
    let file: ExportFile = serde_json::from_slice(&json).unwrap();
    check_file(&file, json.len() as u64, 1 << 30).unwrap();

    let mut b = SinkB {
        written: vec![],
        made: 0,
    };
    let report = import_into(&mut b, &file).await.unwrap();

    assert_eq!(b.written.len(), 2);
    for ((uuid, got), sent) in b.written.iter().zip(&a) {
        assert_eq!(uuid, "b-1", "a new perspective, never the file's uuid");
        assert_eq!(got.author, sent.author);
        assert_eq!(got.timestamp, sent.timestamp);
        assert_eq!(got.data, sent.data);
        assert_eq!(
            got.proof, sent.proof,
            "the proof is as signed, anchor included"
        );
    }
    assert_eq!(
        report.perspectives,
        vec![("p1".to_string(), "b-1".to_string())]
    );
    assert_eq!(report.links_written, 2);
    assert_eq!(report.links_invalid, 0);
}

#[tokio::test]
async fn a_space_writes_no_links_and_comes_back_as_its_url() {
    let file = file_of(vec![link("did:key:zAlice", "a", "sigA", None)]);
    assert!(
        file.perspectives[1].links.is_empty(),
        "the export holds none of a space's links"
    );
    let mut b = SinkB {
        written: vec![],
        made: 0,
    };
    let report = import_into(&mut b, &file).await.unwrap();
    assert_eq!(b.made, 1, "only the private perspective is made");
    assert_eq!(
        report.neighbourhood_urls,
        vec!["neighbourhood://QmClub".to_string()]
    );
}

#[tokio::test]
async fn a_link_that_fails_its_signature_is_kept_and_counted() {
    let file = file_of(vec![
        link("did:key:zAlice", "a", "sigA", None),
        link("did:key:zAlice", "b", "bad", None),
    ]);
    let mut b = SinkB {
        written: vec![],
        made: 0,
    };
    let report = import_into(&mut b, &file).await.unwrap();
    assert_eq!(report.links_written, 2, "the failing link is still written");
    assert_eq!(report.links_invalid, 1);
    assert_eq!(b.written[1].1.proof.signature, "bad", "and never re-signed");
}

#[test]
fn a_file_of_another_format_or_version_or_size_is_refused() {
    let mut file = file_of(vec![]);
    assert_eq!(check_file(&file, 10, 10), Ok(()));
    assert_eq!(
        check_file(&file, 11, 10),
        Err(ImportRefusal::TooLarge { bytes: 11, max: 10 })
    );
    file.version = 2;
    assert_eq!(
        check_file(&file, 1, 10),
        Err(ImportRefusal::UnsupportedVersion(2))
    );
    file.version = VERSION;
    file.format = "other".into();
    assert_eq!(check_file(&file, 1, 10), Err(ImportRefusal::WrongFormat));
}
