//! `create`: make an identity on this surface for its executor.
//!
//! 1. New words; the surface shows them ([`CreateScreen::ShowWords`]).
//! 2. The person types two of them back ([`CreateScreen::ConfirmWords`]); the words are wiped,
//!    the seed stays for the flow.
//! 3. Master #0 and the commitment to master #1 from the seed; an `incept` listing the
//!    executor's device key as `d1` with its `enc` key (`identity.status`); the executor adds
//!    its proof (`identity.cosignIncept`); the client folds the event; `identity.publish`.
//! 4. The vault keeps master #0 and the co-signed incept *before* the publish: a vault that
//!    refuses (a cancelled prompt, no screen lock) leaves nothing in the store, and a
//!    published identity always has its master key on this surface.
//! 5. `identity.publish`; once it answers, the vault forgets the incept. When the publish
//!    fails or its answer is lost, [`resume`] (or `finish` with the same words) publishes the
//!    kept incept again: the executor answers a repeat of the events its store holds with the
//!    store's head, so the identity is made once, with the master key the vault holds.

use ad4m_identity_core::build::{incept_human, Signer};
use ad4m_identity_core::words::{self, Seed, Words};
use ad4m_identity_core::{DeviceKey, Event, KeyLife, Op};
use ad4m_ldk::identity::SignedHead;

use crate::executor::{ExecutorRpc, IdentityStatus, ViaExecutor};
use crate::secret::Secret;
use crate::vault::Vault;
use crate::{fold, ClientError, Random};

/// What the surface shows at each step.
#[derive(Debug, PartialEq, Eq)]
pub enum CreateScreen {
    /// The 12 words, to write down (screen capture off).
    ShowWords(Vec<String>),
    /// Ask for the words at these positions (counted from 1).
    ConfirmWords { positions: [usize; 2] },
}

/// The identity made.
#[derive(Clone, Debug, PartialEq)]
pub struct Created {
    pub did: String,
    pub scid: String,
    /// The executor's device key's id in the log (`d1`).
    pub device_key_id: String,
    pub head: SignedHead,
}

enum Step {
    Shown(Words),
    Asked(Words, [usize; 2]),
    Confirmed(Seed),
}

/// The `create` flow, one state per step. Dropping it wipes the words and the seed.
pub struct CreateFlow {
    /// `None` only while a step moves the state on.
    step: Option<Step>,
}

fn random_index(random: &dyn Random, below: usize) -> usize {
    let mut bytes = [0u8; 8];
    random.fill(&mut bytes);
    (u64::from_le_bytes(bytes) % below as u64) as usize
}

impl CreateFlow {
    /// New words from `random`; the first screen shows them.
    pub fn start(random: &dyn Random) -> (CreateFlow, CreateScreen) {
        let mut entropy = zeroize::Zeroizing::new([0u8; 16]);
        random.fill(&mut entropy[..]);
        Self::from_words(words::from_entropy(&entropy))
    }

    /// The flow over given words (a surface that made them itself, and tests).
    pub fn from_words(words: Words) -> (CreateFlow, CreateScreen) {
        let shown = words.words().iter().map(|w| w.to_string()).collect();
        (
            CreateFlow {
                step: Some(Step::Shown(words)),
            },
            CreateScreen::ShowWords(shown),
        )
    }

    /// The person wrote the words down: ask for two of them, at random distinct positions.
    pub fn words_written(&mut self, random: &dyn Random) -> Result<CreateScreen, ClientError> {
        let Some(Step::Shown(words)) = self.step.take_if(|s| matches!(s, Step::Shown(_))) else {
            return Err(ClientError::Step("words_written follows start"));
        };
        let first = random_index(random, words::WORD_COUNT);
        let mut second = random_index(random, words::WORD_COUNT - 1);
        if second >= first {
            second += 1;
        }
        let positions = [first.min(second) + 1, first.max(second) + 1];
        self.step = Some(Step::Asked(words, positions));
        Ok(CreateScreen::ConfirmWords { positions })
    }

    /// The two words typed back, in the order of the positions asked. On a match the words
    /// are wiped and the seed kept for [`CreateFlow::finish`]; on a mismatch the flow asks
    /// again for the same positions.
    pub fn confirm(&mut self, answers: [&str; 2]) -> Result<(), ClientError> {
        let Some(Step::Asked(words, positions)) =
            self.step.take_if(|s| matches!(s, Step::Asked(..)))
        else {
            return Err(ClientError::Step("confirm follows words_written"));
        };
        let list = words.words();
        let matches = positions
            .iter()
            .zip(answers)
            .all(|(p, a)| list[p - 1] == a.trim().to_lowercase());
        if !matches {
            self.step = Some(Step::Asked(words, positions));
            return Err(ClientError::WrongWords);
        }
        let seed = words::seed(&words);
        drop(words);
        self.step = Some(Step::Confirmed(seed));
        Ok(())
    }

    /// Makes the identity on `executor` and keeps master #0 in `vault`.
    pub async fn finish<R: ExecutorRpc>(
        self,
        executor: &ViaExecutor<R>,
        vault: &dyn Vault,
    ) -> Result<Created, ClientError> {
        let Some(Step::Confirmed(seed)) = self.step else {
            return Err(ClientError::Step("finish follows confirm"));
        };
        create_from_seed(&seed, executor, vault).await
    }
}

/// Steps 3 to 5 of `create` from the seed of confirmed words. When the vault keeps an incept of
/// these words for this executor (an earlier `create` that did not see its publish answered),
/// publishes that one again instead of making another.
pub async fn create_from_seed<R: ExecutorRpc>(
    seed: &Seed,
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
) -> Result<Created, ClientError> {
    let status = executor.status().await?;
    let master = words::master_key(seed, 0)?;
    let (next, _) = words::next_commitment(seed, 1)?;
    let device = DeviceKey {
        id: "d1".into(),
        sign: status.device_key.clone(),
        enc: status.device_enc.clone(),
        life: KeyLife::Standing,
    };
    let incept = incept_human(&master, next, vec![device])?;
    if let Some(pending) = pending_for(vault, &status.device_key)? {
        if pending.scid != incept.scid {
            return Err(ClientError::CreatePending(ad4m_identity_core::did_of(
                &pending.scid,
            )));
        }
        return publish_kept(executor, vault, &status, pending).await;
    }
    if let Some(did) = status.did {
        return Err(ClientError::HasIdentity(did));
    }
    let cosigned: Event = executor.cosign_incept(&incept).await?;
    if cosigned.op != incept.op || cosigned.scid != incept.scid {
        return Err(ClientError::Protocol(
            "the executor changed the incept it co-signed".into(),
        ));
    }
    let chain = fold(std::slice::from_ref(&cosigned))?;
    if chain.state().masters.first().map(|m| m.sign.clone()) != Some(Signer::key(&master)) {
        return Err(ClientError::Protocol(
            "the incept names another master".into(),
        ));
    }
    // Custody first: nothing is public until the vault holds the master key and the incept.
    vault.store_master(&cosigned.scid, 0, Secret::new(master.to_bytes()))?;
    vault.store_pending_incept(&cosigned)?;
    publish_kept(executor, vault, &status, cosigned).await
}

/// Finishes a `create` whose publish failed or went unanswered: publishes the incept the vault
/// keeps for this executor again (the executor answers a repeat of a held log with its head).
/// `None` when the vault keeps no such incept.
pub async fn resume<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
) -> Result<Option<Created>, ClientError> {
    let status = executor.status().await?;
    let Some(pending) = pending_for(vault, &status.device_key)? else {
        return Ok(None);
    };
    publish_kept(executor, vault, &status, pending)
        .await
        .map(Some)
}

/// The kept incept that lists `device_key`.
fn pending_for(vault: &dyn Vault, device_key: &str) -> Result<Option<Event>, ClientError> {
    Ok(vault
        .pending_incepts()?
        .into_iter()
        .find(|e| listed_id(e, device_key).is_some()))
}

fn listed_id(incept: &Event, device_key: &str) -> Option<String> {
    let Op::Incept { add, .. } = &incept.op else {
        return None;
    };
    add.iter()
        .find(|d| d.sign == device_key)
        .map(|d| d.id.clone())
}

/// Publishes `incept`, which the vault keeps with its master #0, and forgets it once the
/// executor answers. An executor that speaks for another identity is refused before the publish.
async fn publish_kept<R: ExecutorRpc>(
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
    status: &IdentityStatus,
    incept: Event,
) -> Result<Created, ClientError> {
    let did = ad4m_identity_core::did_of(&incept.scid);
    if let Some(held) = &status.did {
        if *held != did {
            return Err(ClientError::HasIdentity(held.clone()));
        }
    }
    fold(std::slice::from_ref(&incept))?;
    let device_key_id = listed_id(&incept, &status.device_key).ok_or_else(|| {
        ClientError::Protocol("the kept incept does not list this executor's key".into())
    })?;
    let head = executor.publish(std::slice::from_ref(&incept)).await?;
    vault.incept_published(&incept.scid)?;
    Ok(Created {
        did,
        scid: incept.scid,
        device_key_id,
        head,
    })
}
