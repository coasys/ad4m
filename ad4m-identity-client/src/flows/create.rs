//! `create`: make an identity on this surface for its executor.
//!
//! 1. New words; the surface shows them ([`CreateScreen::ShowWords`]).
//! 2. The person types two of them back ([`CreateScreen::ConfirmWords`]); the words are wiped,
//!    the seed stays for the flow.
//! 3. Master #0 and the commitment to master #1 from the seed; an `incept` listing the
//!    executor's device key as `d1` with its `enc` key (`identity.status`); the executor adds
//!    its proof (`identity.cosignIncept`); the client folds the event; `identity.publish`.
//! 4. The vault keeps master #0 (after the publish, so a cancelled unlock leaves
//!    nothing in the store).

use ad4m_identity_core::build::{incept_human, Signer};
use ad4m_identity_core::words::{self, Seed, Words};
use ad4m_identity_core::{DeviceKey, Event, KeyLife};
use ad4m_ldk::identity::SignedHead;

use crate::executor::{ExecutorRpc, ViaExecutor};
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

/// Step 3 and 4 of `create` from the seed of confirmed words.
pub async fn create_from_seed<R: ExecutorRpc>(
    seed: &Seed,
    executor: &ViaExecutor<R>,
    vault: &dyn Vault,
) -> Result<Created, ClientError> {
    let status = executor.status().await?;
    if let Some(did) = status.did {
        return Err(ClientError::HasIdentity(did));
    }
    let master = words::master_key(seed, 0)?;
    let (next, _) = words::next_commitment(seed, 1)?;
    let device = DeviceKey {
        id: "d1".into(),
        sign: status.device_key.clone(),
        enc: status.device_enc.clone(),
        life: KeyLife::Standing,
    };
    let incept = incept_human(&master, next, vec![device])?;
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
    let head = executor.publish(std::slice::from_ref(&cosigned)).await?;
    let scid = cosigned.scid.clone();
    vault.store_master(&scid, 0, Secret::new(master.to_bytes()))?;
    Ok(Created {
        did: ad4m_identity_core::did_of(&scid),
        scid,
        device_key_id: "d1".into(),
        head,
    })
}
