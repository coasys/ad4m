#[macro_use]
extern crate lazy_static;

use hdk::prelude::*;
use inputs::PullArguments;
use lazy_static::lazy_static;

use perspective_diff_sync_integrity::{
    HashBroadcast, OnlineAgent, OnlineAgentAndAction, Perspective, PerspectiveDiff,
    PerspectiveExpression, PullResult,
};

mod errors;
mod inputs;
mod link_adapter;
mod retriever;
mod telepresence;
mod utils;

#[macro_use]
extern crate maplit;

pub type Hash = HoloHash<holo_hash::hash_type::Action>;

#[hdk_extern]
fn init(_: ()) -> ExternResult<InitCallbackResult> {
    let mut functions = BTreeSet::new();
    functions.insert((zome_info()?.name, "get_online_status".into()));
    //TODO; is this next function needed?
    functions.insert((zome_info()?.name, "recv_send_remote_signal".into()));

    let functions: GrantedFunctions = GrantedFunctions::Listed(functions);

    create_cap_grant(CapGrantEntry {
        tag: "".into(),
        // empty access converts to unrestricted
        access: ().into(),
        functions,
    })?;
    link_adapter::commit::add_active_agent_link::<retriever::HolochainRetreiver>()
        .map_err(|error| utils::err(&format!("{}", error)))?;
    Ok(InitCallbackResult::Pass)
}

/// LinkLanguage implementation

#[hdk_extern]
pub fn commit(diff: PerspectiveDiff) -> ExternResult<Hash> {
    info!("commit");
    let commit_result = link_adapter::commit::commit::<retriever::HolochainRetreiver>(diff)
        .map_err(|error| utils::err(&format!("{}", error)));
    info!("commit_result: {:?}", commit_result);
    commit_result
}

#[hdk_extern]
pub fn current_revision(_: ()) -> ExternResult<Option<Hash>> {
    link_adapter::revisions::current_revision::<retriever::HolochainRetreiver>()
        .map_err(|error| utils::err(&format!("{}", error)))
        .map(|val| val.map(|val| val.hash))
}

#[hdk_extern]
pub fn sync(_: ()) -> ExternResult<Option<Hash>> {
    info!("sync");
    let broadcast_result = link_adapter::commit::broadcast_current::<retriever::HolochainRetreiver>()
        .map_err(|error| utils::err(&format!("{}", error)));
    info!("broadcast_result: {:?}", broadcast_result);
    broadcast_result
}

#[hdk_extern]
pub fn pull(args: PullArguments) -> ExternResult<PullResult> {
    info!("pull");
    let pull_result = link_adapter::pull::pull::<retriever::HolochainRetreiver>(true, args.hash, args.is_scribe)
        .map_err(|error| utils::err(&format!("{}", error)));
    info!("pull_result: {:?}", pull_result);
    pull_result
}

#[hdk_extern]
pub fn render(_: ()) -> ExternResult<Perspective> {
    link_adapter::render::render::<retriever::HolochainRetreiver>()
        .map_err(|error| utils::err(&format!("{}", error)))
}

#[hdk_extern]
pub fn update_current_revision(_hash: Hash) -> ExternResult<()> {
    #[cfg(feature = "test")]
    {
        link_adapter::revisions::update_current_revision::<retriever::HolochainRetreiver>(
            _hash,
            utils::get_now().unwrap(),
        )
        .map_err(|err| utils::err(&format!("{}", err)))?;
    }
    Ok(())
}

/// Signal handling

#[hdk_extern]
fn recv_send_remote_signal(signal: SerializedBytes) -> ExternResult<()> {
    //Check if its a normal diff expression signal
    match HashBroadcast::try_from(signal.clone()) {
        Ok(broadcast) => {
            link_adapter::pull::handle_broadcast::<retriever::HolochainRetreiver>(broadcast)
                .map_err(|err| utils::err(&format!("{}", err)))?;
        }
        //Check if its a broadcast message
        Err(_) => match PerspectiveExpression::try_from(signal.clone()) {
            Ok(sig) => emit_signal(sig)?,
            //Check if its an online ping
            Err(_) => return Err(utils::err(&format!("Signal not recognized: {:?}", signal))),
        },
    };
    Ok(())
}

// Telepresence implementation

#[hdk_extern]
pub fn set_online_status(status: PerspectiveExpression) -> ExternResult<()> {
    telepresence::status::set_online_status(status)
        .map_err(|error| utils::err(&format!("{}", error)))?;
    Ok(())
}

#[hdk_extern]
pub fn create_did_pub_key_link(did: String) -> ExternResult<()> {
    telepresence::status::create_did_pub_key_link(did)
        .map_err(|error| utils::err(&format!("{}", error)))?;
    Ok(())
}

#[hdk_extern]
pub fn get_online_agents(_: ()) -> ExternResult<Vec<OnlineAgent>> {
    let res = telepresence::status::get_online_agents()
        .map_err(|error| utils::err(&format!("{}", error)))?;
    Ok(res)
}

#[hdk_extern]
pub fn get_online_status(_: ()) -> ExternResult<OnlineAgentAndAction> {
    let res = telepresence::status::get_online_status()
        .map_err(|error| utils::err(&format!("{}", error)))?;
    Ok(res)
}

#[hdk_extern]
pub fn get_agents_status(agent: AgentPubKey) -> ExternResult<Option<OnlineAgent>> {
    let res = telepresence::status::get_agents_status(agent);
    Ok(res)
}

#[hdk_extern]
pub fn send_signal(signal_data: inputs::SignalData) -> ExternResult<PerspectiveExpression> {
    let res = telepresence::signal::send_signal(signal_data)
        .map_err(|error| utils::err(&format!("{}", error)))?;
    Ok(res)
}

#[hdk_extern]
pub fn send_broadcast(data: PerspectiveExpression) -> ExternResult<PerspectiveExpression> {
    let res = telepresence::signal::send_broadcast(data)
        .map_err(|error| utils::err(&format!("{}", error)))?;
    Ok(res)
}

#[hdk_extern]
pub fn get_active_agents(_: ()) -> ExternResult<Vec<AgentPubKey>> {
    let res = retriever::holochain::get_active_agents()
        .map_err(|error| utils::err(&format!("{}", error)))?;
    Ok(res)
}

#[hdk_extern]
pub fn get_others(_: ()) -> ExternResult<Vec<String>> {
    info!("get_others");
    let res =
        telepresence::status::get_others().map_err(|error| utils::err(&format!("{}", error)))?;
    info!("get_others res: {:?}", res);
    Ok(res)
}

//not loading from DNA properies since dna zome properties is always null for some reason
lazy_static! {
    pub static ref ACTIVE_AGENT_DURATION: chrono::Duration = chrono::Duration::seconds(3600);
    pub static ref ENABLE_SIGNALS: bool = true;
    pub static ref SNAPSHOT_INTERVAL: usize = 100;
    pub static ref CHUNK_SIZE: u16 = 10000;
}
