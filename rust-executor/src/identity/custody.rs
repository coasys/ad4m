//! The custody channel: the path by which the desktop app's custody service takes a master key
//! that arrived sealed to this computer's device key. It is no listener of its own: it is the
//! custody credential on a loopback listener of an executor started with `--custody-channel`.
//! The credential arrives as the first line of stdin
//! ([`read_custody_credential`](crate::config::read_custody_credential)), never in the
//! environment or on the command line, which the admin credential could read back as host files.
//! The admin credential never opens the channel: an app's window may hold that one, and must not
//! take a master key. [`admit`] is its one guard, for `identity.custodyStatus`,
//! `identity.takeMaster` and a claim that keeps the master key (`identity.claimOffer` with
//! `canKeepMaster`).

use ad4m_identity_core::Multikey;

use crate::agent::session::{OperatorSource, Session};
use crate::agent::AgentContext;
use crate::api::ws_handler::WsRpcError;

use super::types::DeviceKeys;

/// What the channel answers a caller it does not admit.
pub const NO_CUSTODY_CHANNEL: &str = "no-custody-channel";

/// Admits only the custody credential on a loopback listener, on an executor started with
/// `--custody-channel`. The admin credential, a tokenless operator and an operator from a key
/// login are refused: only the credential the supervisor handed to the custody service alone
/// opens the channel.
pub fn admit(session: &Session, channel_on: bool) -> Result<(), WsRpcError> {
    if !channel_on {
        return Err(WsRpcError::forbidden(format!(
            "{NO_CUSTODY_CHANNEL}: this executor was not started with --custody-channel"
        )));
    }
    if session.operator_source() != Some(OperatorSource::Custody)
        || !session.is_operator_on_this_machine()
    {
        return Err(WsRpcError::forbidden(format!(
            "{NO_CUSTODY_CHANNEL}: only the custody credential on a loopback listener reaches \
             the custody channel"
        )));
    }
    Ok(())
}

/// The main agent's device keys: the `did:key` it signs with, and the X25519 key a sender seals
/// to (both derive from the one wallet key).
pub fn main_device_keys() -> Result<DeviceKeys, WsRpcError> {
    let (sign, key): (String, Multikey) = super::device::device_key(&AgentContext::main_agent())
        .map_err(|e| WsRpcError::conflict(format!("the main agent has no device key: {e}")))?;
    let enc = super::device::enc_key_of(&key).map_err(WsRpcError::internal)?;
    Ok(DeviceKeys { sign, enc })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::session::Principal;

    fn code(result: Result<(), WsRpcError>) -> Result<(), (u16, bool)> {
        result.map_err(|e| (e.code, e.message.starts_with(NO_CUSTODY_CHANNEL)))
    }

    /// Only the custody credential on a loopback listener, and only with the flag: the admin
    /// credential, which an app's window may hold, is refused.
    #[test]
    fn the_custody_channel_admits_only_the_custody_credential_on_loopback() {
        let custody = Session::for_test(Principal::Operator).as_custody();
        assert_eq!(code(admit(&custody, true)), Ok(()));
        assert_eq!(code(admit(&custody, false)), Err((403, true)), "no flag");

        let custody_on_network = Session::for_test(Principal::Operator)
            .as_custody()
            .on_network();
        assert_eq!(code(admit(&custody_on_network, true)), Err((403, true)));

        let admin = Session::for_test(Principal::Operator);
        assert_eq!(code(admit(&admin, true)), Err((403, true)), "admin");

        let on_network = Session::for_test(Principal::Operator).on_network();
        assert_eq!(code(admit(&on_network, true)), Err((403, true)));

        let key_login = Session::for_test(Principal::Operator).from_key_login();
        assert_eq!(code(admit(&key_login, true)), Err((403, true)));

        let tokenless = Session::for_test(Principal::Operator).without_credential();
        assert_eq!(code(admit(&tokenless, true)), Err((403, true)));

        let app = Session::for_test(Principal::Agent(AgentContext::main_agent()))
            .with_caps(vec![crate::agent::capabilities::ALL_CAPABILITY.clone()]);
        assert_eq!(code(admit(&app, true)), Err((403, true)));
    }
}
