//! The SFU's media loop in a process of its own (`--sfu-process`).
//!
//! The executor starts `ad4m-executor sfu-worker` as a child process. The
//! child binds the SFU's UDP socket, probes it, and runs the same media loop
//! (`SfuServer::event_loop`) an executor runs in-process: every `Rtc`, all
//! forwarding, SRTP and RTCP live there, on the child's own CPU budget. The
//! executor keeps what it always had: rooms, membership and capability
//! checks, rosters and the clients' events. Clients see no difference: they
//! signal through the executor's RPC and send media to the address the
//! answer names, which is the child's socket.
//!
//! The two talk over the child's stdin and stdout, one JSON message per line:
//! - first the executor writes the [`WorkerConfig`]; the child answers
//!   [`FromWorker::Ready`] with its bound and advertised addresses;
//! - then each [`SfuCommand`] the service sends crosses as a [`ToWorker`].
//!   A join's peer is built in the child (`Prepare` → `Prepared` with the
//!   SDP answer) and added by `Add` once the room accepted the participant;
//! - the media loop's client events (`sfu-data`, renegotiation offers) and
//!   dead peers come back as [`FromWorker`], and the executor publishes them.
//!
//! The child stops when its stdin closes, so it never outlives the executor.
//! Cascade pipes are built in the executor and cannot cross: the executor
//! runs the SFU in-process when cascade gossip is configured.

use std::collections::HashMap;
use std::net::{IpAddr, SocketAddr};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex as StdMutex};
use std::time::{Duration, Instant};

use log::{error, info, warn};
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncBufReadExt, AsyncRead, AsyncWrite, AsyncWriteExt, BufReader, Lines};
use tokio::sync::{mpsc, oneshot};

use super::reachability::{Advertised, SfuReachability};
use super::room::{ParticipantId, RoomId};
use super::server::{DeadPeer, EventSink, SfuCommand, SfuPeer, SfuServer, SfuServerConfig};
use super::{SfuCallRenegotiationOffer, SfuDataMessage};

/// The hidden `ad4m-executor` subcommand that runs [`run_worker`].
pub const WORKER_SUBCOMMAND: &str = "sfu-worker";

/// How long the child may take to bind and probe before the executor gives up.
const READY_TIMEOUT: Duration = Duration::from_secs(30);
/// How long the executor waits for the child to answer one request.
const REQUEST_TIMEOUT: Duration = Duration::from_secs(30);
/// A prepared peer the room never accepted (a refused join) is dropped after this.
const PREPARED_TTL: Duration = Duration::from_secs(60);

/// What the child needs to bind its socket: the in-process SFU's settings.
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct WorkerConfig {
    bind_addr: SocketAddr,
    stun_server: Option<String>,
    public_ip: Option<IpAddr>,
}

/// Executor → child. Participants are named by their `ParticipantId` number.
#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "op", rename_all = "camelCase")]
pub(crate) enum ToWorker {
    /// Answer `sdp_offer` and keep the peer until `Add` (or [`PREPARED_TTL`]).
    #[serde(rename_all = "camelCase")]
    Prepare {
        req: u64,
        pid: u64,
        neighbourhood_url: String,
        room_name: String,
        agent_did: String,
        sdp_offer: String,
    },
    Add {
        pid: u64,
    },
    Remove {
        pid: u64,
    },
    SetQualityPreference {
        pid: u64,
        preference: String,
    },
    #[serde(rename_all = "camelCase")]
    ApplyServerAnswer {
        pid: u64,
        sdp_answer: String,
    },
    AddIceCandidate {
        pid: u64,
        candidate: String,
    },
    RelayData {
        pid: u64,
        label: String,
        data: Vec<u8>,
        binary: bool,
    },
    RestartIce {
        pid: u64,
    },
    GetQualityPreferences {
        req: u64,
    },
    Shutdown,
}

/// Child → executor.
#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "op", rename_all = "camelCase")]
pub(crate) enum FromWorker {
    #[serde(rename_all = "camelCase")]
    Ready {
        local_addr: SocketAddr,
        advertised: Option<Advertised>,
        reachability: SfuReachability,
    },
    Prepared {
        req: u64,
        answer: Result<String, String>,
    },
    QualityPreferences {
        req: u64,
        preferences: Vec<(u64, String)>,
    },
    #[serde(rename_all = "camelCase")]
    DeadPeer {
        pid: u64,
        neighbourhood_url: String,
        room_name: String,
        agent_did: String,
        is_pipe: bool,
    },
    Data {
        payload: SfuDataMessage,
        recipients: Vec<String>,
    },
    Renegotiation {
        payload: SfuCallRenegotiationOffer,
    },
}

fn io_error(message: impl Into<String>) -> std::io::Error {
    std::io::Error::other(message.into())
}

async fn write_line<W: AsyncWrite + Unpin, T: Serialize>(
    out: &mut W,
    message: &T,
) -> std::io::Result<()> {
    let mut line = serde_json::to_vec(message).map_err(|e| io_error(e.to_string()))?;
    line.push(b'\n');
    out.write_all(&line).await?;
    out.flush().await
}

/// Write every message `rx` yields to `out`, one per line, until either side closes.
fn spawn_writer<W, T>(mut out: W, mut rx: mpsc::UnboundedReceiver<T>)
where
    W: AsyncWrite + Unpin + Send + 'static,
    T: Serialize + Send + Sync + 'static,
{
    tokio::spawn(async move {
        while let Some(message) = rx.recv().await {
            if let Err(e) = write_line(&mut out, &message).await {
                warn!("SFU worker link: write failed: {}", e);
                break;
            }
        }
    });
}

// ── The child ──────────────────────────────────────────────────────────────

/// `ad4m-executor sfu-worker`: run the media loop on stdin/stdout until
/// stdin closes. Logs go to stderr, which the executor's log inherits.
pub async fn run_worker() -> std::io::Result<()> {
    let _ = env_logger::Builder::new()
        .parse_default_env()
        .format(|buf, record| {
            use std::io::Write;
            writeln!(
                buf,
                "[sfu-worker {} {}] {}",
                record.level(),
                record.target(),
                record.args()
            )
        })
        .try_init();
    run_worker_on(tokio::io::stdin(), tokio::io::stdout()).await
}

pub(crate) async fn run_worker_on<R, W>(input: R, output: W) -> std::io::Result<()>
where
    R: AsyncRead + Unpin,
    W: AsyncWrite + Unpin + Send + 'static,
{
    let mut lines = BufReader::new(input).lines();
    let first = lines
        .next_line()
        .await?
        .ok_or_else(|| io_error("stdin closed before the config"))?;
    let config: WorkerConfig =
        serde_json::from_str(&first).map_err(|e| io_error(format!("bad config: {}", e)))?;

    let (out_tx, out_rx) = mpsc::unbounded_channel::<FromWorker>();
    let server = SfuServer::start_with_sink(
        SfuServerConfig {
            bind_addr: config.bind_addr,
            stun_server: config.stun_server,
            public_ip: config.public_ip,
            ..Default::default()
        },
        EventSink::Worker(out_tx.clone()),
    )
    .await?;
    let _ = out_tx.send(FromWorker::Ready {
        local_addr: server.local_addr,
        advertised: server.advertised,
        reachability: server.reachability.clone(),
    });
    spawn_writer(output, out_rx);
    info!(
        "SFU worker {} serving on {}",
        std::process::id(),
        server.local_addr
    );

    let dead_peers = server
        .dead_peer_rx
        .lock()
        .expect("dead_peer_rx mutex poisoned")
        .take();
    if let Some(mut dead_peers) = dead_peers {
        let tx = out_tx.clone();
        tokio::spawn(async move {
            while let Some(dead) = dead_peers.recv().await {
                let _ = tx.send(FromWorker::DeadPeer {
                    pid: dead.participant_id.0,
                    neighbourhood_url: dead.room_id.neighbourhood_url,
                    room_name: dead.room_id.room_name,
                    agent_did: dead.agent_did,
                    is_pipe: dead.is_pipe,
                });
            }
        });
    }

    let prepared: Arc<StdMutex<HashMap<u64, (SfuPeer, Instant)>>> = Default::default();
    while let Some(line) = lines.next_line().await? {
        let message: ToWorker = match serde_json::from_str(&line) {
            Ok(m) => m,
            Err(e) => {
                warn!("SFU worker: unreadable message ({}): {}", e, line);
                continue;
            }
        };
        let pid = |n: u64| ParticipantId(n);
        let command = match message {
            ToWorker::Prepare {
                req,
                pid: n,
                neighbourhood_url,
                room_name,
                agent_did,
                sdp_offer,
            } => {
                // Building an Rtc makes its DTLS certificate: off the reader.
                let (local_addr, advertised) = (server.local_addr, server.advertised);
                let (prepared, tx) = (Arc::clone(&prepared), out_tx.clone());
                tokio::task::spawn_blocking(move || {
                    let answer = serde_json::from_str(&sdp_offer)
                        .map_err(|e| format!("Invalid SDP offer: {}", e))
                        .and_then(|offer| {
                            SfuServer::create_rtc_for_offer(
                                offer,
                                local_addr,
                                advertised.map(|a| a.addr),
                            )
                        })
                        .map(|(rtc, answer)| {
                            let room = RoomId::new(neighbourhood_url, room_name);
                            let peer = SfuPeer::new(pid(n), room, agent_did, rtc, false);
                            let now = Instant::now();
                            let mut prepared = prepared.lock().expect("prepared mutex poisoned");
                            prepared.retain(|_, (_, at)| now.duration_since(*at) < PREPARED_TTL);
                            prepared.insert(n, (peer, now));
                            answer
                        });
                    let _ = tx.send(FromWorker::Prepared { req, answer });
                });
                continue;
            }
            ToWorker::Add { pid: n } => {
                let peer = prepared.lock().expect("prepared mutex poisoned").remove(&n);
                match peer {
                    Some((peer, _)) => SfuCommand::AddPeer(peer),
                    None => {
                        warn!("SFU worker: Add for P{} with no prepared peer", n);
                        continue;
                    }
                }
            }
            ToWorker::Remove { pid: n } => {
                prepared.lock().expect("prepared mutex poisoned").remove(&n);
                SfuCommand::RemovePeer(pid(n))
            }
            ToWorker::SetQualityPreference { pid: n, preference } => {
                SfuCommand::SetQualityPreference {
                    participant_id: pid(n),
                    preference,
                }
            }
            ToWorker::ApplyServerAnswer { pid: n, sdp_answer } => SfuCommand::ApplyServerAnswer {
                participant_id: pid(n),
                sdp_answer_json: sdp_answer,
            },
            ToWorker::AddIceCandidate { pid: n, candidate } => SfuCommand::AddIceCandidate {
                participant_id: pid(n),
                candidate_sdp: candidate,
            },
            ToWorker::RelayData {
                pid: n,
                label,
                data,
                binary,
            } => SfuCommand::RelayData {
                participant_id: pid(n),
                channel_label: label,
                data,
                binary,
            },
            ToWorker::RestartIce { pid: n } => SfuCommand::RestartIce {
                participant_id: pid(n),
            },
            ToWorker::GetQualityPreferences { req } => {
                let (reply, rx) = oneshot::channel();
                if server
                    .command_tx
                    .send(SfuCommand::GetQualityPreferences { reply })
                    .await
                    .is_err()
                {
                    break;
                }
                let tx = out_tx.clone();
                tokio::spawn(async move {
                    let preferences = rx
                        .await
                        .unwrap_or_default()
                        .into_iter()
                        .map(|(p, pref)| (p.0, pref))
                        .collect();
                    let _ = tx.send(FromWorker::QualityPreferences { req, preferences });
                });
                continue;
            }
            ToWorker::Shutdown => break,
        };
        if server.command_tx.send(command).await.is_err() {
            break;
        }
    }
    let _ = server.command_tx.send(SfuCommand::Shutdown).await;
    info!("SFU worker {} stopping", std::process::id());
    Ok(())
}

// ── The executor's side ────────────────────────────────────────────────────

/// The executor's end of the link to its SFU worker process.
pub struct WorkerLink {
    to_worker: mpsc::UnboundedSender<ToWorker>,
    /// Requests waiting for their answer; `None` once the worker is gone.
    waiting: StdMutex<Option<HashMap<u64, oneshot::Sender<FromWorker>>>>,
    next_req: AtomicU64,
    /// The child, killed when the link is dropped.
    _child: Option<tokio::process::Child>,
    /// The child's pid, for `sfu.status` readers and measurements.
    pub pid: Option<u32>,
}

impl WorkerLink {
    /// Send a request; its receiver fails at once when the worker is gone.
    fn request(&self, make: impl FnOnce(u64) -> ToWorker) -> oneshot::Receiver<FromWorker> {
        let req = self.next_req.fetch_add(1, Ordering::Relaxed);
        let (tx, rx) = oneshot::channel();
        let mut waiting = self.waiting.lock().expect("waiting mutex poisoned");
        if let Some(waiting) = waiting.as_mut() {
            if self.to_worker.send(make(req)).is_ok() {
                waiting.insert(req, tx);
            }
        }
        rx
    }

    /// Have the worker answer a client's offer and keep its peer.
    pub(crate) async fn prepare(
        &self,
        pid: &ParticipantId,
        room_id: &RoomId,
        agent_did: &str,
        sdp_offer_json: &str,
    ) -> Result<String, String> {
        let rx = self.request(|req| ToWorker::Prepare {
            req,
            pid: pid.0,
            neighbourhood_url: room_id.neighbourhood_url.clone(),
            room_name: room_id.room_name.clone(),
            agent_did: agent_did.to_string(),
            sdp_offer: sdp_offer_json.to_string(),
        });
        match tokio::time::timeout(REQUEST_TIMEOUT, rx).await {
            Ok(Ok(FromWorker::Prepared { answer, .. })) => answer,
            Ok(Ok(other)) => Err(format!("SFU worker answered {:?}", other)),
            Ok(Err(_)) => Err("The SFU worker process is gone".to_string()),
            Err(_) => Err("The SFU worker process did not answer".to_string()),
        }
    }
}

/// Start the SFU worker process and link it: the [`SfuServer`] the service
/// uses, with `worker` set.
pub(crate) async fn start(config: SfuServerConfig) -> std::io::Result<SfuServer> {
    let exe = std::env::current_exe()?;
    let mut child = tokio::process::Command::new(&exe)
        .arg(WORKER_SUBCOMMAND)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::inherit())
        .kill_on_drop(true)
        .spawn()
        .map_err(|e| {
            io_error(format!(
                "could not start {} {}: {}",
                exe.display(),
                WORKER_SUBCOMMAND,
                e
            ))
        })?;
    let stdin = child.stdin.take().ok_or_else(|| io_error("no stdin"))?;
    let stdout = child.stdout.take().ok_or_else(|| io_error("no stdout"))?;
    let pid = child.id();
    let server = connect(config, stdout, stdin, Some(child)).await?;
    info!("SFU: media loop runs in process {:?}", pid);
    Ok(server)
}

/// Link to a worker reading `to` and writing `from`: send the config, wait
/// for `Ready`, then bridge commands and events.
pub(crate) async fn connect<R, W>(
    config: SfuServerConfig,
    from: R,
    mut to: W,
    child: Option<tokio::process::Child>,
) -> std::io::Result<SfuServer>
where
    R: AsyncRead + Unpin + Send + 'static,
    W: AsyncWrite + Unpin + Send + 'static,
{
    write_line(
        &mut to,
        &WorkerConfig {
            bind_addr: config.bind_addr,
            stun_server: config.stun_server.clone(),
            public_ip: config.public_ip,
        },
    )
    .await?;
    let mut lines = BufReader::new(from).lines();
    let ready = tokio::time::timeout(READY_TIMEOUT, lines.next_line())
        .await
        .map_err(|_| io_error("the SFU worker did not start in time"))??
        .ok_or_else(|| io_error("the SFU worker exited before it was ready"))?;
    let FromWorker::Ready {
        local_addr,
        advertised,
        reachability,
    } = serde_json::from_str(&ready).map_err(|e| io_error(format!("bad Ready: {}", e)))?
    else {
        return Err(io_error(format!("expected Ready, got {}", ready)));
    };

    let (to_tx, to_rx) = mpsc::unbounded_channel();
    spawn_writer(to, to_rx);
    let pid = child.as_ref().and_then(|c| c.id());
    let link = Arc::new(WorkerLink {
        to_worker: to_tx,
        waiting: StdMutex::new(Some(HashMap::new())),
        next_req: AtomicU64::new(1),
        _child: child,
        pid,
    });
    let (command_tx, command_rx) = mpsc::channel(256);
    let (dead_tx, dead_rx) = mpsc::unbounded_channel();
    tokio::spawn(forward_commands(command_rx, Arc::clone(&link)));
    tokio::spawn(read_events(lines, Arc::clone(&link), dead_tx));

    Ok(SfuServer {
        local_addr,
        advertised,
        reachability,
        command_tx,
        dead_peer_rx: StdMutex::new(Some(dead_rx)),
        worker: Some(link),
    })
}

/// The service's commands, to the worker.
async fn forward_commands(mut commands: mpsc::Receiver<SfuCommand>, link: Arc<WorkerLink>) {
    while let Some(command) = commands.recv().await {
        let message = match command {
            SfuCommand::AddPrepared(p) => ToWorker::Add { pid: p.0 },
            SfuCommand::AddPeer(peer) => {
                warn!(
                    "SFU: peer {} was built in the executor (a cascade pipe) and cannot move to \
                     the worker process; dropped",
                    peer.id
                );
                continue;
            }
            SfuCommand::ApplyPipeRenegotiationOffer { participant_id, .. } => {
                warn!(
                    "SFU: pipe renegotiation for {} dropped: cascade pipes need the in-process SFU",
                    participant_id
                );
                continue;
            }
            SfuCommand::RemovePeer(p) => ToWorker::Remove { pid: p.0 },
            SfuCommand::SetQualityPreference {
                participant_id,
                preference,
            } => ToWorker::SetQualityPreference {
                pid: participant_id.0,
                preference,
            },
            SfuCommand::ApplyServerAnswer {
                participant_id,
                sdp_answer_json,
            } => ToWorker::ApplyServerAnswer {
                pid: participant_id.0,
                sdp_answer: sdp_answer_json,
            },
            SfuCommand::AddIceCandidate {
                participant_id,
                candidate_sdp,
            } => ToWorker::AddIceCandidate {
                pid: participant_id.0,
                candidate: candidate_sdp,
            },
            SfuCommand::RelayData {
                participant_id,
                channel_label,
                data,
                binary,
            } => ToWorker::RelayData {
                pid: participant_id.0,
                label: channel_label,
                data,
                binary,
            },
            SfuCommand::RestartIce { participant_id } => ToWorker::RestartIce {
                pid: participant_id.0,
            },
            SfuCommand::GetQualityPreferences { reply } => {
                let rx = link.request(|req| ToWorker::GetQualityPreferences { req });
                tokio::spawn(async move {
                    if let Ok(FromWorker::QualityPreferences { preferences, .. }) = rx.await {
                        let _ = reply.send(
                            preferences
                                .into_iter()
                                .map(|(p, pref)| (ParticipantId(p), pref))
                                .collect(),
                        );
                    }
                });
                continue;
            }
            SfuCommand::Shutdown => ToWorker::Shutdown,
        };
        if link.to_worker.send(message).is_err() {
            break;
        }
    }
}

/// The worker's answers and events, to their waiters and the event stream.
async fn read_events<R: AsyncRead + Unpin>(
    mut lines: Lines<BufReader<R>>,
    link: Arc<WorkerLink>,
    dead_tx: mpsc::UnboundedSender<DeadPeer>,
) {
    while let Ok(Some(line)) = lines.next_line().await {
        let message: FromWorker = match serde_json::from_str(&line) {
            Ok(m) => m,
            Err(e) => {
                warn!("SFU worker link: unreadable message ({}): {}", e, line);
                continue;
            }
        };
        match message {
            FromWorker::Prepared { req, .. } | FromWorker::QualityPreferences { req, .. } => {
                let waiter = link
                    .waiting
                    .lock()
                    .expect("waiting mutex poisoned")
                    .as_mut()
                    .and_then(|waiting| waiting.remove(&req));
                if let Some(waiter) = waiter {
                    let _ = waiter.send(message);
                }
            }
            FromWorker::DeadPeer {
                pid,
                neighbourhood_url,
                room_name,
                agent_did,
                is_pipe,
            } => {
                let _ = dead_tx.send(DeadPeer {
                    room_id: RoomId::new(neighbourhood_url, room_name),
                    agent_did,
                    participant_id: ParticipantId(pid),
                    is_pipe,
                });
            }
            FromWorker::Data {
                payload,
                recipients,
            } => crate::pubsub::get_global_pubsub_sync()
                .publish_event(crate::events::Event::sfu_data(&payload, recipients)),
            FromWorker::Renegotiation { payload } => crate::pubsub::get_global_pubsub_sync()
                .publish_event(crate::events::Event::sfu_renegotiation_offer(&payload)),
            FromWorker::Ready { .. } => {}
        }
    }
    error!("SFU worker process ended: calls on this executor stop until it restarts");
    // Fails every waiting request, and every later one at once.
    link.waiting.lock().expect("waiting mutex poisoned").take();
}

#[cfg(test)]
mod tests {
    use super::*;
    use str0m::change::SdpAnswer;
    use str0m::Rtc;

    /// A worker on in-memory pipes, linked as the executor links its child.
    async fn linked() -> SfuServer {
        let (exec_end, worker_end) = tokio::io::duplex(1 << 16);
        let (worker_in, worker_out) = tokio::io::split(worker_end);
        tokio::spawn(async move { run_worker_on(worker_in, worker_out).await });
        let (exec_in, exec_out) = tokio::io::split(exec_end);
        connect(SfuServerConfig::default(), exec_in, exec_out, None)
            .await
            .unwrap()
    }

    fn caller_offer() -> (Rtc, str0m::change::SdpPendingOffer, String) {
        let mut caller = Rtc::builder().build(Instant::now());
        let mut change = caller.sdp_api();
        change.add_media(
            str0m::media::MediaKind::Audio,
            str0m::media::Direction::SendRecv,
            None,
            None,
            None,
        );
        let (offer, pending) = change.apply().unwrap();
        let json = serde_json::to_string(&offer).unwrap();
        (caller, pending, json)
    }

    #[tokio::test]
    async fn the_worker_binds_answers_and_takes_commands_in_order() {
        let server = linked().await;
        assert!(server.local_addr.ip().is_loopback());
        assert_ne!(server.local_addr.port(), 0);
        assert!(server.worker.is_some());

        let (mut caller, pending, offer) = caller_offer();
        let pid = ParticipantId::next();
        let room = RoomId::new("neighbourhood://n", "room");
        let (add, answer) = server
            .prepare_peer(pid.clone(), room, "did:key:a", &offer)
            .await
            .unwrap();
        assert!(matches!(&add, SfuCommand::AddPrepared(p) if *p == pid));
        // The answer names the worker's socket, and the caller accepts it.
        assert!(answer.contains(&server.local_addr.port().to_string()));
        let answer: SdpAnswer = serde_json::from_str(&answer).unwrap();
        caller.sdp_api().accept_answer(pending, answer).unwrap();

        server.command_tx.send(add).await.unwrap();
        server
            .command_tx
            .send(SfuCommand::SetQualityPreference {
                participant_id: pid.clone(),
                preference: "low".into(),
            })
            .await
            .unwrap();
        let (reply, rx) = oneshot::channel();
        server
            .command_tx
            .send(SfuCommand::GetQualityPreferences { reply })
            .await
            .unwrap();
        let preferences = tokio::time::timeout(Duration::from_secs(5), rx)
            .await
            .unwrap()
            .unwrap();
        assert_eq!(preferences.get(&pid).map(String::as_str), Some("low"));
    }

    #[tokio::test]
    async fn a_bad_offer_is_refused_by_the_worker() {
        let server = linked().await;
        let err = server
            .worker
            .as_ref()
            .unwrap()
            .prepare(
                &ParticipantId::next(),
                &RoomId::new("neighbourhood://n", "room"),
                "did:key:a",
                "{\"type\":\"offer\",\"sdp\":\"nonsense\"}",
            )
            .await
            .unwrap_err();
        assert!(err.contains("SDP"), "{}", err);
    }

    #[tokio::test]
    async fn a_worker_that_is_gone_fails_the_join_at_once() {
        let (exec_end, worker_end) = tokio::io::duplex(1 << 16);
        let (worker_in, mut worker_out) = tokio::io::split(worker_end);
        // A worker that says Ready, then exits.
        tokio::spawn(async move {
            let mut lines = BufReader::new(worker_in).lines();
            lines.next_line().await.unwrap();
            write_line(
                &mut worker_out,
                &FromWorker::Ready {
                    local_addr: "127.0.0.1:40000".parse().unwrap(),
                    advertised: None,
                    reachability: SfuReachability::Unknown {
                        reason: "test".into(),
                    },
                },
            )
            .await
            .unwrap();
        });
        let (exec_in, exec_out) = tokio::io::split(exec_end);
        let server = connect(SfuServerConfig::default(), exec_in, exec_out, None)
            .await
            .unwrap();
        let (_, _, offer) = caller_offer();
        let err = tokio::time::timeout(
            Duration::from_secs(5),
            server.prepare_peer(
                ParticipantId::next(),
                RoomId::new("neighbourhood://n", "room"),
                "did:key:a",
                &offer,
            ),
        )
        .await
        .expect("no wait for the request timeout")
        .unwrap_err();
        assert!(err.contains("gone"), "{}", err);
    }
}
