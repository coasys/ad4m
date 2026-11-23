//! The SFU's media sockets, and the host candidates it offers callers.
//!
//! Two modes, from the bind address [`super::server::sfu_bind_ip`] picks:
//!
//! - **Exact** (`--sfu-bind-addr`): one socket on that address; every answer
//!   offers it.
//! - **LAN default** (no bind address): a socket on loopback, and one on the
//!   LAN address, on the same port. Each answer offers both. The LAN address
//!   is detected again for every answer: when the machine moves to another
//!   network, the next call binds the new address (same port) and offers it.
//!   Loopback keeps working through the move. With no LAN (offline), answers
//!   offer loopback only.
//!
//! One socket per address, never `0.0.0.0`: str0m matches an inbound STUN
//! request to a local host candidate by the address it was sent to, and a
//! socket bound to one address knows that address exactly; a wildcard socket
//! does not (`recv_from` gives no destination). Replies leave from the same
//! address the caller sent to, as ICE requires.

use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use log::{info, warn};
use tokio::net::UdpSocket;

/// Most LAN sockets kept, one per network the machine has been on since
/// start. A socket on an address the machine no longer has receives nothing
/// and costs nothing; the oldest goes once there are more.
const MAX_LAN_SOCKETS: usize = 8;

/// How the LAN address is found: [`super::detect_outbound_ip`] in the
/// executor, a fixed answer in tests.
pub type DetectLan = Arc<dyn Fn() -> Result<IpAddr, String> + Send + Sync>;

/// The SFU's bound UDP sockets. Cheap to clone: clones share the sockets.
#[derive(Clone)]
pub struct MediaSockets {
    inner: Arc<Inner>,
}

struct Inner {
    /// `None`: one exact address, `exact`.
    lan: Option<LanDefault>,
    /// The exact address, or the loopback socket's in the LAN default.
    exact: SocketAddr,
    /// Bound sockets, by local address. The first is `exact`'s. Empty for
    /// [`MediaSockets::unbound`].
    sockets: Mutex<Vec<(SocketAddr, Arc<UdpSocket>)>>,
    /// Bumped whenever `sockets` changes, so the event loop rereads it.
    version: AtomicU64,
    /// The address startup found: the LAN socket's, else `exact`.
    primary: SocketAddr,
}

struct LanDefault {
    detect: DetectLan,
    /// The port every socket takes.
    port: u16,
    /// A fixed (forwarded) port: a LAN socket on another port is no use.
    fixed: bool,
    /// The last detection: `None` before the first, then the address or
    /// why there is none. Logged when it changes.
    last: Mutex<Option<Result<IpAddr, String>>>,
}

impl std::fmt::Debug for MediaSockets {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MediaSockets")
            .field("lan_default", &self.inner.lan.is_some())
            .field("primary", &self.inner.primary)
            .field("bound", &self.bound_addrs())
            .finish()
    }
}

impl From<SocketAddr> for MediaSockets {
    fn from(addr: SocketAddr) -> Self {
        Self::unbound(addr)
    }
}

impl MediaSockets {
    /// Bind the SFU's sockets. An unspecified IP (`0.0.0.0`) is the LAN
    /// default: loopback plus the LAN address [`super::detect_outbound_ip`]
    /// finds. Any other IP is bound exactly. Port 0 lets the OS pick; any
    /// other port is bound exactly, or this fails with a message that names it.
    pub async fn bind(bind_addr: SocketAddr) -> std::io::Result<Self> {
        Self::bind_with(bind_addr, Arc::new(super::detect_outbound_ip)).await
    }

    /// [`MediaSockets::bind`] with the LAN address from `detect`.
    pub async fn bind_with(bind_addr: SocketAddr, detect: DetectLan) -> std::io::Result<Self> {
        let fixed = bind_addr.port() != 0;
        if !bind_addr.ip().is_unspecified() {
            let socket = bind_udp(bind_addr, fixed)?;
            let addr = socket.local_addr()?;
            return Ok(Self::new(None, addr, vec![(addr, Arc::new(socket))], addr));
        }

        let loopback = bind_udp(
            SocketAddr::new(Ipv4Addr::LOCALHOST.into(), bind_addr.port()),
            fixed,
        )?;
        let loopback_addr = loopback.local_addr()?;
        let mut sockets = vec![(loopback_addr, Arc::new(loopback))];
        let found = detect();
        let mut primary = loopback_addr;
        match &found {
            Ok(ip) => match bind_lan(*ip, loopback_addr.port(), fixed) {
                Ok(socket) => {
                    let addr = socket.local_addr()?;
                    sockets.push((addr, Arc::new(socket)));
                    primary = addr;
                    info!(
                        "SFU media: loopback {} and LAN {}; the LAN address is detected again \
                         at each call",
                        loopback_addr, addr
                    );
                }
                // A forwarded port the LAN address cannot take: say so now,
                // as for the loopback socket.
                Err(e) if fixed => return Err(e),
                Err(e) => warn!(
                    "SFU media: loopback {}; the LAN address {} could not be bound ({}): calls \
                     offer loopback only until it can",
                    loopback_addr, ip, e
                ),
            },
            Err(why) => warn!("{}", no_lan_message(loopback_addr, why)),
        }
        let lan = LanDefault {
            detect,
            port: loopback_addr.port(),
            fixed,
            last: Mutex::new(Some(found)),
        };
        Ok(Self::new(Some(lan), loopback_addr, sockets, primary))
    }

    /// Addresses with no socket behind them: what [`MediaSockets::host_addrs`]
    /// offers is `addr`. For code that only builds SDP.
    pub fn unbound(addr: SocketAddr) -> Self {
        Self::new(None, addr, Vec::new(), addr)
    }

    fn new(
        lan: Option<LanDefault>,
        exact: SocketAddr,
        sockets: Vec<(SocketAddr, Arc<UdpSocket>)>,
        primary: SocketAddr,
    ) -> Self {
        Self {
            inner: Arc::new(Inner {
                lan,
                exact,
                sockets: Mutex::new(sockets),
                version: AtomicU64::new(0),
                primary,
            }),
        }
    }

    /// The address the SFU reports as its bind address: the LAN socket
    /// bound at startup, else loopback (LAN default); the exact address
    /// otherwise.
    pub fn primary(&self) -> SocketAddr {
        self.inner.primary
    }

    /// Whether this is the LAN default (loopback plus the LAN address).
    pub fn is_lan_default(&self) -> bool {
        self.inner.lan.is_some()
    }

    /// The host candidates to offer a new connection now. Exact: the bound
    /// address. LAN default: loopback, and the LAN address as detected now,
    /// bound first if it is new (another network since the last call).
    pub fn host_addrs(&self) -> Vec<SocketAddr> {
        let Some(lan) = &self.inner.lan else {
            return vec![self.inner.exact];
        };
        let mut hosts = vec![self.inner.exact];
        let found = (lan.detect)();
        let changed = {
            let mut last = lan.last.lock().expect("lan detection mutex poisoned");
            let changed = last.as_ref() != Some(&found);
            *last = Some(found.clone());
            changed
        };
        match found {
            Ok(ip) => match self.lan_socket(lan, ip) {
                Ok(addr) => {
                    if changed {
                        info!(
                            "SFU media: the LAN address is now {}; new calls offer it next to \
                             loopback {}",
                            addr, self.inner.exact
                        );
                    }
                    hosts.push(addr);
                }
                Err(e) => warn!(
                    "SFU media: the LAN address {} could not be bound on port {} ({}): this \
                     call is offered loopback only",
                    ip, lan.port, e
                ),
            },
            Err(why) if changed => warn!("{}", no_lan_message(self.inner.exact, &why)),
            Err(_) => {}
        }
        hosts
    }

    /// The socket on `ip`, bound now if there is none yet.
    fn lan_socket(&self, lan: &LanDefault, ip: IpAddr) -> std::io::Result<SocketAddr> {
        let mut sockets = self.inner.sockets.lock().expect("sockets mutex poisoned");
        if let Some((addr, _)) = sockets.iter().find(|(addr, _)| addr.ip() == ip) {
            return Ok(*addr);
        }
        // `from_std` registers the socket with the runtime the event loop runs on.
        if tokio::runtime::Handle::try_current().is_err() {
            return Err(std::io::Error::other(
                "no tokio runtime to register the socket",
            ));
        }
        let socket = bind_lan(ip, lan.port, lan.fixed)?;
        let addr = socket.local_addr()?;
        sockets.push((addr, Arc::new(socket)));
        // Never the loopback socket (the first), nor the primary: a public
        // mapping learned at startup forwards to it.
        if sockets.len() > MAX_LAN_SOCKETS + 1 {
            let oldest = (1..sockets.len())
                .find(|&i| sockets[i].0 != self.inner.primary)
                .expect("more LAN sockets than the primary");
            let (gone, _) = sockets.remove(oldest);
            info!(
                "SFU media: closing the socket on {}, the oldest network",
                gone
            );
        }
        self.inner.version.fetch_add(1, Ordering::Release);
        Ok(addr)
    }

    /// Every bound address, the exact/loopback one first.
    pub fn bound_addrs(&self) -> Vec<SocketAddr> {
        self.snapshot()
            .1
            .into_iter()
            .map(|(addr, _)| addr)
            .collect()
    }

    /// The socket bound on `addr`.
    pub fn socket(&self, addr: SocketAddr) -> Option<Arc<UdpSocket>> {
        let sockets = self.inner.sockets.lock().expect("sockets mutex poisoned");
        sockets
            .iter()
            .find(|(a, _)| *a == addr)
            .map(|(_, s)| Arc::clone(s))
    }

    /// The sockets now, with the version they were read at.
    pub(crate) fn snapshot(&self) -> (u64, Vec<(SocketAddr, Arc<UdpSocket>)>) {
        let sockets = self.inner.sockets.lock().expect("sockets mutex poisoned");
        (self.version(), sockets.clone())
    }

    /// Changes whenever a socket is added or closed.
    pub(crate) fn version(&self) -> u64 {
        self.inner.version.load(Ordering::Acquire)
    }
}

/// The socket a packet str0m sends from `source` leaves on: the one bound
/// there, else the first (the exact or loopback socket).
pub(crate) fn socket_for(
    sockets: &[(SocketAddr, Arc<UdpSocket>)],
    source: SocketAddr,
) -> Option<&UdpSocket> {
    sockets
        .iter()
        .find(|(addr, _)| *addr == source)
        .or_else(|| sockets.first())
        .map(|(_, socket)| socket.as_ref())
}

fn no_lan_message(loopback: SocketAddr, why: &str) -> String {
    format!(
        "SFU media: no LAN address ({}); calls offer loopback {} only. Firefox on this machine \
         (it gathers no loopback candidate) and other machines cannot reach the media until \
         a network is up; each call looks again",
        why, loopback
    )
}

/// Bind `addr` for the event loop. A fixed port that is taken fails with a
/// message that names it.
fn bind_udp(addr: SocketAddr, fixed: bool) -> std::io::Result<UdpSocket> {
    let bound = std::net::UdpSocket::bind(addr).and_then(|socket| {
        socket.set_nonblocking(true)?;
        UdpSocket::from_std(socket)
    });
    bound.map_err(|e| {
        if fixed {
            std::io::Error::new(
                e.kind(),
                format!(
                    "SFU UDP port {} on {} is not available ({}): free it, or set another \
                     --sfu-udp-port and forward that one",
                    addr.port(),
                    addr.ip(),
                    e
                ),
            )
        } else {
            e
        }
    })
}

/// Bind the LAN address on the loopback socket's port, so one port number
/// serves both. An ephemeral port taken on the LAN address takes another.
fn bind_lan(ip: IpAddr, port: u16, fixed: bool) -> std::io::Result<UdpSocket> {
    match bind_udp(SocketAddr::new(ip, port), fixed) {
        Err(_) if !fixed => bind_udp(SocketAddr::new(ip, 0), false),
        bound => bound,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicU8;

    fn ip(s: &str) -> IpAddr {
        s.parse().unwrap()
    }

    /// The LAN `lan` names, switched by the test: 0 offline, else
    /// `127.0.0.<n>` (every 127/8 address is on Linux's `lo`).
    fn lan_from(lan: Arc<AtomicU8>) -> DetectLan {
        Arc::new(move || match lan.load(Ordering::SeqCst) {
            0 => Err("no default route".to_string()),
            n => Ok(IpAddr::V4(Ipv4Addr::new(127, 0, 0, n))),
        })
    }

    fn lan_default() -> SocketAddr {
        SocketAddr::new(Ipv4Addr::UNSPECIFIED.into(), 0)
    }

    #[tokio::test]
    async fn an_exact_address_is_the_only_one_offered() {
        let lan = Arc::new(AtomicU8::new(2));
        let sockets = MediaSockets::bind_with("127.0.0.1:0".parse().unwrap(), lan_from(lan))
            .await
            .unwrap();
        assert!(!sockets.is_lan_default());
        let hosts = sockets.host_addrs();
        assert_eq!(hosts, vec![sockets.primary()]);
        assert_eq!(hosts[0].ip(), ip("127.0.0.1"));
        assert_ne!(hosts[0].port(), 0);
        assert_eq!(sockets.bound_addrs(), hosts);
    }

    #[cfg(target_os = "linux")]
    #[tokio::test]
    async fn the_default_offers_loopback_and_the_lan_on_one_port() {
        let lan = Arc::new(AtomicU8::new(2));
        let sockets = MediaSockets::bind_with(lan_default(), lan_from(lan))
            .await
            .unwrap();
        assert!(sockets.is_lan_default());
        let hosts = sockets.host_addrs();
        assert_eq!(hosts.len(), 2, "{hosts:?}");
        assert_eq!(hosts[0].ip(), ip("127.0.0.1"));
        assert_eq!(hosts[1].ip(), ip("127.0.0.2"));
        assert_eq!(hosts[0].port(), hosts[1].port(), "one port for both");
        assert_eq!(
            sockets.primary(),
            hosts[1],
            "the LAN is what status reports"
        );
        assert_eq!(sockets.bound_addrs(), hosts);
    }

    #[tokio::test]
    async fn with_no_lan_the_default_offers_loopback_only() {
        let lan = Arc::new(AtomicU8::new(0));
        let sockets = MediaSockets::bind_with(lan_default(), lan_from(lan))
            .await
            .unwrap();
        let hosts = sockets.host_addrs();
        assert_eq!(hosts.len(), 1, "{hosts:?}");
        assert!(hosts[0].ip().is_loopback());
        assert_eq!(sockets.primary(), hosts[0]);
    }

    /// A laptop that moves to another network: the next call binds and
    /// offers the new address on the same port, loopback stays, and the old
    /// socket is kept (the address may come back). Going offline offers
    /// loopback only; coming back reuses the socket.
    #[cfg(target_os = "linux")]
    #[tokio::test]
    async fn a_new_lan_address_is_bound_at_the_next_call() {
        let lan = Arc::new(AtomicU8::new(2));
        let sockets = MediaSockets::bind_with(lan_default(), lan_from(Arc::clone(&lan)))
            .await
            .unwrap();
        let first = sockets.host_addrs();
        let version = sockets.version();

        lan.store(3, Ordering::SeqCst);
        let moved = sockets.host_addrs();
        assert_eq!(moved[0], first[0], "loopback is unchanged");
        assert_eq!(moved[1], SocketAddr::new(ip("127.0.0.3"), first[0].port()));
        assert_ne!(
            sockets.version(),
            version,
            "the event loop rereads the sockets"
        );
        assert_eq!(sockets.bound_addrs(), vec![first[0], first[1], moved[1]]);
        // The new socket receives.
        let probe = std::net::UdpSocket::bind("127.0.0.1:0").unwrap();
        probe.send_to(b"ping", moved[1]).unwrap();
        let mut buf = [0u8; 8];
        let socket = sockets.socket(moved[1]).unwrap();
        let (n, from) = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            socket.recv_from(&mut buf),
        )
        .await
        .unwrap()
        .unwrap();
        assert_eq!(
            (&buf[..n], from),
            (&b"ping"[..], probe.local_addr().unwrap())
        );

        lan.store(0, Ordering::SeqCst);
        assert_eq!(sockets.host_addrs(), vec![first[0]]);

        lan.store(2, Ordering::SeqCst);
        let version = sockets.version();
        assert_eq!(sockets.host_addrs(), first, "back home: the same socket");
        assert_eq!(sockets.version(), version);
    }

    /// Many networks later, the oldest LAN sockets close, but never the one
    /// bound at startup: a public mapping learned there forwards to it.
    #[cfg(target_os = "linux")]
    #[tokio::test]
    async fn the_startup_socket_outlives_the_networks_after_it() {
        let lan = Arc::new(AtomicU8::new(2));
        let sockets = MediaSockets::bind_with(lan_default(), lan_from(Arc::clone(&lan)))
            .await
            .unwrap();
        let primary = sockets.primary();
        assert_eq!(primary.ip(), ip("127.0.0.2"));
        for n in 10..20 {
            lan.store(n, Ordering::SeqCst);
            sockets.host_addrs();
        }
        let bound = sockets.bound_addrs();
        assert_eq!(bound.len(), MAX_LAN_SOCKETS + 1, "{bound:?}");
        assert!(bound.contains(&primary), "{bound:?}");
        assert!(sockets.socket(primary).is_some());
        assert_eq!(
            bound.last().unwrap().ip(),
            ip("127.0.0.19"),
            "the newest is kept"
        );
        assert!(
            !bound.iter().any(|a| a.ip() == ip("127.0.0.10")),
            "the oldest after the primary went: {bound:?}"
        );
    }

    /// Offline at start, online later: the first call after binds the LAN.
    #[cfg(target_os = "linux")]
    #[tokio::test]
    async fn a_lan_that_comes_up_after_start_is_offered() {
        let lan = Arc::new(AtomicU8::new(0));
        let sockets = MediaSockets::bind_with(lan_default(), lan_from(Arc::clone(&lan)))
            .await
            .unwrap();
        assert_eq!(sockets.host_addrs().len(), 1);
        lan.store(4, Ordering::SeqCst);
        let hosts = sockets.host_addrs();
        assert_eq!(hosts.len(), 2, "{hosts:?}");
        assert_eq!(hosts[1].ip(), ip("127.0.0.4"));
    }

    #[cfg(target_os = "linux")]
    #[tokio::test]
    async fn a_taken_fixed_port_on_the_lan_fails_with_a_message_that_names_it() {
        let taken = std::net::UdpSocket::bind("127.0.0.5:0").unwrap();
        let port = taken.local_addr().unwrap().port();
        let lan = Arc::new(AtomicU8::new(5));
        let err = MediaSockets::bind_with(
            SocketAddr::new(Ipv4Addr::UNSPECIFIED.into(), port),
            lan_from(lan),
        )
        .await
        .err()
        .expect("the port is taken on the LAN address");
        let message = err.to_string();
        assert!(
            message.contains(&format!("SFU UDP port {port}")),
            "{message}"
        );
        assert!(message.contains("127.0.0.5"), "{message}");
    }

    #[test]
    fn a_packet_leaves_on_the_socket_it_names_as_its_source() {
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap();
        rt.block_on(async {
            let a = Arc::new(UdpSocket::bind("127.0.0.1:0").await.unwrap());
            let b = Arc::new(UdpSocket::bind("127.0.0.1:0").await.unwrap());
            let (a_addr, b_addr) = (a.local_addr().unwrap(), b.local_addr().unwrap());
            let sockets = vec![(a_addr, a), (b_addr, b)];
            let on = |source| socket_for(&sockets, source).unwrap().local_addr().unwrap();
            assert_eq!(on(b_addr), b_addr);
            assert_eq!(on(a_addr), a_addr);
            assert_eq!(
                on("192.0.2.1:9".parse().unwrap()),
                a_addr,
                "unknown: the first"
            );
            assert!(socket_for(&[], a_addr).is_none());
        });
    }
}
