//! Keys, hashes, cursors and timestamps of the scope index.

use super::*;

#[test]
fn member_key_bytes_sort_as_time_then_seq() {
    let keys = [
        MemberKey::new(0, 0),
        MemberKey::new(0, 1),
        MemberKey::new(0, SEQ_MASK),
        MemberKey::new(1, 0),
        MemberKey::new(T_LATE, 5),
        MemberKey::new(T_LATE + 1, 0),
        MemberKey::new(MAX_T_MS, SEQ_MASK),
    ];
    for pair in keys.windows(2) {
        assert!(pair[0] < pair[1], "{:?} < {:?}", pair[0], pair[1]);
        assert!(
            pair[0].to_bytes() < pair[1].to_bytes(),
            "big-endian bytes sort as the keys do"
        );
    }
}

const T_LATE: u64 = 1_790_000_000_000;

#[test]
fn member_key_parts_round_trip() {
    let k = MemberKey::new(T_LATE, 12345);
    assert_eq!(k.t_ms(), T_LATE);
    assert_eq!(k.seq(), 12345);
    assert_eq!(MemberKey::from_bytes(&k.to_bytes()), Some(k));
    assert_eq!(MemberKey::from_bytes(&[1, 2, 3]), None);
    // seq keeps its low 20 bits; t_ms is clamped to the largest it holds.
    assert_eq!(MemberKey::new(7, (1 << SEQ_BITS) + 3).seq(), 3);
    assert_eq!(MemberKey::new(u64::MAX, 0).t_ms(), MAX_T_MS);
}

#[test]
fn entry_keys_group_by_scope_and_sort_by_member_key() {
    let a = Scope::Class { class: "A".into() }.id();
    let b = Scope::Class { class: "B".into() }.id();
    let (lo, hi) = if a < b { (a, b) } else { (b, a) };
    let k1 = entry_key(lo, MemberKey::new(5, 0));
    let k2 = entry_key(lo, MemberKey::new(6, 0));
    let k3 = entry_key(hi, MemberKey::new(0, 0));
    assert!(
        k1 < k2 && k2 < k3,
        "a scope's entries are contiguous, in key order"
    );
    assert!(k1.starts_with(&entry_prefix(lo)) && k2 < prefix_end(&entry_prefix(lo)));
    assert!(k3 >= prefix_end(&entry_prefix(lo)));
    assert_eq!(k1.len(), 25);
    assert_eq!(member_scope_key(&member_hash("m"), lo).len(), 33);
    assert_eq!(flag_key(&member_hash("m"), ClassId::of("A")).len(), 33);
    assert_eq!(count_key(lo).len(), 17);
    assert_eq!(build_key(ClassId::of("A")).len(), 17);
}

#[test]
fn prefix_end_is_the_next_prefix() {
    assert_eq!(prefix_end(&[1, 2, 3]), vec![1, 2, 4]);
    assert_eq!(prefix_end(&[1, 0xff]), vec![2]);
    assert_eq!(prefix_end(&[0xff, 0xff]), Vec::<u8>::new());
}

#[test]
fn scope_hashes_keep_their_parts_apart() {
    let under = |c: &str, p: &str, r: &str| {
        Scope::Under {
            class: c.into(),
            parent: p.into(),
            relation: r.into(),
        }
        .id()
    };
    assert_ne!(under("ab", "c", "d"), under("a", "bc", "d"));
    assert_ne!(under("a", "b", "cd"), under("a", "bc", "d"));
    assert_ne!(
        Scope::Class { class: "x".into() }.id(),
        under("x", "", ""),
        "a class scope is not an Under scope"
    );
    assert_ne!(
        Scope::Class { class: "x".into() }.id(),
        noncanonical_scope("x")
    );
    assert_eq!(under("a", "b", "c"), under("a", "b", "c"));
}

#[test]
fn cursor_tokens_round_trip_and_reject_garbage() {
    let c = Cursor(MemberKey::new(T_LATE, 42));
    let token = c.to_token();
    assert_eq!(token.len(), 16);
    assert_eq!(Cursor::parse(&token).unwrap(), c);
    assert!(Cursor::parse("xyz").is_err());
    assert!(Cursor::parse("zzzzzzzzzzzzzzzz").is_err());
}

#[test]
fn canonical_timestamps_are_the_executor_form() {
    assert_eq!(
        parse_timestamp("2026-09-23T12:00:00.123Z"),
        (1_790_164_800_123, true)
    );
    // RFC 3339 that is not the executor's form: its time, not canonical.
    let (ms, canonical) = parse_timestamp("2026-09-23T12:00:00Z");
    assert_eq!((ms, canonical), (1_790_164_800_000, false));
    assert!(!parse_timestamp("2026-09-23T14:00:00.000+02:00").1);
    assert!(!parse_timestamp("2026-09-23T12:00:00.123456Z").1);
    // Not a time at all, or before 1970.
    assert_eq!(parse_timestamp("yesterday"), (0, false));
    assert_eq!(parse_timestamp("1969-12-31T23:59:59.000Z"), (0, false));
}

#[test]
fn canonical_strings_sort_as_their_times() {
    let times = [0u64, 1, 999, 1000, 59_999, T_LATE, T_LATE + 1, MAX_T_MS];
    let strings: Vec<String> = times
        .iter()
        .map(|t| {
            chrono::DateTime::<chrono::Utc>::from_timestamp_millis(*t as i64)
                .unwrap()
                .to_rfc3339_opts(chrono::SecondsFormat::Millis, true)
        })
        .collect();
    for (s, t) in strings.iter().zip(times) {
        assert_eq!(parse_timestamp(s), (t, true));
    }
    let mut sorted = strings.clone();
    sorted.sort();
    assert_eq!(sorted, strings, "the SPARQL string order is the time order");
    // Past the 44 bits of a member key (the year 2527) a time is clamped, and
    // not canonical: its class goes to SPARQL.
    assert_eq!(
        parse_timestamp("3000-01-01T00:00:00.000Z"),
        (MAX_T_MS, false)
    );
}

#[test]
fn a_full_page_has_a_next_cursor() {
    let members = vec![
        ("c".to_string(), MemberKey::new(3, 0)),
        ("b".to_string(), MemberKey::new(2, 0)),
    ];
    let page = ScopePage::from_entries(members.clone(), 2);
    assert_eq!(page.after, Some(Cursor(MemberKey::new(2, 0))));
    assert_eq!(page.newest, Some(Cursor(MemberKey::new(3, 0))));
    let short = ScopePage::from_entries(members, 3);
    assert_eq!(short.after, None);
}
