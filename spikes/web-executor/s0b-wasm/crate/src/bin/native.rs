use s0b_oxigraph::*;
use oxigraph::store::Store;
use std::time::Instant;

fn rss() -> u64 {
    let s = std::fs::read_to_string("/proc/self/statm").unwrap();
    s.split_whitespace().nth(1).unwrap().parse::<u64>().unwrap() * 4096
}

fn main() {
    let n: usize = std::env::args().nth(1).map(|a| a.parse().unwrap()).unwrap_or(50_000);
    let per_link = std::env::args().nth(2).as_deref() == Some("per_link");
    let reps: usize = 10;
    let r0 = rss();
    let store = Store::new().unwrap();
    let t = Instant::now();
    load(&store, n, per_link);
    let load_ms = t.elapsed().as_secs_f64() * 1e3;
    let r1 = rss();
    println!("links={n} quads={} per_link={per_link} load_ms={load_ms:.1} rss_before={r0} rss_after={r1} delta={} bytes_per_link={:.0}",
        store.len().unwrap(), r1 - r0, (r1 - r0) as f64 / n as f64);
    let ids = page_ids(&store);
    for (name, q) in queries(&ids) {
        let rows = run(&store, &q); // warm-up
        let mut ts = vec![];
        for _ in 0..reps {
            let t = Instant::now();
            run(&store, &q);
            ts.push(t.elapsed().as_secs_f64() * 1e3);
        }
        ts.sort_by(|a, b| a.partial_cmp(b).unwrap());
        println!("{name}\trows={rows}\tmedian_ms={:.3}\tmin_ms={:.3}", ts[reps / 2], ts[0]);
    }
    println!("rss_end={}", rss());
    // Extra queries, one per line in the file named by S0B_EXTRA.
    if let Ok(f) = std::env::var("S0B_EXTRA") {
        for q in std::fs::read_to_string(f).unwrap().split("\n----\n").filter(|q| !q.trim().is_empty()) {
            let q = q.replace("__VALUES__", &ids.iter().map(|s| format!("<{s}>")).collect::<Vec<_>>().join(" "));
            let rows = run(&store, &q);
            let t = Instant::now();
            for _ in 0..5 { run(&store, &q); }
            println!("extra\trows={rows}\tavg_ms={:.3}\t{}", t.elapsed().as_secs_f64() * 200.0, q.lines().next().unwrap_or(""));
        }
    }
}
