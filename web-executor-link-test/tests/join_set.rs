//! `rt::JoinSet` on wasm32 (tokio's needs a runtime): the RPC connection loop runs each call in
//! one, reaps finished calls with `try_join_next` and waits for the rest with `join_next`.
#![cfg(target_arch = "wasm32")]

use futures::channel::oneshot;
use rust_executor::rt::JoinSet;
use std::cell::Cell;
use std::rc::Rc;
use wasm_bindgen_test::wasm_bindgen_test;

/// Lets every task that is ready run once.
async fn yield_to_tasks() {
    let (tx, rx) = oneshot::channel::<()>();
    drop(rust_executor::rt::spawn(async move {
        let _ = tx.send(());
    }));
    let _ = rx.await;
}

#[wasm_bindgen_test]
async fn finished_tasks_come_back_and_waiting_ones_do_not() {
    let mut set = JoinSet::new();
    let (release, wait) = oneshot::channel::<()>();
    set.spawn(async { 1 });
    set.spawn(async move {
        let _ = wait.await;
        2
    });
    assert!(set.try_join_next().is_none(), "nothing ran yet");
    yield_to_tasks().await;
    assert_eq!(set.try_join_next().unwrap().unwrap(), 1);
    assert!(set.try_join_next().is_none(), "the second task still waits");
    assert_eq!(set.len(), 1);
    release.send(()).unwrap();
    assert_eq!(set.join_next().await.unwrap().unwrap(), 2);
    assert!(set.join_next().await.is_none());
    assert!(set.is_empty());
}

#[wasm_bindgen_test]
async fn dropping_the_set_aborts_its_tasks() {
    let (_never, wait) = oneshot::channel::<()>();
    let reached_end = Rc::new(Cell::new(false));
    let flag = reached_end.clone();
    let mut set = JoinSet::new();
    set.spawn(async move {
        let _ = wait.await;
        flag.set(true);
    });
    yield_to_tasks().await;
    drop(set);
    yield_to_tasks().await;
    assert!(!reached_end.get());
}
