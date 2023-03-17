import { render, renderMerges } from "./render";
import { unSyncFetch, mergeFetch, mergeFetchDeep } from "./pull";
import { testRevisionUpdates } from "./revisions";
import { stressTest } from "./stress"
import { signals } from "./signals";
import { testTelepresence } from "./telepresence";

import test from "tape-promise/tape.js";

test("unsynced fetch", async (t) => {
    await unSyncFetch(t);
})

test("merge fetch", async (t) => {
    await mergeFetch(t);
})

test("merge fetch deep", async (t) => {
    await mergeFetchDeep(t);
})

//test("stress", async (t) => {
//    await stressTest(t);
//})


//test("complex merge", async (t) => {
//    await complexMerge(t);
//})

test("test revision updates", async (t) => {
    await testRevisionUpdates(t);
})

test("render", async (t) => {
    await render(t)
})

test("render merges", async (t) => {
    await renderMerges(t)
})

test("signals", async (t) => {
    await signals(t)
})

test("telepresence", async (t) => {
    await testTelepresence(t)
})
