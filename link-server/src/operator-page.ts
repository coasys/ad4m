// The operator admission page, served by operator.ts. Kept as strings so
// `tsc` ships it in dist/ with no copy step. The script builds every node
// with textContent: room ids and DIDs are chosen by clients and must never
// be parsed as HTML.

export const OPERATOR_PAGE_HTML = `<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="robots" content="noindex, nofollow">
<title>Link server admission</title>
<style>
  body { font: 15px/1.45 system-ui, sans-serif; margin: 0 auto; max-width: 1100px; padding: 1rem 1.5rem; color: #1d1d1f; }
  h1 { font-size: 1.3rem; margin: 0 0 .25rem; }
  h2 { font-size: 1.05rem; margin: 1.5rem 0 .5rem; }
  .muted { color: #6e6e73; }
  table { border-collapse: collapse; width: 100%; }
  th, td { text-align: left; padding: .35rem .5rem; border-bottom: 1px solid #e5e5ea; vertical-align: top; }
  th { font-weight: 600; font-size: .85rem; color: #6e6e73; }
  td.mono, span.mono { font-family: ui-monospace, monospace; font-size: .82rem; word-break: break-all; }
  tr.room { cursor: pointer; }
  tr.room:hover, tr.room.selected { background: #f2f2f7; }
  .tag { display: inline-block; font-size: .75rem; padding: 0 .4rem; border-radius: .6rem; background: #e5e5ea; margin-right: .25rem; }
  .tag.admin { background: #ffe8a3; }
  .tag.online { background: #c9f0d1; }
  .tag.instance { font-size: .9rem; background: #d6e4ff; vertical-align: middle; }
  form { display: flex; gap: .5rem; margin: .75rem 0; }
  input[type=text] { flex: 1; font: inherit; font-family: ui-monospace, monospace; font-size: .85rem; padding: .35rem .5rem; }
  button { font: inherit; padding: .3rem .8rem; cursor: pointer; }
  #error { color: #b00020; min-height: 1.2rem; }
  .note { background: #f2f2f7; padding: .5rem .75rem; border-radius: .4rem; font-size: .9rem; }
</style>
</head>
<body>
<h1>Link server admission <span id="instance" class="tag instance"></span></h1>
<div class="muted">Signed in as <span id="who">?</span></div>
<p class="note">A room is a neighbourhood (the server link language's UID). Its first agent is the room admin.
An added DID can sync once it reconnects. It can read and write encrypted links only after the
room admin's executor has been online and granted it the room keys.</p>
<div id="error" role="alert"></div>
<h2>Rooms</h2>
<table id="rooms"><thead><tr><th>Room</th><th>Admin</th><th>Members</th><th>E2E</th><th>Created</th></tr></thead><tbody></tbody></table>
<section id="detail" hidden>
  <h2>Room <span id="detail-id" class="mono"></span></h2>
  <form id="add-form">
    <input id="add-did" type="text" placeholder="did:key:z6Mk..." autocomplete="off" spellcheck="false" required>
    <button type="submit">Admit DID</button>
  </form>
  <table id="members"><thead><tr><th>Member</th><th>Added</th><th>Added by</th><th>Keys</th><th></th></tr></thead><tbody></tbody></table>
  <h2>History</h2>
  <table id="history"><thead><tr><th>When</th><th>Change</th><th>DID</th><th>By</th></tr></thead><tbody></tbody></table>
</section>
<script src="admin.js"></script>
</body>
</html>
`;

export const OPERATOR_PAGE_JS = `"use strict";
(function () {
  var selected = null;
  var errorBox = document.getElementById("error");

  function el(tag, text, className) {
    var node = document.createElement(tag);
    if (text !== undefined && text !== null) node.textContent = String(text);
    if (className) node.className = className;
    return node;
  }

  function showError(message) { errorBox.textContent = message || ""; }

  function api(path, body) {
    var init = { credentials: "same-origin", headers: {} };
    if (body) {
      init.method = "POST";
      init.headers["content-type"] = "application/json";
      init.body = JSON.stringify(body);
    }
    return fetch("api" + path, init).then(function (res) {
      return res.json().catch(function () { return {}; }).then(function (data) {
        if (!res.ok) throw new Error(data.error || ("HTTP " + res.status));
        return data;
      });
    });
  }

  function loadRooms() {
    return api("/rooms").then(function (data) {
      var tbody = document.querySelector("#rooms tbody");
      tbody.replaceChildren();
      if (data.rooms.length === 0) {
        var empty = el("tr");
        var cell = el("td", "No rooms yet.", "muted");
        cell.colSpan = 5;
        empty.appendChild(cell);
        tbody.appendChild(empty);
      }
      data.rooms.forEach(function (room) {
        var row = el("tr", null, "room" + (room.id === selected ? " selected" : ""));
        row.appendChild(el("td", room.id, "mono"));
        row.appendChild(el("td", room.admin, "mono"));
        row.appendChild(el("td", room.memberCount));
        row.appendChild(el("td", room.e2e ? "yes" : "no"));
        row.appendChild(el("td", room.createdAt.slice(0, 16).replace("T", " ")));
        row.addEventListener("click", function () { selectRoom(room.id); });
        tbody.appendChild(row);
      });
    });
  }

  function renderRoom(room) {
    document.getElementById("detail").hidden = false;
    document.getElementById("detail-id").textContent = room.id;
    var tbody = document.querySelector("#members tbody");
    tbody.replaceChildren();
    room.members.forEach(function (member) {
      var row = el("tr");
      var who = el("td", null, "mono");
      if (member.did === room.admin) who.appendChild(el("span", "admin", "tag admin"));
      if (member.online) who.appendChild(el("span", "online", "tag online"));
      who.appendChild(el("span", member.did, "mono"));
      row.appendChild(who);
      row.appendChild(el("td", (member.addedAt || "").slice(0, 16).replace("T", " ")));
      row.appendChild(el("td", member.addedBy ? actorLabel(member.addedBy) : "", "mono"));
      row.appendChild(el("td", member.hasX25519Key ? "registered" : "not yet"));
      var action = el("td");
      if (member.did !== room.admin) {
        var remove = el("button", "Remove");
        remove.addEventListener("click", function () {
          if (!window.confirm("Remove " + member.did + " from " + room.id + "?")) return;
          changeAcl("remove", member.did);
        });
        action.appendChild(remove);
      }
      row.appendChild(action);
      tbody.appendChild(row);
    });
    renderHistory(room);
  }

  function actorLabel(change) {
    return change.source === "operator" ? change.actor : "room admin";
  }

  function renderHistory(room) {
    var tbody = document.querySelector("#history tbody");
    tbody.replaceChildren();
    room.history.forEach(function (change) {
      var row = el("tr");
      row.appendChild(el("td", change.at.slice(0, 16).replace("T", " ")));
      row.appendChild(el("td", change.action === "add" ? "admitted" : "removed"));
      row.appendChild(el("td", change.did, "mono"));
      row.appendChild(el("td", actorLabel(change), "mono"));
      tbody.appendChild(row);
    });
  }

  function selectRoom(id) {
    selected = id;
    showError("");
    return api("/rooms/" + encodeURIComponent(id)).then(renderRoom).then(loadRooms).catch(function (e) { showError(e.message); });
  }

  function changeAcl(action, did) {
    showError("");
    return api("/rooms/" + encodeURIComponent(selected) + "/acl", { action: action, did: did })
      .then(renderRoom).then(loadRooms)
      .catch(function (e) { showError(e.message); });
  }

  document.getElementById("add-form").addEventListener("submit", function (event) {
    event.preventDefault();
    var input = document.getElementById("add-did");
    var did = input.value.trim();
    if (!did) return;
    changeAcl("add", did).then(function () { if (!errorBox.textContent) input.value = ""; });
  });

  api("/whoami").then(function (data) {
    document.getElementById("who").textContent = data.operator || "(no sign-in header)";
    if (data.instance) {
      document.getElementById("instance").textContent = data.instance;
      document.title = "Link server admission: " + data.instance;
    }
  }).catch(function () {});
  loadRooms().catch(function (e) { showError(e.message); });
})();
`;
