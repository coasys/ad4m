// The operator page, served by operator.ts. Kept as strings so
// `tsc` ships it in dist/ with no copy step. The styles are a file of their
// own (admin.css), because the page's CSP (default-src 'self') blocks inline
// styles. The script builds every node with textContent: room ids and DIDs
// are chosen by clients and must never be parsed as HTML.

export const OPERATOR_PAGE_HTML = `<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="robots" content="noindex, nofollow">
<title>Link server rooms</title>
<link rel="stylesheet" href="admin.css">
</head>
<body>
<h1>Link server rooms <span id="instance" class="tag instance"></span></h1>
<div class="muted">Signed in as <span id="who">?</span></div>
<p id="admission" class="note"></p>
<p class="note">A room is a space. Who belongs to it is the room's own signed membership: its
creator is its first admin, and its admins add, remove and invite people. You cannot add or
remove anyone here; you can suspend a room, which closes it to everyone until you resume it.</p>
<div id="error" role="alert"></div>
<h2>Rooms</h2>
<table id="rooms"><thead><tr><th>Room</th><th>Creator</th><th>Members</th><th>Created</th><th>State</th></tr></thead><tbody></tbody></table>
<section id="detail" hidden>
  <h2>Room <span id="detail-id" class="mono"></span></h2>
  <p><span id="detail-state"></span> <button id="suspend" type="button"></button></p>
  <table id="members"><thead><tr><th>Member</th><th>Role</th><th>Since</th></tr></thead><tbody></tbody></table>
  <h2>Keys</h2>
  <table id="keys"><thead><tr><th>Key</th><th>Speaks for</th><th>State</th><th>Last seen</th></tr></thead><tbody></tbody></table>
</section>
<h2>Logs <select id="log-level"><option value="30">info and up</option><option value="40">warn and up</option><option value="50">error and up</option></select></h2>
<div id="log-lines" role="log"></div>
<script src="admin.js"></script>
</body>
</html>
`;

export const OPERATOR_PAGE_CSS = `body { font: 15px/1.45 system-ui, sans-serif; margin: 0 auto; max-width: 1100px; padding: 1rem 1.5rem; color: #1d1d1f; }
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
.tag.suspended, .tag.revoked { background: #ffd1d1; }
.tag.online { background: #c9f0d1; }
.tag.instance { font-size: .9rem; background: #d6e4ff; vertical-align: middle; }
form { display: flex; gap: .5rem; margin: .75rem 0; }
input[type=text] { flex: 1; font: inherit; font-family: ui-monospace, monospace; font-size: .85rem; padding: .35rem .5rem; }
button { font: inherit; padding: .3rem .8rem; cursor: pointer; }
#error { color: #b00020; min-height: 1.2rem; }
.note { background: #f2f2f7; padding: .5rem .75rem; border-radius: .4rem; font-size: .9rem; }
#log-lines { font: .78rem ui-monospace, monospace; height: 24rem; overflow-y: auto; background: #1d1d1f; color: #e5e5ea; padding: .5rem; white-space: pre-wrap; word-break: break-all; }
.l40 { color: #ffd60a; }
.l50, .l60 { color: #ff6961; }
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
        row.appendChild(el("td", room.creator, "mono"));
        row.appendChild(el("td", room.memberCount));
        row.appendChild(el("td", room.createdAt.slice(0, 16).replace("T", " ")));
        row.appendChild(el("td", stateOf(room)));
        row.addEventListener("click", function () { selectRoom(room.id); });
        tbody.appendChild(row);
      });
    });
  }

  function stateOf(room) {
    return room.legacy ? "legacy (closed)" : room.suspended ? "suspended" : "open";
  }

  function when(iso) { return (iso || "").slice(0, 16).replace("T", " "); }

  function renderRoom(room) {
    document.getElementById("detail").hidden = false;
    document.getElementById("detail-id").textContent = room.id;
    document.getElementById("detail-state").textContent = "State: " + stateOf(room);
    var toggle = document.getElementById("suspend");
    toggle.hidden = !!room.legacy;
    toggle.textContent = room.suspended ? "Resume room" : "Suspend room";
    toggle.onclick = function () {
      var action = room.suspended ? "resume" : "suspend";
      if (action === "suspend" && !window.confirm("Suspend " + room.id + "? Everyone in it is disconnected.")) return;
      showError("");
      api("/rooms/" + encodeURIComponent(room.id) + "/" + action, {})
        .then(renderRoom).then(loadRooms)
        .catch(function (e) { showError(e.message); });
    };

    var members = document.querySelector("#members tbody");
    members.replaceChildren();
    room.members.forEach(function (member) {
      var row = el("tr");
      var who = el("td", null, "mono");
      if (member.online) who.appendChild(el("span", "online", "tag online"));
      who.appendChild(el("span", member.person, "mono"));
      row.appendChild(who);
      var role = el("td");
      role.appendChild(el("span", member.role, member.role === "admin" ? "tag admin" : "tag"));
      if (member.botOf) role.appendChild(el("span", "of " + member.botOf, "mono"));
      row.appendChild(role);
      row.appendChild(el("td", when(member.since)));
      members.appendChild(row);
    });

    var keys = document.querySelector("#keys tbody");
    keys.replaceChildren();
    room.keys.forEach(function (key) {
      var row = el("tr");
      var who = el("td", null, "mono");
      if (key.online) who.appendChild(el("span", "online", "tag online"));
      who.appendChild(el("span", key.did, "mono"));
      row.appendChild(who);
      row.appendChild(el("td", key.person ? key.person + (key.byBot ? " (bot)" : "") : "no person", "mono"));
      var state = el("td");
      state.appendChild(el("span", key.revoked ? "revoked" : key.admitted ? "admitted" : "not admitted",
        key.revoked ? "tag revoked" : "tag"));
      row.appendChild(state);
      row.appendChild(el("td", when(key.updatedAt)));
      keys.appendChild(row);
    });
  }

  function selectRoom(id) {
    selected = id;
    showError("");
    return api("/rooms/" + encodeURIComponent(id)).then(renderRoom).then(loadRooms).catch(function (e) { showError(e.message); });
  }

  // Live logs (Server-Sent Events): the server's buffered lines, then each new
  // one. Every (re)connect resends the buffer, so the list starts over, and it
  // keeps as many rows as the server's buffer (LogTail: 1000).
  var logBox = document.getElementById("log-lines");
  var logLevel = document.getElementById("log-level");
  function filterLog(row) { row.hidden = row.level < Number(logLevel.value); }
  var logs = new EventSource("api/logs");
  logs.onopen = function () { logBox.replaceChildren(); };
  logs.onmessage = function (event) {
    var entry = JSON.parse(event.data);
    var row = el("div", null, "l" + (entry.level || 30));
    row.level = entry.level || 30;
    var head = (entry.time ? new Date(entry.time).toISOString().slice(11, 19) + " " : "") +
      ({ 40: "WARN ", 50: "ERROR ", 60: "FATAL " }[row.level] || "") + (entry.msg || "");
    delete entry.level; delete entry.time; delete entry.msg;
    row.textContent = Object.keys(entry).length ? head + " " + JSON.stringify(entry) : head;
    filterLog(row);
    var atBottom = logBox.scrollTop + logBox.clientHeight >= logBox.scrollHeight - 4;
    logBox.appendChild(row);
    if (logBox.childElementCount > 1000) logBox.firstChild.remove();
    if (atBottom) logBox.scrollTop = logBox.scrollHeight;
  };
  logLevel.addEventListener("change", function () { Array.prototype.forEach.call(logBox.children, filterLog); });

  api("/whoami").then(function (data) {
    document.getElementById("who").textContent = data.operator || "(no sign-in header)";
    document.getElementById("admission").textContent = data.admission === "auto"
      ? "Admission: auto (--auto-admit, tests and development only). Every key that authenticates is admitted to any room."
      : "Admission: membership (the default). A key is admitted while the person it speaks for is a member of the room.";
    if (data.instance) {
      document.getElementById("instance").textContent = data.instance;
      document.title = "Link server rooms: " + data.instance;
    }
  }).catch(function () {});
  loadRooms().catch(function (e) { showError(e.message); });
})();
`;
