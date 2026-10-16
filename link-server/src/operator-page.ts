// The operator admission page, served by operator.ts. Kept as strings so
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
<title>Link server admission</title>
<link rel="stylesheet" href="admin.css">
</head>
<body>
<h1>Link server admission <span id="instance" class="tag instance"></span></h1>
<div class="muted">Signed in as <span id="who">?</span></div>
<p id="admission" class="note" hidden></p>
<p class="note">A room is a neighbourhood (the server link language's UID). Its first agent is the room admin.
An added DID can sync once it reconnects. It can read and write encrypted links only after the
room admin's executor has been online and granted it the room keys.</p>
<div id="error" role="alert"></div>
<h2>Rooms</h2>
<table id="rooms"><thead><tr><th>Room</th><th>Admin</th><th>Members</th><th>Waiting</th><th>E2E</th><th>Created</th></tr></thead><tbody></tbody></table>
<section id="detail" hidden>
  <h2>Room <span id="detail-id" class="mono"></span></h2>
  <form id="add-form">
    <input id="add-did" type="text" placeholder="did:key:z6Mk..." autocomplete="off" spellcheck="false" required>
    <button type="submit">Admit DID</button>
  </form>
  <section id="pending-section" hidden>
    <h2>Waiting for approval</h2>
    <p class="muted">These DIDs proved their key and asked to join. Check each one with the person before you admit it.</p>
    <table id="pending"><thead><tr><th>DID</th><th>First asked</th><th>Last asked</th><th>Attempts</th><th></th></tr></thead><tbody></tbody></table>
  </section>
  <table id="members"><thead><tr><th>Member</th><th>Added</th><th>Added by</th><th>Keys</th><th></th></tr></thead><tbody></tbody></table>
  <h2>History</h2>
  <table id="history"><thead><tr><th>When</th><th>Change</th><th>DID</th><th>By</th></tr></thead><tbody></tbody></table>
</section>
<section id="logs">
  <h2>Logs <span id="log-status" class="tag">connecting</span></h2>
  <div class="log-controls">
    <label>Level <select id="log-level">
      <option value="30" selected>info and up</option>
      <option value="40">warn and up</option>
      <option value="50">error and up</option>
    </select></label>
    <label><input id="log-pause" type="checkbox"> Pause</label>
    <button id="log-clear" type="button">Clear</button>
  </div>
  <div id="log-lines" role="log" aria-live="off"></div>
</section>
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
.tag.online { background: #c9f0d1; }
.tag.instance { font-size: .9rem; background: #d6e4ff; vertical-align: middle; }
form { display: flex; gap: .5rem; margin: .75rem 0; }
input[type=text] { flex: 1; font: inherit; font-family: ui-monospace, monospace; font-size: .85rem; padding: .35rem .5rem; }
button { font: inherit; padding: .3rem .8rem; cursor: pointer; }
#error { color: #b00020; min-height: 1.2rem; }
.note { background: #f2f2f7; padding: .5rem .75rem; border-radius: .4rem; font-size: .9rem; }
.log-controls { display: flex; gap: 1rem; align-items: center; margin: .5rem 0; font-size: .9rem; }
#log-lines { font-family: ui-monospace, monospace; font-size: .78rem; height: 24rem; overflow-y: auto; background: #1d1d1f; color: #e5e5ea; padding: .5rem; border-radius: .4rem; }
.log-line { white-space: pre-wrap; word-break: break-all; }
.log-line .lvl { display: inline-block; width: 3.2rem; }
.log-line.l20 .lvl { color: #8e8e93; }
.log-line.l30 .lvl { color: #64d2ff; }
.log-line.l40 .lvl { color: #ffd60a; }
.log-line.l50 .lvl, .log-line.l60 .lvl { color: #ff6961; }
.log-line .fields { color: #8e8e93; }
.tag.live { background: #c9f0d1; }
td.waiting { font-weight: 600; color: #b25000; }
.tag.down { background: #ffd1d1; }
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
        cell.colSpan = 6;
        empty.appendChild(cell);
        tbody.appendChild(empty);
      }
      data.rooms.forEach(function (room) {
        var row = el("tr", null, "room" + (room.id === selected ? " selected" : ""));
        row.appendChild(el("td", room.id, "mono"));
        row.appendChild(el("td", room.admin, "mono"));
        row.appendChild(el("td", room.memberCount));
        row.appendChild(el("td", room.pendingCount ? String(room.pendingCount) : "", room.pendingCount ? "waiting" : null));
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
    renderPending(room);
    renderHistory(room);
  }

  function renderPending(room) {
    var pending = room.pending || [];
    document.getElementById("pending-section").hidden = pending.length === 0;
    var tbody = document.querySelector("#pending tbody");
    tbody.replaceChildren();
    pending.forEach(function (request) {
      var row = el("tr");
      row.appendChild(el("td", request.did, "mono"));
      row.appendChild(el("td", request.firstSeen.slice(0, 16).replace("T", " ")));
      row.appendChild(el("td", request.lastSeen.slice(0, 16).replace("T", " ")));
      row.appendChild(el("td", String(request.attempts)));
      var action = el("td");
      var admit = el("button", "Admit");
      admit.addEventListener("click", function () { changeAcl("add", request.did); });
      action.appendChild(admit);
      row.appendChild(action);
      tbody.appendChild(row);
    });
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

  // Live logs: Server-Sent Events from api/logs. The browser reconnects by
  // itself and the server resumes after the last line it sent.
  var LEVELS = { 10: "trace", 20: "debug", 30: "info", 40: "warn", 50: "error", 60: "fatal" };
  var MAX_LOG_LINES = 1000;
  var logBox = document.getElementById("log-lines");
  var logStatus = document.getElementById("log-status");
  var logLevel = document.getElementById("log-level");
  var logPause = document.getElementById("log-pause");
  var logEntries = [];

  function setLogStatus(text, className) {
    logStatus.textContent = text;
    logStatus.className = "tag " + className;
  }

  // The fields worth a glance; the rest of the line stays out of the way.
  function logFields(entry) {
    var parts = [];
    if (entry.req) parts.push(entry.req.method + " " + entry.req.url);
    if (entry.res) parts.push(String(entry.res.statusCode));
    if (typeof entry.responseTime === "number") parts.push(Math.round(entry.responseTime) + " ms");
    ["operator", "instance", "roomId", "action", "did"].forEach(function (key) {
      if (entry[key] !== undefined && entry[key] !== null) parts.push(key + "=" + entry[key]);
    });
    if (entry.err && entry.err.message) parts.push("error: " + entry.err.message);
    return parts.join(" ");
  }

  function logRow(entry) {
    var row = el("div", null, "log-line l" + entry.level);
    row.appendChild(el("span", new Date(entry.time).toLocaleTimeString() + " "));
    row.appendChild(el("span", LEVELS[entry.level] || entry.level, "lvl"));
    row.appendChild(el("span", entry.msg + " "));
    row.appendChild(el("span", logFields(entry), "fields"));
    return row;
  }

  function renderLogs() {
    var min = Number(logLevel.value);
    var atBottom = logBox.scrollTop + logBox.clientHeight >= logBox.scrollHeight - 4;
    logBox.replaceChildren();
    logRows.clear();
    logEntries.forEach(function (entry) {
      if (entry.level < min) return;
      var row = logRow(entry);
      logRows.set(entry, row);
      logBox.appendChild(row);
    });
    if (atBottom) logBox.scrollTop = logBox.scrollHeight;
  }

  // Each shown entry keeps its row, so the row goes when its entry leaves the
  // buffer, whatever the filter hides.
  var logRows = new Map();

  function addLog(entry) {
    logEntries.push(entry);
    if (logEntries.length > MAX_LOG_LINES) {
      var dropped = logEntries.shift();
      var oldRow = logRows.get(dropped);
      if (oldRow) { oldRow.remove(); logRows.delete(dropped); }
    }
    if (logPause.checked || entry.level < Number(logLevel.value)) return;
    var atBottom = logBox.scrollTop + logBox.clientHeight >= logBox.scrollHeight - 4;
    var row = logRow(entry);
    logRows.set(entry, row);
    logBox.appendChild(row);
    if (atBottom) logBox.scrollTop = logBox.scrollHeight;
  }

  if (window.EventSource) {
    var logs = new EventSource("api/logs");
    logs.onopen = function () { setLogStatus("live", "live"); };
    logs.onerror = function () {
      // A refused stream (logging off, too many streams) is closed for good; a dropped one retries.
      setLogStatus(logs.readyState === EventSource.CLOSED ? "unavailable" : "reconnecting", "down");
    };
    logs.onmessage = function (event) {
      try { addLog(JSON.parse(event.data)); } catch (e) { /* not a log line */ }
    };
  } else {
    setLogStatus("not supported by this browser", "down");
  }
  logLevel.addEventListener("change", renderLogs);
  logPause.addEventListener("change", function () { if (!logPause.checked) renderLogs(); });
  document.getElementById("log-clear").addEventListener("click", function () {
    logEntries = [];
    logRows.clear();
    logBox.replaceChildren();
  });

  api("/whoami").then(function (data) {
    document.getElementById("who").textContent = data.operator || "(no sign-in header)";
    var admission = document.getElementById("admission");
    admission.textContent = data.admission === "auto"
      ? "Admission: auto. Every DID that proves its key joins any room; nothing waits for approval."
      : "Admission: allowlist. A room's first DID is its admin. Other DIDs wait until the room admin or you admit them: they show under “Waiting”.";
    admission.hidden = false;
    if (data.instance) {
      document.getElementById("instance").textContent = data.instance;
      document.title = "Link server admission: " + data.instance;
    }
  }).catch(function () {});
  loadRooms().catch(function (e) { showError(e.message); });
})();
`;
