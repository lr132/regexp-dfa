/* main.js — GHC WASM glue + UI logic */

let hs = null;        // Haskell WASM exports
let vizInstance = null;
let lastDot = "";
let lastRegexp = "";

// ── DOM refs ─────────────────────────────────────────────────────────────────
const regexInput   = document.getElementById("regex-input");
const compileBtn   = document.getElementById("compile-btn");
const statusEl     = document.getElementById("status");
const resultsEl    = document.getElementById("results");
const loadingEl    = document.getElementById("loading");
const testInput    = document.getElementById("test-input");
const testBtn      = document.getElementById("test-btn");
const testResult   = document.getElementById("test-result");
const exportBtn    = document.getElementById("export-btn");
const copyDotBtn   = document.getElementById("copy-dot-btn");
const graphContainer = document.getElementById("graph-container");
const tablePre     = document.getElementById("table-pre");
const dotPre       = document.getElementById("dot-pre");

// ── Init ─────────────────────────────────────────────────────────────────────
async function init() {
  loadingEl.classList.remove("hidden");
  compileBtn.disabled = true;

  try {
    const { default: makeJsffiImports } = await import("./ghc_wasm_jsffi.js");
    const exports_ref = {};
    const jsffiImports = makeJsffiImports(exports_ref);

    const wasi = {
      proc_exit:            (code) => { throw new Error("proc_exit(" + code + ")"); },
      fd_write:             (fd, iovs, iovs_len, nwritten) => {
        const mem = new DataView(instance.exports.memory.buffer);
        let written = 0;
        for (let i = 0; i < iovs_len; i++) {
          const base = mem.getUint32(iovs + i * 8,     true);
          const len  = mem.getUint32(iovs + i * 8 + 4, true);
          const bytes = new Uint8Array(instance.exports.memory.buffer, base, len);
          console.log(new TextDecoder().decode(bytes));
          written += len;
        }
        mem.setUint32(nwritten, written, true);
        return 0;
      },
      fd_read:              () => 8,
      fd_close:             () => 0,
      fd_seek:              () => 0,
      fd_fdstat_get:        () => 0,
      fd_fdstat_set_flags:  () => 0,
      environ_get:          () => 0,
      environ_sizes_get:    (count_ptr, buf_size_ptr) => {
        const mem = new DataView(instance.exports.memory.buffer);
        mem.setUint32(count_ptr,    0, true);
        mem.setUint32(buf_size_ptr, 0, true);
        return 0;
      },
      args_get:             () => 0,
      args_sizes_get:       (argc_ptr, argv_buf_size_ptr) => {
        const mem = new DataView(instance.exports.memory.buffer);
        mem.setUint32(argc_ptr,          0, true);
        mem.setUint32(argv_buf_size_ptr, 0, true);
        return 0;
      },
      clock_time_get:       (id, precision, time_ptr) => {
        const mem = new DataView(instance.exports.memory.buffer);
        mem.setBigUint64(time_ptr, BigInt(Date.now()) * 1_000_000n, true);
        return 0;
      },
      random_get:           (buf, buf_len) => {
        crypto.getRandomValues(new Uint8Array(instance.exports.memory.buffer, buf, buf_len));
        return 0;
      },
      fd_prestat_get:       () => 8,
      fd_prestat_dir_name:  () => 8,
      fd_filestat_get:      () => 8,
      fd_filestat_set_size: () => 8,
      fd_filestat_set_times: () => 0,
      fd_advise:            () => 0,
      fd_allocate:          () => 8,
      fd_datasync:          () => 0,
      fd_sync:              () => 0,
      fd_tell:              () => 8,
      fd_readdir:           () => 8,
      fd_renumber:          () => 8,
      path_open:            () => 8,
      path_filestat_get:    () => 8,
      path_create_directory: () => 8,
      path_remove_directory: () => 8,
      path_unlink_file:     () => 8,
      path_rename:          () => 8,
      path_readlink:        () => 8,
      path_symlink:         () => 8,
      path_link:            () => 8,
      sock_accept:          () => 8,
      sock_recv:            () => 8,
      sock_send:            () => 8,
      sock_shutdown:        () => 0,
      sched_yield:          () => 0,
      poll_oneoff:          () => 52,
    };

    let instance;
    ({ instance } = await WebAssembly.instantiateStreaming(
      fetch("./main.wasm"),
      { ghc_wasm_jsffi: jsffiImports, wasi_snapshot_preview1: wasi }
    ));
    Object.assign(exports_ref, instance.exports);
    instance.exports.hs_init();
    hs = instance.exports;

    vizInstance = await Viz.instance();
  } catch (e) {
    showStatus("Failed to load WASM runtime: " + e.message, "error");
    loadingEl.classList.add("hidden");
    return;
  }

  loadingEl.classList.add("hidden");
  compileBtn.disabled = false;
  regexInput.focus();
}

// ── Compile ───────────────────────────────────────────────────────────────────
compileBtn.addEventListener("click", doCompile);
regexInput.addEventListener("keydown", (e) => { if (e.key === "Enter") doCompile(); });

function doCompile() {
  if (!hs) { showStatus("Runtime not ready yet.", "error"); return; }
  const input = regexInput.value.trim();
  if (!input) return;

  const raw = hs.hs_compile(input);
  const data = JSON.parse(raw);

  if (data.error) {
    showStatus("Error: " + data.error, "error");
    resultsEl.classList.add("hidden");
    return;
  }

  hideStatus();
  lastDot    = data.dot;
  lastRegexp = data.regexp;

  // Meta row
  document.getElementById("meta-states").innerHTML =
    `<strong>${data.states}</strong> states`;
  document.getElementById("meta-alpha").innerHTML =
    `Alphabet: <strong>{${data.alphabet.split("").join(", ")}}</strong>`;
  document.getElementById("meta-accept").innerHTML =
    `Accepting: <strong>{${data.accepting.join(", ")}}</strong>`;

  // Examples
  const exRow = document.getElementById("examples-row");
  if (data.examples.length === 0) {
    exRow.textContent = "No matches up to length 8.";
  } else {
    exRow.innerHTML = "Examples: " +
      data.examples.map(s => `<code>${s === "" ? "ε" : s}</code>`).join(", ");
  }

  // Table
  tablePre.textContent = data.table;

  // DOT source
  dotPre.textContent = data.dot;

  // Graph
  renderGraph(data.dot);

  resultsEl.classList.remove("hidden");
  showTab("graph");
}

function renderGraph(dot) {
  try {
    const svg = vizInstance.renderSVGElement(dot);
    // Style the SVG for dark mode
    svg.querySelectorAll("text").forEach(t => {
      t.setAttribute("fill", "#e2e8f0");
    });
    svg.querySelectorAll("path, polygon, ellipse").forEach(el => {
      const stroke = el.getAttribute("stroke");
      if (stroke && stroke !== "none") el.setAttribute("stroke", "#8892a4");
      const fill = el.getAttribute("fill");
      if (fill === "black") el.setAttribute("fill", "#e2e8f0");
      if (fill === "none")  el.setAttribute("fill", "none");
    });
    svg.style.background = "transparent";
    graphContainer.innerHTML = "";
    graphContainer.appendChild(svg);
  } catch (e) {
    graphContainer.textContent = "Graph render failed: " + e.message;
  }
}

// ── Tabs ─────────────────────────────────────────────────────────────────────
document.querySelectorAll(".tab-btn").forEach(btn => {
  btn.addEventListener("click", () => showTab(btn.dataset.tab));
});

function showTab(name) {
  document.querySelectorAll(".tab-btn").forEach(b => {
    b.classList.toggle("active", b.dataset.tab === name);
  });
  document.querySelectorAll(".tab-panel").forEach(p => {
    p.classList.toggle("hidden", p.id !== "tab-" + name);
  });
}

// ── Test string ───────────────────────────────────────────────────────────────
testBtn.addEventListener("click", doTest);
testInput.addEventListener("keydown", (e) => { if (e.key === "Enter") doTest(); });

function doTest() {
  if (!hs) return;
  const str   = testInput.value;
  const regex = regexInput.value.trim();
  if (!regex) return;

  const raw  = hs.hs_test(str, regex);
  const data = JSON.parse(raw);

  testResult.classList.remove("hidden", "match", "no-match");
  if (data.error) {
    testResult.textContent = "Error: " + data.error;
    testResult.classList.add("no-match");
  } else if (data.match) {
    testResult.textContent = `"${str}" matches.`;
    testResult.classList.add("match");
  } else {
    testResult.textContent = `"${str}" does not match.`;
    testResult.classList.add("no-match");
  }
}

// ── Copy DOT ─────────────────────────────────────────────────────────────────
copyDotBtn.addEventListener("click", () => {
  navigator.clipboard.writeText(lastDot).then(() => {
    copyDotBtn.textContent = "Copied!";
    setTimeout(() => { copyDotBtn.textContent = "Copy DOT"; }, 1500);
  });
});

// ── Export .df ───────────────────────────────────────────────────────────────
exportBtn.addEventListener("click", () => {
  if (!lastDot) return;
  // Reconstruct a .df file: just embed the dot source with a header
  const content = buildDfContent();
  const blob = new Blob([content], { type: "text/plain" });
  const url  = URL.createObjectURL(blob);
  const a    = document.createElement("a");
  a.href     = url;
  a.download = "dfa.df";
  a.click();
  URL.revokeObjectURL(url);
});

function buildDfContent() {
  // Mirrors the format from DFFile.hs: parse the ASCII table back from tablePre
  // Simplest approach: include regexp + DOT (self-contained)
  return `regexp: ${lastRegexp}\n${tablePre.textContent}`;
}

// ── Helpers ───────────────────────────────────────────────────────────────────
function showStatus(msg, type) {
  statusEl.textContent = msg;
  statusEl.className   = "status " + type;
}

function hideStatus() {
  statusEl.className = "status hidden";
}

// ── Boot ─────────────────────────────────────────────────────────────────────
init();
