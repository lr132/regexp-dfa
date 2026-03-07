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
    // Load GHC WASM runtime. The build outputs main.wasm + ghc_wasm_jsffi.js
    const { default: initGhc } = await import("./ghc_wasm_jsffi.js");
    const wasmModule = await initGhc({ module_or_path: "./main.wasm" });
    await wasmModule.exports.hs_init();
    hs = wasmModule.exports;

    // Initialise Viz.js
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
