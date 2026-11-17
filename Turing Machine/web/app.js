(function () {
  const T = window.Turing;
  const $ = (id) => document.getElementById(id);

  const app = {
    playing: false,
    timer: 0,
    history: [],
    index: 0,
    sourceSnapshot: "",
    inputSnapshot: "111",
    error: ""
  };

  const examples = {
    successor: { input: "111", file: "../examples/successor.tm" },
    anbn: { input: "0011", file: "../examples/anbn.tm" },
    partial: { input: "1", file: "../examples/partial.tm" }
  };

  $("ex-successor").addEventListener("click", () => useExample("successor"));
  $("ex-anbn").addEventListener("click", () => useExample("anbn"));
  $("ex-partial").addEventListener("click", () => useExample("partial"));
  $("load").addEventListener("click", load);
  $("back").addEventListener("click", () => { pause(); back(); });
  $("step").addEventListener("click", () => { pause(); stepForward(); render(); });
  $("play").addEventListener("click", togglePlay);
  $("reset").addEventListener("click", () => { pause(); app.index = 0; render(); });
  $("speed").addEventListener("input", () => {
    $("speed-label").textContent = $("speed").value + " ms";
  });
  $("source").addEventListener("input", renderChrome);
  $("input").addEventListener("input", renderChrome);
  document.addEventListener("keydown", onKey);

  useExample("successor");

  async function useExample(name) {
    const ex = examples[name];
    try {
      const res = await fetch(ex.file);
      if (!res.ok) throw new Error(String(res.status));
      $("source").value = await res.text();
      $("input").value = ex.input;
      load();
    } catch (err) {
      app.error = "读不到 " + ex.file + "。请在 Turing Machine 目录启动 python -m http.server。";
      render();
    }
  }

  function load() {
    pause();
    app.error = "";
    const src = $("source").value;
    const input = $("input").value;
    const parsed = T.parseProgram(src);
    if (!parsed.ok) {
      app.error = parsed.error;
      render();
      return;
    }
    const bad = T.checkInput(parsed.program, input);
    if (bad) {
      app.error = bad;
      render();
      return;
    }
    app.sourceSnapshot = src;
    app.inputSnapshot = input;
    app.history = [runFrame(parsed.program, input)];
    app.index = 0;
    render();
  }

  function runFrame(program, input) {
    const cfg = T.initialConfig(program, input);
    const st = T.statusOf(program, cfg);
    return {
      cfg: cfg,
      rule: null,
      halt: st === "run" ? null : st,
      program: program
    };
  }

  function back() {
    if (app.index > 0) app.index -= 1;
    render();
  }

  function stepForward() {
    if (!app.history.length) return false;
    if (app.index < app.history.length - 1) {
      app.index += 1;
      return app.history[app.index].halt == null;
    }
    const cur = app.history[app.index];
    if (cur.halt) return false;
    if (app.history.length > 4000) {
      app.error = "已超过 4000 步，停止。";
      return false;
    }
    const stepped = T.apply(cur.program, cur.cfg);
    const st = T.statusOf(cur.program, stepped.cfg);
    app.history.push({
      cfg: stepped.cfg,
      rule: stepped.rule,
      halt: st === "run" ? null : st,
      program: cur.program
    });
    app.index += 1;
    return st === "run";
  }

  function togglePlay() {
    if (app.playing) {
      pause();
      renderChrome();
      return;
    }
    const cur = current();
    if (!cur || (cur.halt && app.index === app.history.length - 1)) return;
    app.playing = true;
    renderChrome();
    app.timer = setTimeout(playLoop, delay());
  }

  function playLoop() {
    if (!app.playing) return;
    const more = stepForward();
    render();
    if (!more) {
      pause();
      renderChrome();
      return;
    }
    app.timer = setTimeout(playLoop, delay());
  }

  function pause() {
    app.playing = false;
    clearTimeout(app.timer);
  }

  function delay() {
    return Number($("speed").value) || 280;
  }

  function current() {
    return app.history[app.index] || null;
  }

  function onKey(event) {
    const tag = document.activeElement && document.activeElement.tagName;
    if (tag === "TEXTAREA" || tag === "INPUT") return;
    if (event.code === "Space") {
      event.preventDefault();
      pause();
      stepForward();
      render();
    } else if (event.key === "ArrowLeft") {
      event.preventDefault();
      pause();
      back();
    } else if (event.key === "ArrowRight") {
      event.preventDefault();
      pause();
      stepForward();
      render();
    }
  }

  function render() {
    renderChrome();
    renderProgram();
    renderMachine();
    renderTrace();
  }

  function renderChrome() {
    const cur = current();
    const dirty = $("source").value !== app.sourceSnapshot || $("input").value !== app.inputSnapshot;
    $("dirty").hidden = !dirty || !app.history.length;
    $("error").hidden = !app.error;
    $("error").textContent = app.error || "";
    $("step-readout").textContent = String(app.index);
    $("play").textContent = app.playing ? "暂停" : "运行";
    const atStart = app.index === 0;
    const atEnd = cur && app.index === app.history.length - 1 && cur.halt;
    $("back").disabled = atStart || !cur;
    $("step").disabled = !cur || !!atEnd;
    $("play").disabled = !cur || !!atEnd;
    $("speed-label").textContent = $("speed").value + " ms";
  }

  function renderProgram() {
    const cur = current();
    const body = $("rules");
    body.replaceChildren();
    if (!cur || !cur.program) {
      $("prog-meta").textContent = "";
      return;
    }
    const program = cur.program;
    $("prog-meta").textContent = "写出 " + program.rules.length + " 条，补全 "
      + program.filled + " 条。空白 " + see(program.blank, program.blank) + "。";
    const active = activeRule(cur, program);
    for (const rule of program.allRules) {
      const tr = document.createElement("tr");
      if (rule.filled) tr.classList.add("filled");
      if (active && sameRule(active, rule)) tr.classList.add("active");
      for (const text of [rule.q, see(program.blank, rule.a), rule.q2, see(program.blank, rule.c), rule.dir]) {
        const td = document.createElement("td");
        td.textContent = text;
        tr.append(td);
      }
      body.append(tr);
    }
    reveal($("rules-wrap"), body.querySelector("tr.active"));
  }

  function renderMachine() {
    const cur = current();
    if (!cur) {
      $("badge").textContent = "未装入";
      $("badge").className = "badge";
      $("state-name").textContent = "—";
      $("head-no").textContent = "—";
      $("tape").replaceChildren();
      $("idline").textContent = "";
      $("now").textContent = "";
      return;
    }
    const badge = $("badge");
    const kind = badgeKind(cur);
    badge.className = "badge " + kind.cls;
    badge.textContent = kind.text;
    $("state-name").textContent = cur.cfg.state;
    $("head-no").textContent = String(cur.cfg.head);
    drawTape(cur);
    $("idline").textContent = idLine(cur);
    $("now").textContent = nowLine(cur);
    reveal($("tape"), $("tape").querySelector(".cell.head"), true);
  }

  function drawTape(cur) {
    const cfg = cur.cfg;
    const bounds = span(cfg);
    const tape = $("tape");
    tape.replaceChildren();
    for (let i = bounds.lo; i <= bounds.hi; i++) {
      const slot = document.createElement("div");
      slot.className = "slot";
      const above = document.createElement("div");
      above.className = "above";
      const cell = document.createElement("div");
      cell.className = "cell";
      const idx = document.createElement("div");
      idx.className = "idx";
      cell.textContent = see(cfg.blank, T.readAt(cfg, i));
      idx.textContent = String(i);
      if (i === cfg.head) {
        cell.classList.add("head");
        above.textContent = cfg.state;
      }
      slot.append(above, cell, idx);
      tape.append(slot);
    }
  }

  function renderTrace() {
    const box = $("trace");
    box.replaceChildren();
    app.history.forEach((item, i) => {
      const btn = document.createElement("button");
      btn.type = "button";
      if (i === app.index) btn.classList.add("here");
      btn.textContent = traceLabel(item, i);
      btn.addEventListener("click", () => {
        pause();
        app.index = i;
        render();
      });
      box.append(btn);
    });
    reveal(box, box.querySelector(".here"));
    $("trace-count").textContent = app.history.length
      ? "共 " + (app.history.length - 1) + " 步可回放，正看着第 " + app.index + " 步"
      : "";
  }

  function traceLabel(item, i) {
    if (!item.rule) return i + "  初始，状态 " + item.cfg.state + "，h=" + item.cfg.head;
    return i + "  " + fmtRule(item.cfg.blank, item.rule);
  }

  function badgeKind(cur) {
    if (cur.halt === "accept") return { cls: "accept", text: "接受" };
    if (cur.halt === "reject") return { cls: "reject", text: "拒绝" };
    return { cls: "run", text: "运行" };
  }

  function activeRule(cur, program) {
    if (cur.halt) return cur.rule;
    const a = T.readAt(cur.cfg, cur.cfg.head);
    return program.delta.get(cur.cfg.state + "\u0001" + a) || null;
  }

  function sameRule(a, b) {
    return a.q === b.q && a.a === b.a && a.q2 === b.q2 && a.c === b.c && a.dir === b.dir;
  }

  function nowLine(cur) {
    const rule = activeRule(cur, cur.program);
    if (cur.halt) {
      if (!cur.rule) return "停机。";
      return "停机。上一步 " + fmtRule(cur.program.blank, cur.rule);
    }
    if (!rule) return "δ 没有这一格。";
    const prefix = rule.filled ? "即将执行补上的转移  " : "即将执行  ";
    return prefix + fmtRule(cur.program.blank, rule);
  }

  function fmtRule(blank, rule) {
    return "δ(" + rule.q + ", " + see(blank, rule.a) + ") = ("
      + rule.q2 + ", " + see(blank, rule.c) + ", " + rule.dir + ")";
  }

  function idLine(cur) {
    const cfg = cur.cfg;
    const bounds = span(cfg);
    let text = "h=" + cfg.head + "  ";
    for (let i = bounds.lo; i <= bounds.hi; i++) {
      const g = see(cfg.blank, T.readAt(cfg, i));
      if (i === cfg.head && i === bounds.lo) text += cfg.state + " " + g;
      else if (i === cfg.head) text += " " + cfg.state + " " + g;
      else text += g;
    }
    return text;
  }

  function span(cfg) {
    let lo = Math.min(0, cfg.head);
    let hi = Math.max(0, cfg.head);
    for (const k of cfg.cells.keys()) {
      lo = Math.min(lo, k);
      hi = Math.max(hi, k);
    }
    return { lo: lo, hi: hi };
  }

  function reveal(container, el, horizontal) {
    if (!container || !el) return;
    const c = container.getBoundingClientRect();
    const r = el.getBoundingClientRect();
    if (r.top < c.top) container.scrollTop -= c.top - r.top;
    else if (r.bottom > c.bottom) container.scrollTop += r.bottom - c.bottom;
    if (horizontal) {
      if (r.left < c.left) container.scrollLeft -= c.left - r.left;
      else if (r.right > c.right) container.scrollLeft += r.right - c.left;
    }
  }

  function see(blank, ch) {
    if (ch === blank || ch === "\0") return "⊔";
    if (ch === "\n") return "↵";
    if (ch === " ") return "␣";
    return ch;
  }
})();
