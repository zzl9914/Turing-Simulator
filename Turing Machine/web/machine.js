// 与 Universal.hs 同一套语义：程序是数据，缺的转移补成进入 q_rej。
(function (root, factory) {
  const api = factory();
  if (typeof module === "object" && module.exports) module.exports = api;
  root.Turing = api;
})(typeof globalThis !== "undefined" ? globalThis : this, function () {
  function normalize(src) {
    return String(src).replace(/^\uFEFF/, "").replace(/\r/g, "");
  }

  function trim(s) {
    return s.replace(/^\s+/, "").replace(/\s+$/, "");
  }

  function uniq(xs) {
    const seen = new Set();
    const out = [];
    for (const x of xs) {
      if (!seen.has(x)) {
        seen.add(x);
        out.push(x);
      }
    }
    return out;
  }

  function fail(line, msg) {
    throw new Error(line ? "第 " + line + " 行：" + msg : msg);
  }

  function symbolToken(tok) {
    if (tok.length !== 1) throw new Error("符号必须是一个字符，收到 " + tok);
    return tok;
  }

  function parseProgram(src) {
    try {
      return { ok: true, program: parse(src) };
    } catch (err) {
      return { ok: false, error: err.message };
    }
  }

  function parse(src) {
    const draft = { blank: null, start: null, accept: null, reject: null, rules: [] };
    const lines = normalize(src).split("\n");
    for (let i = 0; i < lines.length; i++) {
      const lineNo = i + 1;
      const line = trim(lines[i]);
      if (!line || line.startsWith("--")) continue;
      const parts = line.split(/\s+/);
      try {
        if (parts[0] === "blank" && parts.length === 2) {
          assignOnce(draft, "blank", symbolToken(parts[1]), "blank");
        } else if (parts[0] === "start" && parts.length === 2) {
          assignOnce(draft, "start", parts[1], "start");
        } else if (parts[0] === "accept" && parts.length === 2) {
          assignOnce(draft, "accept", parts[1], "accept");
        } else if (parts[0] === "reject" && parts.length === 2) {
          assignOnce(draft, "reject", parts[1], "reject");
        } else if (parts.length === 6 && parts[2] === "->") {
          const dir = parts[5];
          if (dir !== "L" && dir !== "R") throw new Error("方向只能是 L 或 R，收到 " + dir);
          draft.rules.push({
            q: parts[0],
            a: symbolToken(parts[1]),
            q2: parts[3],
            c: symbolToken(parts[4]),
            dir: dir,
            filled: false
          });
        } else {
          throw new Error("无法识别：" + line);
        }
      } catch (err) {
        fail(lineNo, err.message);
      }
    }
    if (draft.blank == null) fail(0, "缺少 blank");
    if (draft.start == null) fail(0, "缺少 start");
    if (draft.accept == null) fail(0, "缺少 accept");
    if (draft.reject == null) fail(0, "缺少 reject");
    if (draft.accept === draft.reject) fail(0, "accept 与 reject 必须是两个不同的状态");
    if (draft.rules.some((r) => r.q === draft.accept || r.q === draft.reject)) {
      fail(0, "停机状态上不能写转移");
    }
    const keys = draft.rules.map((r) => r.q + "\u0001" + r.a);
    if (new Set(keys).size !== keys.length) fail(0, "同一个 (状态, 符号) 有两条转移");

    const states = uniq([draft.start, draft.accept, draft.reject].concat(
      draft.rules.flatMap((r) => [r.q, r.q2])
    ));
    const syms = uniq([draft.blank].concat(draft.rules.flatMap((r) => [r.a, r.c])));
    const known = new Set(keys);
    const extra = [];
    for (const q of states) {
      if (q === draft.accept || q === draft.reject) continue;
      for (const a of syms) {
        if (!known.has(q + "\u0001" + a)) {
          extra.push({ q: q, a: a, q2: draft.reject, c: a, dir: "R", filled: true });
        }
      }
    }
    const allRules = draft.rules.concat(extra);
    const delta = new Map();
    for (const rule of allRules) delta.set(rule.q + "\u0001" + rule.a, rule);
    return {
      blank: draft.blank,
      start: draft.start,
      accept: draft.accept,
      reject: draft.reject,
      rules: draft.rules,
      allRules: allRules,
      delta: delta,
      sigma: new Set(syms.filter((s) => s !== draft.blank)),
      gamma: syms,
      filled: extra.length
    };
  }

  function assignOnce(draft, key, value, name) {
    if (draft[key] != null) throw new Error(name + " 重复定义");
    draft[key] = value;
  }

  function serialize(program) {
    const lines = [
      "blank " + program.blank,
      "start " + program.start,
      "accept " + program.accept,
      "reject " + program.reject,
      ""
    ].concat(program.rules.map((r) => r.q + " " + r.a + " -> " + r.q2 + " " + r.c + " " + r.dir));
    return lines.join("\n") + "\n";
  }

  function readAt(cfg, i) {
    return cfg.cells.has(i) ? cfg.cells.get(i) : cfg.blank;
  }

  function initialConfig(program, input) {
    const cells = new Map();
    for (let i = 0; i < input.length; i++) cells.set(i, input[i]);
    return { state: program.start, head: 0, cells: cells, blank: program.blank };
  }

  function checkInput(program, input) {
    for (const ch of input) {
      if (!program.sigma.has(ch)) return "输入符号不属于 Σ：" + input;
    }
    return null;
  }

  function statusOf(program, cfg) {
    if (cfg.state === program.accept) return "accept";
    if (cfg.state === program.reject) return "reject";
    return "run";
  }

  function apply(program, cfg) {
    const a = readAt(cfg, cfg.head);
    const rule = program.delta.get(cfg.state + "\u0001" + a);
    if (!rule) throw new Error("δ 在非停机状态上无定义");
    const cells = new Map(cfg.cells);
    if (rule.c === cfg.blank) cells.delete(cfg.head);
    else cells.set(cfg.head, rule.c);
    return {
      rule: rule,
      cfg: {
        state: rule.q2,
        head: cfg.head + (rule.dir === "L" ? -1 : 1),
        cells: cells,
        blank: cfg.blank
      }
    };
  }

  function runUntilHalt(program, input, limit) {
    const cap = limit == null ? 5000 : limit;
    const bad = checkInput(program, input);
    if (bad) return { status: "error", error: bad, steps: 0, cfg: null };
    let cfg = initialConfig(program, input);
    let steps = 0;
    while (steps < cap) {
      const st = statusOf(program, cfg);
      if (st !== "run") return { status: st, steps: steps, cfg: cfg };
      cfg = apply(program, cfg).cfg;
      steps += 1;
    }
    return { status: "diverged", steps: steps, cfg: cfg };
  }

  function tapeWord(cfg) {
    if (!cfg || cfg.cells.size === 0) return "";
    let lo = Infinity;
    let hi = -Infinity;
    for (const k of cfg.cells.keys()) {
      lo = Math.min(lo, k);
      hi = Math.max(hi, k);
    }
    let s = "";
    for (let i = lo; i <= hi; i++) s += readAt(cfg, i);
    return s;
  }

  function writtenSoFar(cfg) {
    const n = Number(cfg.state);
    let s = "";
    for (let i = 0; i < n; i++) s += cfg.cells.get(i);
    return s;
  }

  function applyWriter(item) {
    const i = Number(item.cfg.state);
    const ch = item.text[i];
    const cells = new Map(item.cfg.cells);
    cells.set(item.cfg.head, ch);
    return {
      rule: { q: String(i), a: "\0", q2: String(i + 1), c: ch, dir: "R", filled: false, writer: true },
      cfg: { state: String(i + 1), head: item.cfg.head + 1, cells: cells, blank: "\0" }
    };
  }

  function runWriter(text) {
    let cfg = { state: "0", head: 0, cells: new Map(), blank: "\0" };
    let steps = 0;
    while (Number(cfg.state) < text.length) {
      cfg = applyWriter({ cfg: cfg, text: text }).cfg;
      steps += 1;
    }
    return { steps: steps, cfg: cfg, word: writtenSoFar(cfg) };
  }

  return {
    parseProgram: parseProgram,
    serialize: serialize,
    initialConfig: initialConfig,
    checkInput: checkInput,
    statusOf: statusOf,
    apply: apply,
    readAt: readAt,
    runUntilHalt: runUntilHalt,
    tapeWord: tapeWord,
    writtenSoFar: writtenSoFar,
    applyWriter: applyWriter,
    runWriter: runWriter
  };
});
