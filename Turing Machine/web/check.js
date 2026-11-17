const fs = require("fs");
const path = require("path");
const T = require("./machine.js");

function assert(cond, msg) {
  if (!cond) {
    console.error("失败：" + msg);
    process.exitCode = 1;
  }
}

function readExample(name) {
  return fs.readFileSync(path.join(__dirname, "..", "examples", name), "utf8");
}

function mustParse(src) {
  const r = T.parseProgram(src);
  assert(r.ok, r.error || "解析失败");
  return r.program;
}

const suc = mustParse(readExample("successor.tm"));
const an = mustParse(readExample("anbn.tm"));
const partial = mustParse(readExample("partial.tm"));
assert(suc.rules.length === 2 && suc.filled === 0, "后继条数");
assert(an.rules.length === 20 && an.filled === 0, "anbn 条数");
assert(partial.filled === 2, "补全条数");

for (const k of [0, 3]) {
  const w = "1".repeat(k);
  const got = T.runUntilHalt(suc, w, 50);
  assert(got.status === "accept" && T.tapeWord(got.cfg) === "1".repeat(k + 1) && got.steps === k + 1, "后继 " + w);
}

function isAnBn(w) {
  let i = 0;
  while (i < w.length && w[i] === "0") i += 1;
  const zeros = i;
  while (i < w.length && w[i] === "1") i += 1;
  return i === w.length && zeros * 2 === w.length;
}

let layer = [""];
const samples = [""];
for (let len = 1; len <= 6; len++) {
  const next = [];
  for (const s of layer) next.push(s + "0", s + "1");
  layer = next;
  samples.push(...next);
}
for (const w of samples) {
  const got = T.runUntilHalt(an, w, 5000);
  const want = isAnBn(w) ? "accept" : "reject";
  assert(got.status === want, w + " => " + got.status);
  if (want === "accept") {
    const n = w.length / 2;
    assert(got.steps === 2 * n * n + 2 * n + 1, w + " 步数");
  }
}

const text = T.serialize(suc);
const written = T.runWriter(text);
assert(written.word === text && written.steps === text.length, "书写器");
assert(T.serialize(mustParse(written.word)) === text, "装入");
assert(T.runUntilHalt(partial, "1", 10).status === "accept", "补全接受");
assert(T.runUntilHalt(partial, "", 10).status === "reject", "补全拒绝");
assert(!T.parseProgram("blank _\nstart q0\naccept qh\nreject qr\nq0 _ -> qh _ R\nq0 _ -> qr _ L\n").ok, "重复转移");

if (!process.exitCode) console.log("引擎检查通过");
