# Abstract Mathematics

三份彼此独立的材料。

| 目录 | 内容 |
| --- | --- |
| `Algebraic Structure Core` | 带运算和等式的代数结构，写在 `Core.hs` |
| `Hyperboolean Core` | 多值逻辑与模糊集合，写在 `hyper_bool.hpp` |
| `Turing Machine` | 一台程序写在带上的通用图灵机，以及观看它运行的页面 |

## 代数结构

`Algebraic Structure Core/Core.hs` 用类型类 `T`、`T1`、`T2` 描述加法、乘法、对数幂和普通幂。`Bool`、`Integer`、`Double`，以及它们的列表和 `Maybe`，各有一个实例。文件后半是这些运算要满足的等式：交换、结合、分配、幂等、吸收，以及 Laver shelf 和指数律。等式是可执行的谓词，不是证明。

## 超布尔

`Hyperboolean Core/hyper_bool.hpp` 是一个头文件。`hbool` 把真值存在 `0 … max_value`，否定、蕴涵、析取随静态设置取 Kleene、Łukasiewicz、Gödel、Post 或朱-肖之一。合取用德摩根律。`for_all` / `exist` 在全部真值上取最小或最大。`hset` 是隶属度取 `hbool` 的模糊集。`formula_node` 是命题公式树。

## 图灵机

定义在 `Turing Machine/Turing.hs`。一台机器是

```text
M = (Q, Σ, Γ, δ, q0, q_acc, q_rej)
```

带是函数 τ : ℤ → Γ，只有有限个格子不是空白。格局是 (q, τ, h)。δ(q, τ(h)) = (q′, c, D) 时，把第 h 格改成 c，读写头按 L 或 R 移动一格。进入 q_acc 接受，进入 q_rej 拒绝。没写出的 (q, a) 在装入时补成「原样写下，向右，进入 q_rej」，所以运行时的 δ 是全函数。

具体计算不焊在这台机器里。程序是 `examples/` 里的文本，由 `Universal.hs` 读入并执行。

```text
blank _
start q0
accept qh
reject qr

q0 1 -> q0 1 R
q0 _ -> qh 1 R
```

一行一条。`--` 开头是注释。符号是一个字符，`_` 在这里就是空白。`examples/successor.tm` 在一进制数末尾加一。`examples/anbn.tm` 判定 { 0^n 1^n | n ≥ 0 }。`examples/partial.tm` 故意少写转移，用来看补全。

在 `Turing Machine` 目录：

```text
runghc Universal.hs
runghc Universal.hs examples/successor.tm 111
runghc Universal.hs examples/anbn.tm 0011
```

无参数时直接执行示例，并在末尾印出 `examples/successor.tm` 的源文。改这个文件再运行即可。

页面在 `Turing Machine/web`。双击 `Turing Machine/启动.bat` 会启动服务并打开 http://127.0.0.1:8765/web/ 。关掉那个黑窗口，服务就停了。也可以在 `Turing Machine` 目录手动执行 `python -m http.server 8765`。上面是转移表、状态和带子，当前转移会高亮。可以单步、连续运行，也可以点过程列表回到某一步。页面下方是可修改的源代码，改完按装入。

`web/machine.js` 是同一套语义的浏览器实现。在 `web` 目录执行 `node check.js` 可以核对它和示例程序。
