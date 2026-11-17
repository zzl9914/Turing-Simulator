-- 一进制后继：在 1^k 的末尾再写一个 1。空带得到 1。
blank _
start q0
accept qh
reject qr

q0 1 -> q0 1 R
q0 _ -> qh 1 R
