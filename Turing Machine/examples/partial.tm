-- 只写了一条转移。读到空白时，装入器补成进入 qr。
blank _
start q0
accept qh
reject qr

q0 1 -> qh x R
