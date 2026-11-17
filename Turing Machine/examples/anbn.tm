-- 判定 { 0^n 1^n | n ≥ 0 }。没写出的 (状态, 符号) 会补成进入 q_rej。
blank _
start q1
accept q_acc
reject q_rej

q1 0 -> q2 x R
q1 1 -> q_rej 1 R
q1 x -> q_rej x R
q1 y -> q4 y R
q1 _ -> q_acc _ R

q2 0 -> q2 0 R
q2 1 -> q3 y L
q2 x -> q_rej x R
q2 y -> q2 y R
q2 _ -> q_rej _ R

q3 0 -> q3 0 L
q3 1 -> q_rej 1 L
q3 x -> q1 x R
q3 y -> q3 y L
q3 _ -> q_rej _ L

q4 0 -> q_rej 0 R
q4 1 -> q_rej 1 R
q4 x -> q_rej x R
q4 y -> q4 y R
q4 _ -> q_acc _ R
