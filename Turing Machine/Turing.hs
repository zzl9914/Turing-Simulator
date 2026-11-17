-- 确定性单带图灵机
--
--   M = (Q, Σ, Γ, δ, q0, q_acc, q_rej)
--
--   Q 有限；q0, q_acc, q_rej ∈ Q，且 q_acc ≠ q_rej。
--   Σ ⊆ Γ 有限，空白符 b ∈ Γ ∖ Σ。
--   δ : (Q ∖ {q_acc, q_rej}) × Γ → Q × Γ × {L, R}
--   是全函数，由互不冲突的五元组 (q, a, q', c, D) 给出：
--   δ(q, a) = (q', c, D)。
--
--   带是 τ : ℤ → Γ，且 { n ∈ ℤ | τ(n) ≠ b } 有限。
--   格局是 (q, τ, h)，h ∈ ℤ。
--   若 δ(q, τ(h)) = (q', c, D)，则 τ' 只在 h 处改为 c，
--   h' = h − 1（D = L）或 h' = h + 1（D = R），
--   并记 (q, τ, h) ⊢ (q', τ', h')。
--   进入 q_acc 为接受停机，进入 q_rej 为拒绝停机。
--
--   输入 w = w_0 … w_{n-1} 的初始格局：状态 q0，h = 0，
--   τ(i) = w_i（0 ≤ i < n），其余格点为 b。空字则 τ 恒为 b。
--
-- 程序与运行见 Universal.hs。说明见仓库根目录 README.md。

{-# LANGUAGE BangPatterns #-}

module Turing where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- 动作集 {L, R}。L 把读写头减 1，R 加 1。
data Dir = L | R
  deriving (Eq, Ord, Show)

-- δ 的一条指令：(q, a, q', c, D) 表示 δ(q, a) = (q', c, D)。
type Quintuple q s = (q, s, q, s, Dir)

-- τ 的有限支集。格点不在 Map 中则 τ(n) = b，且 Map 中不存放 b。
data Tape s = Tape
  { tapeBlank :: s
  , tapeCells :: Map Integer s
  } deriving (Eq)

-- 格局 (q, τ, h)。
data Config q s = Config
  { cfgState :: q
  , cfgTape  :: Tape s
  , cfgHead  :: Integer
  } deriving (Eq)

data Halt = Acc | Rej
  deriving (Eq, Show)

data Status = Accepted | Rejected | Diverged
  deriving (Eq, Show)

data Trace q s = Trace
  { traceStatus  :: Status
  , traceConfigs :: [Config q s]
  , traceRules   :: [Quintuple q s]
  }

data TM q s = TM
  { tmSigma :: [s]
  , tmDelta :: Map (q, s) (q, s, Dir)
  , tmStart :: q
  , tmBlank :: s
  , tmAcc   :: q
  , tmRej   :: q
  }

data MachineSpec q s = MachineSpec
  { specQ      :: [q]
  , specSigma  :: [s]
  , specGamma  :: [s]
  , specDelta  :: [Quintuple q s]
  , specQ0     :: q
  , specBlank  :: s
  , specAccept :: q
  , specReject :: q
  }

readTape :: Ord s => Tape s -> Integer -> s
readTape (Tape b cells) i = Map.findWithDefault b i cells

writeTape :: Ord s => Tape s -> Integer -> s -> Tape s
writeTape (Tape b cells) i a
  | a == b    = Tape b (Map.delete i cells)
  | otherwise = Tape b (Map.insert i a cells)

moveHead :: Dir -> Integer -> Integer
moveHead L h = h - 1
moveHead R h = h + 1

haltVerdict :: Eq q => TM q s -> q -> Maybe Halt
haltVerdict m q
  | q == tmAcc m = Just Acc
  | q == tmRej m = Just Rej
  | otherwise    = Nothing

-- 非停机格局上的一步。调用前状态必须不属于 {q_acc, q_rej}。
transition :: (Ord q, Ord s) => TM q s -> Config q s -> (Config q s, Quintuple q s)
transition m (Config q t h) =
  case Map.lookup (q, a) (tmDelta m) of
    Just (q', c, d) ->
      (Config q' (writeTape t h c) (moveHead d h), (q, a, q', c, d))
    Nothing ->
      error "δ 在非停机状态上无定义"
  where
    a = readTape t h

-- 从 C0 出发，沿 ⊢ 最多走 fuel 步。fuel 只用来截断意外的无穷计算。
compute :: (Ord q, Ord s) => Int -> TM q s -> Config q s -> Trace q s
compute fuel m c0 = go 0 c0 [] []
  where
    go !n c cs rs =
      case haltVerdict m (cfgState c) of
        Just Acc -> done Accepted
        Just Rej -> done Rejected
        Nothing
          | n >= fuel -> done Diverged
          | otherwise ->
              let (c', instr) = transition m c
              in go (n + 1) c' (c : cs) (instr : rs)
      where
        done tag = Trace tag (reverse (c : cs)) (reverse rs)

rule :: q -> s -> q -> s -> Dir -> Quintuple q s
rule q a q' c d = (q, a, q', c, d)

hasDup :: Ord a => [a] -> Bool
hasDup xs = length xs /= Set.size (Set.fromList xs)

-- 构造前核对定义里的全部约束。合法才给出可运行的 M。
validate :: (Ord q, Ord s, Show q, Show s) => MachineSpec q s -> [String]
validate spec = concat
  [ bad "Q 为空" (null (specQ spec))
  , bad "Q 中有重复状态" (hasDup (specQ spec))
  , bad "Γ 为空" (null (specGamma spec))
  , bad "Γ 中有重复符号" (hasDup (specGamma spec))
  , bad "Σ 中有重复符号" (hasDup (specSigma spec))
  , bad "b 不属于 Γ" (specBlank spec `notElem` specGamma spec)
  , bad "b 属于 Σ" (specBlank spec `elem` specSigma spec)
  , bad "Σ 不是 Γ 的子集" (not (Set.fromList (specSigma spec) `Set.isSubsetOf` Set.fromList (specGamma spec)))
  , bad "q0 不属于 Q" (specQ0 spec `notElem` specQ spec)
  , bad "q_acc 不属于 Q" (specAccept spec `notElem` specQ spec)
  , bad "q_rej 不属于 Q" (specReject spec `notElem` specQ spec)
  , bad "q_acc 与 q_rej 相同" (specAccept spec == specReject spec)
  , targetErrors
  , domainErrors
  ]
  where
    bad msg True  = [msg]
    bad _   False = []

    qset = Set.fromList (specQ spec)
    gset = Set.fromList (specGamma spec)

    targetErrors = concat
      [ [ "第 " ++ show i ++ " 条五元组的目标状态不属于 Q"
        | q' `Set.notMember` qset ]
      ++ [ "第 " ++ show i ++ " 条五元组的写入符号不属于 Γ"
        | c `Set.notMember` gset ]
      | (i, (_, _, q', c, _)) <- zip [(1 :: Int) ..] (specDelta spec) ]

    domainErrors =
      let halt    = Set.fromList [specAccept spec, specReject spec]
          nonHalt = Set.difference qset halt
          needed  = Set.fromList [(q, a) | q <- Set.toList nonHalt, a <- specGamma spec]
          haveList = [(q, a) | (q, a, _, _, _) <- specDelta spec]
          have    = Set.fromList haveList
          missing = Set.difference needed have
          extra   = Set.difference have needed
      in  ["有重复的 (q, a)，δ 不是单值函数" | hasDup haveList]
          ++ ["下列 (q, a) 没有转移：" ++ show (Set.toList missing) | not (Set.null missing)]
          ++ ["下列 (q, a) 不在 δ 的定义域中：" ++ show (Set.toList extra) | not (Set.null extra)]

build :: (Ord q, Ord s, Show q, Show s) => MachineSpec q s -> TM q s
build spec =
  case validate spec of
    [] -> TM
      { tmSigma = specSigma spec
      , tmDelta = Map.fromList [((q, a), (q', c, d)) | (q, a, q', c, d) <- specDelta spec]
      , tmStart = specQ0 spec
      , tmBlank = specBlank spec
      , tmAcc   = specAccept spec
      , tmRej   = specReject spec
      }
    errs -> error ("图灵机不合法：\n" ++ unlines (map ("  " ++) errs))

initial :: Ord s => TM q s -> [s] -> Config q s
initial m w
  | all (`elem` tmSigma m) w =
      Config (tmStart m) (Tape (tmBlank m) (Map.fromList (zip [0 ..] w))) 0
  | otherwise = error "输入含有 Σ 之外的符号"
