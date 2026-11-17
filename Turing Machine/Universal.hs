-- 可编程、并且能把自己的程序写出来再执行的图灵机。
--
-- 通用机 U 的硬件固定。程序 Π 是数据，不是焊死的 δ。
-- U 的一步：在 Π 中查找 (q, τ(h))，写入新符号，读写头移动一格，状态改成 q'。
-- 缺转移的 (q, a) 在装入时补成进入 q_rej，因此装入后的 δ 仍是全函数。
--
-- 自编程机 S 的格局分成两个阶段。
--   Write：跑书写器 W。W 是一台普通单带图灵机，每个非停机状态对应正文里的一个字符，
--          读到空白就写下该字符并右移。字符烤在 W 的五元组里。
--   Load ：W 进入接受状态后，把带上的有限字解析成 Π。这一步是带上的字的可计算函数。
--   Run  ：此后每一步就是 U 的一步，Π 用的是刚写出来的那一份。
--
-- 程序文法（一行一条）：
--   blank _
--   start q0
--   accept qh
--   reject qr
--   q0 1 -> q0 1 R
--   -- 注释
--
-- 运行（在 Turing Machine 目录）：
--   runghc Universal.hs
--   runghc Universal.hs examples/successor.tm 111
--   runghc Universal.hs examples/anbn.tm 0011
-- 页面：python -m http.server 8765
--   打开 http://127.0.0.1:8765/web/

{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Exception (IOException, evaluate, try)
import Data.Char (isSpace)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.IO.Encoding (setLocaleEncoding)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hSetEncoding, stderr, stdout, utf8)
import Turing
  ( Config (..)
  , Dir (..)
  , Halt (..)
  , MachineSpec (..)
  , Quintuple
  , Status (..)
  , TM (..)
  , Tape (..)
  , Trace (..)
  , build
  , compute
  , haltVerdict
  , initial
  , readTape
  , rule
  , transition
  )

-- 步数上界只用来截断意外的死循环。
fuelFor :: Int -> Int
fuelFor n = 20 * (n + 2) * (n + 2)

data Program = Program
  { progTM :: TM String Char
  , progBlank :: Char
  , progStart :: String
  , progAcc :: String
  , progRej :: String
  , progRules :: [Quintuple String Char]
  , progFilled :: Int
  }

data Draft = Draft
  { dBlank :: Maybe Char
  , dStart :: Maybe String
  , dAcc :: Maybe String
  , dRej :: Maybe String
  , dRules :: [Quintuple String Char]
  }

emptyDraft :: Draft
emptyDraft = Draft Nothing Nothing Nothing Nothing []

data Phase = PhWrite | PhRun
  deriving (Eq)

-- S 的格局。Write 阶段只用书写器的带；Run 阶段的程序来自那条带，不来自调用方留下来的字符串。
data Self = Self
  { selfPhase :: Phase
  , selfWTM :: TM Int Char
  , selfWCfg :: Config Int Char
  , selfRTM :: Maybe (TM String Char)
  , selfRCfg :: Maybe (Config String Char)
  , selfIn :: String
  , selfText :: String
  }

data SelfResult = SelfResult
  { srWritten :: String
  , srWriteSteps :: Int
  , srWriteRules :: [Quintuple Int Char]
  , srStatus :: Status
  , srRunSteps :: Int
  , srRunRules :: [Quintuple String Char]
  , srRunConfigs :: [Config String Char]
  , srFinal :: Config String Char
  }

data Advance
  = WriteOne Self (Quintuple Int Char)
  | Load Self
  | RunOne Self (Quintuple String Char)
  | Stop Status (Config String Char)

normalize :: String -> String
normalize s = filter (/= '\r') (case s of
  '\xfeff' : rest -> rest
  other -> other)

trim :: String -> String
trim s =
  let dropped = dropWhile isSpace s
  in reverse (dropWhile isSpace (reverse dropped))

parseProgram :: String -> Either String Program
parseProgram src = go (1 :: Int) emptyDraft (lines (normalize src))
  where
    go :: Int -> Draft -> [String] -> Either String Program
    go _ d [] = finish d
    go n d (raw : rest) =
      case parseOne (trim raw) d of
        Left e -> Left ("第 " ++ show n ++ " 行：" ++ e)
        Right d' -> go (n + 1) d' rest

parseOne :: String -> Draft -> Either String Draft
parseOne line d
  | null line || take 2 line == "--" = Right d
  | otherwise = case words line of
      ["blank", sy] -> do
        c <- symbolToken sy
        b <- once "blank" (dBlank d) c
        Right d { dBlank = b }
      ["start", q] -> do
        s <- once "start" (dStart d) q
        Right d { dStart = s }
      ["accept", q] -> do
        s <- once "accept" (dAcc d) q
        Right d { dAcc = s }
      ["reject", q] -> do
        s <- once "reject" (dRej d) q
        Right d { dRej = s }
      [q, sy, "->", q', sy', dir] -> do
        a <- symbolToken sy
        c <- symbolToken sy'
        mv <- parseDir dir
        Right d { dRules = dRules d ++ [(q, a, q', c, mv)] }
      _ -> Left ("无法识别：" ++ line)

symbolToken :: String -> Either String Char
symbolToken [c] = Right c
symbolToken s = Left ("符号必须是一个字符，收到 " ++ s)

parseDir :: String -> Either String Dir
parseDir "L" = Right L
parseDir "R" = Right R
parseDir s = Left ("方向只能是 L 或 R，收到 " ++ s)

once :: String -> Maybe a -> a -> Either String (Maybe a)
once _ Nothing v = Right (Just v)
once name (Just _) _ = Left (name ++ " 重复定义")

finish :: Draft -> Either String Program
finish d = do
  b <- need "缺少 blank" (dBlank d)
  st <- need "缺少 start" (dStart d)
  ac <- need "缺少 accept" (dAcc d)
  rj <- need "缺少 reject" (dRej d)
  if ac == rj
    then Left "accept 与 reject 必须是两个不同的状态"
    else Right ()
  let rules = dRules d
  if any (\(q, _, _, _, _) -> q == ac || q == rj) rules
    then Left "停机状态上不能写转移"
    else Right ()
  let keys = [(q, a) | (q, a, _, _, _) <- rules]
  if length keys /= Set.size (Set.fromList keys)
    then Left "同一个 (状态, 符号) 有两条转移"
    else Right ()
  let states = nub (st : ac : rj : concat [[q, q'] | (q, _, q', _, _) <- rules])
      syms = nub (b : concat [[a, c] | (_, a, _, c, _) <- rules])
      nonHalt = [q | q <- states, q /= ac, q /= rj]
      known = Set.fromList keys :: Set (String, Char)
      extra = [(q, a) | q <- nonHalt, a <- syms, Set.notMember (q, a) known]
      spec = MachineSpec
        { specQ = states
        , specSigma = filter (/= b) syms
        , specGamma = syms
        , specDelta = rules ++ [rule q a rj a R | (q, a) <- extra]
        , specQ0 = st
        , specBlank = b
        , specAccept = ac
        , specReject = rj
        }
  Right Program
    { progTM = build spec
    , progBlank = b
    , progStart = st
    , progAcc = ac
    , progRej = rj
    , progRules = rules
    , progFilled = length extra
    }

need :: String -> Maybe a -> Either String a
need msg Nothing = Left msg
need _ (Just x) = Right x

serialize :: Program -> String
serialize p = unlines $
  [ "blank " ++ [progBlank p]
  , "start " ++ progStart p
  , "accept " ++ progAcc p
  , "reject " ++ progRej p
  , ""
  ] ++ map showRule (progRules p)

showRule :: Quintuple String Char -> String
showRule (q, a, q', c, d) = unwords [q, [a], "->", q', [c], show d]

-- 正文的每个字符成为一个状态。δ(i, 空白) = (i+1, 该字符, R)。其余符号进入拒绝状态。
emitWriter :: String -> TM Int Char
emitWriter text
  | '\0' `elem` text = error "程序正文含有书写器的空白符 NUL"
  | otherwise =
      let n = length text
          acc = n
          rej = n + 1
          gamma = nub (text ++ "\0")
          rules =
            [ if a == '\0'
                then rule i '\0' (i + 1) ch R
                else rule i a rej a R
            | (i, ch) <- zip [0 ..] text
            , a <- gamma
            ]
      in build MachineSpec
           { specQ = [0 .. n + 1]
           , specSigma = filter (/= '\0') gamma
           , specGamma = gamma
           , specDelta = rules
           , specQ0 = 0
           , specBlank = '\0'
           , specAccept = acc
           , specReject = rej
           }

machineEq :: TM String Char -> TM String Char -> Bool
machineEq a b =
  tmStart a == tmStart b
  && tmAcc a == tmAcc b
  && tmRej a == tmRej b
  && tmBlank a == tmBlank b
  && tmDelta a == tmDelta b
  && Set.fromList (tmSigma a) == Set.fromList (tmSigma b)

runTrace :: Program -> String -> Int -> Either String (Trace String Char)
runTrace p w fuel
  | any (`notElem` tmSigma (progTM p)) w = Left ("输入符号不属于 Σ：" ++ w)
  | otherwise = Right (compute fuel (progTM p) (initial (progTM p) w))

tapeSpan :: Config q Char -> String
tapeSpan c =
  case Map.keys (tapeCells (cfgTape c)) of
    [] -> ""
    k : ks ->
      let (lo, hi) = foldl' (\(u, v) z -> (min u z, max v z)) (k, k) ks
      in [readTape (cfgTape c) i | i <- [lo .. hi]]

lastCfg :: Trace q s -> Config q s
lastCfg tr = case reverse (traceConfigs tr) of
  c : _ -> c
  [] -> error "空轨迹"

advance :: Self -> Either String Advance
advance s = case selfPhase s of
  PhWrite -> case haltVerdict (selfWTM s) (cfgState (selfWCfg s)) of
    Just Rej -> Left "书写器拒绝停机"
    Just Acc ->
      let text = tapeSpan (selfWCfg s)
      in case parseProgram text of
           Left e -> Left ("带上的字不是程序：" ++ e)
           Right p ->
             if any (`notElem` tmSigma (progTM p)) (selfIn s)
               then Left ("输入符号不属于 Σ：" ++ selfIn s)
               else Right (Load s
                 { selfPhase = PhRun
                 , selfRTM = Just (progTM p)
                 , selfRCfg = Just (initial (progTM p) (selfIn s))
                 , selfText = text
                 })
    Nothing ->
      let (c', quin) = transition (selfWTM s) (selfWCfg s)
      in Right (WriteOne (s { selfWCfg = c' }) quin)
  PhRun -> case (selfRTM s, selfRCfg s) of
    (Just tm, Just c) -> case haltVerdict tm (cfgState c) of
      Just Acc -> Right (Stop Accepted c)
      Just Rej -> Right (Stop Rejected c)
      Nothing ->
        let (c', quin) = transition tm c
        in Right (RunOne (s { selfRCfg = Just c' }) quin)
    _ -> Left "运行阶段缺少从带上装入的程序"

selfRun :: String -> String -> Int -> Either String SelfResult
selfRun src input fuel = case parseProgram src of
  Left e -> Left e
  Right p ->
    let wtm = emitWriter (serialize p)
        start = Self PhWrite wtm (initial wtm "") Nothing Nothing input ""
    in drive fuel 0 0 [] [] [] start

drive
  :: Int -> Int -> Int
  -> [Quintuple Int Char]
  -> [Quintuple String Char]
  -> [Config String Char]
  -> Self
  -> Either String SelfResult
drive fuel !wn !rn wrev rrev crev s
  | PhRun <- selfPhase s
  , Just tm <- selfRTM s
  , Just c <- selfRCfg s
  , Nothing <- haltVerdict tm (cfgState c)
  , rn >= fuel =
      Right (pack Diverged c wn rn wrev rrev crev s)
  | otherwise = case advance s of
      Left e -> Left e
      Right (Stop st c) -> Right (pack st c wn rn wrev rrev crev s)
      Right (WriteOne s' quin) ->
        drive fuel (wn + 1) rn (quin : wrev) rrev crev s'
      Right (Load s') -> case selfRCfg s' of
        Just c0 -> drive fuel wn rn wrev rrev (c0 : crev) s'
        Nothing -> Left "装入后没有格局"
      Right (RunOne s' quin) -> case selfRCfg s' of
        Just c' -> drive fuel wn (rn + 1) wrev (quin : rrev) (c' : crev) s'
        Nothing -> Left "运行步没有格局"

pack
  :: Status -> Config String Char -> Int -> Int
  -> [Quintuple Int Char]
  -> [Quintuple String Char]
  -> [Config String Char]
  -> Self
  -> SelfResult
pack st c wn rn wrev rrev crev s = SelfResult
  { srWritten = selfText s
  , srWriteSteps = wn
  , srWriteRules = reverse wrev
  , srStatus = st
  , srRunSteps = rn
  , srRunRules = reverse rrev
  , srRunConfigs = reverse crev
  , srFinal = c
  }

prettyW :: String -> String
prettyW "" = "ε"
prettyW w = w

statusText :: Status -> String
statusText Accepted = "接受"
statusText Rejected = "拒绝"
statusText Diverged = "未停机"

glyph :: Char -> Char -> Char
glyph b ch = if ch == b then '⊔' else ch

formatCfg :: Char -> Config String Char -> String
formatCfg b c =
  let h = cfgHead c
      t = cfgTape c
      ks = Map.keys (tapeCells t)
      (lo, hi) = case ks of
        [] -> (min h 0, max h 0)
        k : rest ->
          let (x, y) = foldl' (\(u, v) z -> (min u z, max v z)) (k, k) rest
          in (min h (min 0 x), max h (max 0 y))
      piece i
        | i == h && i == lo = cfgState c ++ " " ++ [glyph b (readTape t i)]
        | i == h = " " ++ cfgState c ++ " " ++ [glyph b (readTape t i)]
        | otherwise = [glyph b (readTape t i)]
  in "  h=" ++ show h ++ "  " ++ concatMap piece [lo .. hi]

fmtR :: Char -> Quintuple String Char -> String
fmtR b (q, a, q', c, d) =
  "δ(" ++ q ++ ", " ++ [glyph b a] ++ ") = ("
  ++ q' ++ ", " ++ [glyph b c] ++ ", " ++ show d ++ ")"

formatRun
  :: Char -> Status -> Config String Char
  -> [Config String Char] -> [Quintuple String Char] -> String
formatRun b st final cs rs = unlines (steps cs rs ++ [end])
  where
    n = length rs
    word =
      let raw = map (glyph b) (tapeSpan final)
      in if null raw then "（空）" else raw
    end = case st of
      Accepted -> "  停机：接受。共 " ++ show n ++ " 步。带 " ++ word ++ "。"
      Rejected -> "  停机：拒绝。共 " ++ show n ++ " 步。带 " ++ word ++ "。"
      Diverged -> "  已走 " ++ show n ++ " 步，仍未停机。"
    steps (c : rest) (r : rr) =
      formatCfg b c : ("         ⊢ " ++ fmtR b r) : steps rest rr
    steps [c] [] = [formatCfg b c]
    steps [] [] = ["  （空轨迹）"]
    steps _ _ = ["  （轨迹不一致）"]

readText :: FilePath -> IO (Either String String)
readText path = do
  r <- try (readFile path >>= evaluate . force . normalize) :: IO (Either IOException String)
  pure $ case r of
    Left e -> Left ("读不到 " ++ path ++ "（" ++ show e ++ "）。请在 Turing Machine 目录下运行。")
    Right s -> Right s
  where
    force s = length s `seq` s

usage :: String
usage = unlines
  [ "用法："
  , "  runghc Universal.hs"
  , "  runghc Universal.hs examples/successor.tm 111"
  , "  runghc Universal.hs examples/anbn.tm 0011"
  , "无参数时执行示例，并在末尾印出可修改的源代码。"
  , "页面要在本目录启动：python -m http.server 8765"
  , "  打开 http://127.0.0.1:8765/web/"
  ]

present :: String -> String -> IO ()
present successorSrc anbnSrc = do
  showLoaded "examples/successor.tm" successorSrc "111"
  showLoaded "examples/anbn.tm" anbnSrc "01"
  putStrLn "可修改的源代码（examples/successor.tm）："
  putStrLn "-----"
  putStr successorSrc
  putStrLn "-----"

showLoaded :: String -> String -> String -> IO ()
showLoaded name src w = case parseProgram src of
  Left e -> putStrLn e >> exitFailure
  Right p -> case runTrace p w (fuelFor (length w)) of
    Left e -> putStrLn e >> exitFailure
    Right tr -> do
      putStrLn ("—— 同一台 U，程序换成 " ++ name ++ "，输入 " ++ prettyW w ++ " ——")
      putStr (formatRun (progBlank p) (traceStatus tr) (lastCfg tr) (traceConfigs tr) (traceRules tr))
      putStrLn ""

check :: String -> String -> String -> [String]
check successorSrc anbnSrc partialSrc =
  case (parseProgram successorSrc, parseProgram anbnSrc, parseProgram partialSrc) of
    (Left e, _, _) -> ["后继：" ++ e]
    (_, Left e, _) -> ["anbn：" ++ e]
    (_, _, Left e) -> ["补全：" ++ e]
    (Right suc, Right an, Right part) -> concat
      [ countRules "后继" suc 2 0
      , countRules "anbn" an 20 0
      , countRules "补全" part 1 2
      , expectTape suc "" "1" 1
      , expectTape suc "111" "1111" 4
      , expectStatus an "" Accepted
      , expectStatus an "01" Accepted
      , expectStatus an "001" Rejected
      , expectTape an "0011" "xxyy" 13
      , expectTape an (replicate 10 '0' ++ replicate 10 '1') (replicate 10 'x' ++ replicate 10 'y') 221
      , expectStatus an (replicate 10 '0' ++ replicate 9 '1') Rejected
      , expectTape part "1" "x" 1
      , expectStatus part "" Rejected
      , roundtrip "后继" successorSrc
      , selfAgree "后继 111" successorSrc "111" (fuelFor 3) Accepted
      , selfAgree "anbn 0011" anbnSrc "0011" (fuelFor 4) Accepted
      , diverges
      , expectLeft "重复转移" dupSrc
      , expectLeft "缺少 blank" "start q0\naccept qh\nreject qr\n"
      ]

countRules :: String -> Program -> Int -> Int -> [String]
countRules name p rules filled =
  [ name ++ "：写出 " ++ show (length (progRules p)) ++ " 条，补全 " ++ show (progFilled p)
  | length (progRules p) /= rules || progFilled p /= filled ]

expectTape :: Program -> String -> String -> Int -> [String]
expectTape p w want steps = case runTrace p w (fuelFor (length w)) of
  Right tr
    | traceStatus tr == Accepted
    , tapeSpan (lastCfg tr) == want
    , length (traceRules tr) == steps -> []
  Right tr -> [prettyW w ++ "：得到 " ++ tapeSpan (lastCfg tr) ++ "，" ++ show (length (traceRules tr)) ++ " 步"]
  Left e -> [prettyW w ++ "：" ++ e]

expectStatus :: Program -> String -> Status -> [String]
expectStatus p w want = case runTrace p w (fuelFor (length w)) of
  Right tr | traceStatus tr == want -> []
  Right tr -> [prettyW w ++ "：结果是" ++ statusText (traceStatus tr)]
  Left e -> [prettyW w ++ "：" ++ e]

roundtrip :: String -> String -> [String]
roundtrip name src = case parseProgram src of
  Left e -> [name ++ "：" ++ e]
  Right p -> case parseProgram (serialize p) of
    Left e -> [name ++ "：规范化后再解析失败：" ++ e]
    Right q ->
      [ name ++ "：规范化改变了 δ" | not (machineEq (progTM p) (progTM q)) ]
      ++ [ name ++ "：规范化不是不动点" | serialize q /= serialize p ]

selfAgree :: String -> String -> String -> Int -> Status -> [String]
selfAgree label src w fuel expect =
  case (parseProgram src, selfRun src w fuel) of
    (Left e, _) -> [label ++ "：" ++ e]
    (_, Left e) -> [label ++ "：" ++ e]
    (Right p, Right s) -> case runTrace p w fuel of
      Left e -> [label ++ "：" ++ e]
      Right tr ->
        [ label ++ "：自编程的轨迹与直接把程序装入 U 不一致"
        | srStatus s /= traceStatus tr
          || srRunSteps s /= length (traceRules tr)
          || srRunRules s /= traceRules tr
          || srRunConfigs s /= traceConfigs tr
          || srFinal s /= lastCfg tr ]
        ++ [ label ++ "：结果是" ++ statusText (srStatus s) ++ "，期望" ++ statusText expect
           | srStatus s /= expect ]
        ++ [ label ++ "：写下的不是规范化程序" | srWritten s /= serialize p ]
        ++ [ label ++ "：书写步数与正文长度不同" | srWriteSteps s /= length (srWritten s) ]

loopSrc :: String
loopSrc = unlines
  [ "blank _"
  , "start q0"
  , "accept qh"
  , "reject qr"
  , "q0 _ -> q0 _ R"
  ]

dupSrc :: String
dupSrc = unlines
  [ "blank _"
  , "start q0"
  , "accept qh"
  , "reject qr"
  , "q0 _ -> qh _ R"
  , "q0 _ -> qr _ L"
  ]

diverges :: [String]
diverges = case parseProgram loopSrc of
  Left e -> [e]
  Right p -> case runTrace p "" 25 of
    Right tr | traceStatus tr == Diverged -> []
    _ -> ["死循环应在步数上界处停下"]

expectLeft :: String -> String -> [String]
expectLeft name src = case parseProgram src of
  Left _ -> []
  Right _ -> [name ++ " 本应解析失败"]

runUser :: FilePath -> String -> IO ()
runUser path w = do
  srcR <- readText path
  case srcR of
    Left e -> putStrLn e >> exitFailure
    Right src -> case parseProgram src of
      Left e -> putStrLn e >> exitFailure
      Right p -> do
        let fuel = max 20000 (fuelFor (length w))
        putStrLn ("程序 " ++ path ++ "，输入 " ++ prettyW w ++ "。")
        putStrLn ("写出的转移 " ++ show (length (progRules p))
          ++ " 条，补成拒绝 " ++ show (progFilled p) ++ " 条。")
        case runTrace p w fuel of
          Left e -> putStrLn e >> exitFailure
          Right tr -> do
            let n = length (traceRules tr)
                word = map (glyph (progBlank p)) (tapeSpan (lastCfg tr))
            if n <= 40
              then putStr (formatRun (progBlank p) (traceStatus tr) (lastCfg tr)
                     (traceConfigs tr) (traceRules tr))
              else putStrLn ("共 " ++ show n ++ " 步，" ++ statusText (traceStatus tr)
                     ++ "。带 " ++ (if null word then "（空）" else word) ++ "。轨迹较长，已省略。")

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  case args of
    [] -> do
      files <- mapM readText
        ["examples/successor.tm", "examples/anbn.tm", "examples/partial.tm"]
      case files of
        [Right suc, Right anb, Right part] -> do
          present suc anb
          let probs = check suc anb part
          if null probs
            then putStrLn "检查通过。" >> putStrLn "" >> putStr usage
            else do
              putStrLn "检查未通过："
              mapM_ (putStrLn . ("  " ++)) probs
              exitFailure
        _ -> do
          mapM_ (either putStrLn (const (pure ()))) files
          exitFailure
    [path] -> runUser path ""
    [path, w] -> runUser path w
    _ -> putStrLn usage >> exitFailure
