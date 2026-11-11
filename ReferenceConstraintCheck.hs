module Main where

import Control.Monad (unless, forM_)
import Data.List (findIndex, foldl', nub, sort, transpose)
import Data.Ratio ((%), numerator, denominator)
import System.Exit (die)

-- ============================================================
-- 基本型
-- ============================================================

-- 次元ベクトル。
-- 各成分は、固定した基本次元に対する整数指数を表す。
-- このプログラムの具体例では、長さ・質量・時間・電流の順。
type Dim = [Integer]

-- 行列計算には厳密な有理数を用いる。
-- 浮動小数点による近似的な零判定や階数判定は行わない。
type Row = [Rational]
type Matrix = [Row]

-- ============================================================
-- 基準量・変数・式の登録情報
-- ============================================================

-- 基準量 B_i = b_i U_i の登録情報。
-- referenceScale は、固定した整合的単位系に対する数値 b_i。
-- 正規化値 x_i や変更倍率 lambda_i とは異なる。
data Reference = Reference
  { referenceName :: String
  , referenceDim :: Dim
  , referenceScale :: Rational
  } deriving (Eq, Show)

-- 入力変数と、その正規化に用いる基準量との対応。
--
-- Just i:
--   modelReferences の i 番目の基準量を使う。
--   添字は 0 始まり。
--
-- Nothing:
--   無次元変数であり、基準値を 1 に固定する。
--   「不明」「未登録」という意味ではない。
data Variable = Variable
  { variableName :: String
  , variableReference :: Maybe Int
  } deriving (Eq, Show)

-- 一つの単項式 c * product_j x_j^(beta_j)。
--
-- termPowers の並びは modelVariables の並びと対応する。
-- 整数指数を許すので、負指数を含む Laurent 単項式も表せる。
--
-- 例:
--   変数順が [vo, voff, gain, rs] のとき、
--   Term 1 [1,0,-1,-1] は vo / (gain * rs) を表す。
data Term = Term
  { termCoefficient :: Rational
  , termPowers :: [Integer]
  } deriving (Eq, Show)

-- 量の式と、入力・出力の基準量をまとめたモデル。
--
-- modelTerms:
--   正規化に伴う基準係数を掛ける前の式。
--
-- modelOutput:
--   Just i なら i 番目の基準量で出力を正規化する。
--   Nothing なら無次元出力であり、基準値は 1。
--
-- 基準量の数値は、一つの固定された整合的単位系に対して与える。
data Model = Model
  { modelName :: String
  , modelReferences :: [Reference]
  , modelVariables :: [Variable]
  , modelOutput :: Maybe Int
  , modelTerms :: [Term]
  } deriving (Eq, Show)

-- 基準量変更に対する検査対象。
--
-- factorExponents の各行が、基準係数
--   k_a = product_i b_i^(e_ai)
-- の指数を表す。
--
-- Model と異なり、入力変数や元の係数 c_beta は保持しない。
-- 検査したい無次元積だけを直接登録することもできる。
data Contract = Contract
  { contractName :: String
  , contractReferences :: [Reference]
  , factorExponents :: [[Integer]]
  } deriving (Eq, Show)

-- 基準量変更に対して要求する仕様。
--
-- PreserveFactorsOnly:
--   登録した基準係数が変更前後で保存されればよい。
--
-- CommonDimensionScaling:
--   各基本次元に共通の倍率を与える変更を要求する。
--   これは、選択した基準量間の全次元関係の保存に対応する。
data Policy = PreserveFactorsOnly | CommonDimensionScaling
  deriving (Eq, Show)

-- 基準量変更の検査結果。
data Audit = Audit
  { initialFactors :: [Rational]  -- 変更前の基準係数 k_a
  , factorMultipliers :: [Rational]
      -- 各係数の変更比 k_a' / k_a
  , changedFactors :: [Rational]  -- 変更後の基準係数 k_a'
  , initialOmissionAllowed :: Bool
      -- 変更前の基準係数が全て 1 か
  , changedOmissionAllowed :: Bool
      -- 変更後の基準係数が全て 1 か
  , factorPreserved :: Bool
      -- 登録した全基準係数が保存されるか
  , allDimensionRelationsPreserved :: Bool
      -- 全次元関係が保存されるか
  , allDimensionChecks :: [([Integer], Rational)]
      -- ker M を張る整数指数と、その倍率積
  , additionalChecks :: [([Integer], Rational)]
      -- 既存の係数検査を補う追加指数と、その倍率積
  , remainingDegrees :: Int
      -- 元の E に対する不足数 delta。
      -- 今回の倍率で不合格になった条件数ではない。
  , policySatisfied :: Bool
      -- 選択した仕様に対する最終判定
  } deriving (Eq, Show)

-- 候補式との係数比較で見つかった不一致。
data Mismatch = Mismatch
  { mismatchPowers :: [Integer]       -- 不一致の単項式指数
  , expectedCoefficient :: Rational  -- 必要な係数
  , suppliedCoefficient :: Rational  -- 候補式の係数
  } deriving (Eq, Show)

-- ============================================================
-- エラー処理と基準量の検証
-- ============================================================

-- 条件が真なら成功し、偽ならエラーメッセージを返す。
-- Either の do ブロックでは、Left が出た時点で処理を打ち切る。
ensure :: Bool -> String -> Either String ()
ensure p msg = if p then Right () else Left msg

-- 基準量の登録情報を検証し、次元ベクトルの長さを返す。
validateReferences :: [Reference] -> Either String Int
validateReferences [] =
  Left "No references"

validateReferences rs@(r : _) = do
  let d = length (referenceDim r)

  -- 次元座標を少なくとも一つ持つことを要求する。
  ensure (d > 0)
         "Empty dimension vector"

  -- 全基準量で同じ次元座標系を使う。
  ensure (all ((== d) . length . referenceDim) rs)
         "Dimension-vector length mismatch"

  -- 正の基準量だけを扱う。
  -- 負指数の計算と、正倍率に基づく保存条件の議論にも必要。
  ensure (all ((> 0) . referenceScale) rs)
         "Reference scales must be positive"

  -- 識別名の重複を拒否する。
  ensure (length (nub (map referenceName rs)) == length rs)
         "Duplicate reference IDs"

  pure d

-- ============================================================
-- 有理数による線形代数
-- ============================================================

-- 簡約行階段形とピボット列の一覧を求める。
--
-- n は列数。
-- 入力行列の各行が n 成分を持つことを前提とする。
-- この低水準関数自体では行長を検証しない。
rref :: Int -> Matrix -> (Matrix, [Int])
rref n input = go 0 0 input []
  where
    -- r: 次にピボットを置く行
    -- c: 調べる列
    -- a: 作業中の行列
    -- piv: 確定したピボット列
    go r c a piv
      | r >= length a || c >= n = (a, piv)
      | otherwise = case findIndex ((/= 0) . (!! c)) (drop r a) of

          -- 現在行以降に非零成分がない列は飛ばす。
          Nothing -> go r (c+1) a piv

          Just k ->
            let
                -- findIndex は drop r a 内の添字を返すため補正する。
                j = r+k

                -- 見つかった行を現在のピボット行へ移す。
                swapped =
                  [ if i == r then a !! j
                    else if i == j then a !! r
                    else row
                  | (i,row) <- zip [0..] a
                  ]

                -- ピボット成分を 1 にする。
                pivot =
                  map (/ ((swapped !! r) !! c)) (swapped !! r)

                -- ピボット行以外の全行から第 c 成分を消去する。
                -- 上側も下側も消去するので簡約行階段形となる。
                reduced =
                  [ if i == r then pivot
                    else zipWith (-) row
                           (map (* (row !! c)) pivot)
                  | (i,row) <- zip [0..] swapped
                  ]

            in go (r+1) (c+1) reduced (piv ++ [c])

-- 階数はピボット列の数。
rank :: Int -> Matrix -> Int
rank n = length . snd . rref n

-- A z = 0 の有理数核基底を求める。
--
-- 各基底ベクトルを、返り値の一つの行として格納する。
-- 行列が空なら、R^n の標準基底を返す。
nullspace :: Int -> Matrix -> Matrix
nullspace n a =
  let
      (r,piv) = rref n a

      -- ピボットでない列を自由変数とする。
      free = filter (`notElem` piv) [0..n-1]

      -- 自由変数 f を 1、他の自由変数を 0 にした解の第 j 成分。
      column f j = case lookup j (zip piv r) of
        Just row -> negate (row !! f)
        Nothing -> if j == f then 1 else 0

  in [ [column f j | j <- [0..n-1]] | f <- free ]

-- 有理数ベクトルを原始整数ベクトルへ直す。
--
-- 1. 分母の最小公倍数を掛けて整数化する。
-- 2. 全成分の最大公約数で割る。
-- 3. 最初の非零成分を正にそろえる。
--
-- ここで得るのは個々のベクトルの整数化であり、
-- 整数核全体の「整数格子の基底」を求める処理ではない。
primitive :: Row -> [Integer]
primitive row =
  let
      l = foldl' lcm 1 (map denominator row)
      ints = map (\x -> numerator (x * fromInteger l)) row
      g = foldl' gcd 0 (map abs ints)
      v = if g == 0 then ints else map (`div` g) ints
  in case dropWhile (== 0) v of
       (x:_) | x < 0 -> map negate v
       _ -> v

-- 各基準量の次元ベクトルを列に並べ、次元行列 M を作る。
-- M のサイズは「次元座標数 × 基準量数」。
dimensionMatrix :: [Reference] -> Matrix
dimensionMatrix =
  transpose . map (map fromInteger . referenceDim)

-- E の行空間を ker M まで拡張するための追加ベクトルを選ぶ。
--
-- 前提:
--   E の各行は ker M に属する。
--
-- 核基底の各ベクトルについて、追加すると階数が増える場合だけ採用。
-- 返すのは追加したベクトルだけであり、拡張行列全体ではない。
completion :: Int -> Matrix -> Matrix -> Matrix
completion n e m =
  snd (foldl' step (e, []) (nullspace n m))
  where
    step (rows, added) z
      | rank n (rows ++ [z]) > rank n rows =
          (rows ++ [z], added ++ [z])
      | otherwise =
          (rows, added)

-- ============================================================
-- 基準係数と基準量変更の検査
-- ============================================================

-- product_i scales_i^(exponents_i) を厳密に計算する。
--
-- scales に基準量の数値 b_i を渡すと基準係数、
-- 変更倍率 lambda_i を渡すと係数の変更比になる。
--
-- (^^) は整数指数を扱うため、負指数も使用できる。
-- リスト長の一致は呼出し側で保証する。
factorValue :: [Rational] -> [Integer] -> Rational
factorValue scales exponents =
  product (zipWith (^^) scales exponents)

-- 基準量変更を検査する中心関数。
--
-- 実核を張る有理数基底を整数化すれば十分であり、
-- 整数核の整数基底までは必要ない。
-- 正倍率のため、分母払いで「倍率積 = 1」の条件は変わらない。
--
-- 対数座標で議論できるが、実装では対数を近似計算せず、
-- 有理数の倍率積が厳密に 1 かを判定する。
audit :: Policy -> Contract -> [Rational] -> Either String Audit
audit policy contract lambdas = do
  d <- validateReferences rs

  let
      n = length rs
      eInt = factorExponents contract
      e = map (map fromInteger) eInt
      m = dimensionMatrix rs

  -- 各基準量に一つずつ正倍率を指定する。
  ensure (length lambdas == n && all (> 0) lambdas)
         "Invalid positive multipliers"

  -- 各係数指数行の成分数は基準量数に一致する。
  ensure (all ((== n) . length) eInt)
         "Exponent-vector length mismatch"

  -- 各係数が無次元積を表すことを検証する。
  -- 各行 z に対する M z = 0、全体では M E^T = 0。
  ensure
    (all
      (\z -> all (== 0)
        [sum (zipWith (*) row z) | row <- m])
      e)
    "A claimed reference factor has nonzero dimension"

  ensure (length m == d)
         "Internal dimension-matrix mismatch"

  let
      scales = map referenceScale rs

      -- 変更前の係数 k_a。
      old = map (factorValue scales) eInt

      -- 係数の変更比 k_a' / k_a。
      ratios = map (factorValue lambdas) eInt

      -- 変更後の係数 k_a'。
      new = zipWith (*) old ratios

      -- 全次元関係を張る整数指数と、それぞれの倍率積。
      full =
        [ (z, factorValue lambdas z)
        | z <- map primitive (nullspace n m)
        ]

      -- 既存の係数検査に追加すべき指数と、その倍率積。
      extra =
        [ (z, factorValue lambdas z)
        | z <- map primitive (completion n e m)
        ]

      -- 選択した基準係数が全て保存されるか。
      gf = all (== 1) ratios

      -- 全次元関係が保存されるか。
      gd = all ((== 1) . snd) full

      -- 元の係数検査に不足する独立な線形条件数。
      delta = n - rank n e - rank n m

      -- 係数保存仕様では gf のみ、
      -- 共通次元倍率仕様では gf と gd の両方を要求する。
      -- 登録条件 M E^T = 0 の下では gd は gf を含意する。
      ok = gf && (policy == PreserveFactorsOnly || gd)

  pure
    (Audit
      old ratios new
      (all (== 1) old)
      (all (== 1) new)
      gf gd full extra delta ok)

  where
    rs = contractReferences contract

-- ============================================================
-- 登録モデルの検証と基準係数の導出
-- ============================================================

-- 元の量の式が、登録情報の下で整合しているかを検証する。
-- 物理モデルそのものの妥当性を証明する処理ではない。
validateModel :: Model -> Either String ()
validateModel model = do
  d <- validateReferences rs

  let
      n = length rs
      valid Nothing = True
      valid (Just i) = i >= 0 && i < n

  -- 入力・出力が存在する基準量を参照しているか。
  ensure
    (all (valid . variableReference) vs
      && valid (modelOutput model))
    "Reference index out of range"

  ensure
    (length (nub (map variableName vs)) == length vs)
    "Duplicate variable names"

  ensure (not (null ts))
         "Empty model"

  -- 各項に、全入力変数分の指数が必要。
  ensure
    (all ((== length vs) . length . termPowers) ts)
    "Term arity mismatch"

  -- 元モデルは零項を除いた形で登録する。
  ensure
    (all ((/= 0) . termCoefficient) ts)
    "Zero term: combine like terms first"

  -- 元モデルは同類項をまとめた形で登録する。
  ensure
    (length (nub (map termPowers ts)) == length ts)
    "Duplicate monomials"

  let
      dimOf Nothing = replicate d 0
      dimOf (Just i) = referenceDim (rs !! i)

      outDim = dimOf (modelOutput model)

      -- 単項式の次元は、各変数の次元×指数の総和。
      dimTerm t =
        foldl' (zipWith (+)) (replicate d 0)
          [ map (* p) (dimOf (variableReference v))
          | (p,v) <- zip (termPowers t) vs
          ]

  -- 全項が出力と同じ次元を持つことを要求する。
  ensure
    (all ((== outDim) . dimTerm) ts)
    "Dimensionally inhomogeneous model"

  where
    rs = modelReferences model
    vs = modelVariables model
    ts = modelTerms model

-- モデルの各項から基準係数の指数行列 E を導出する。
contractFromModel :: Model -> Either String Contract
contractFromModel model = do
  validateModel model

  let
      rs = modelReferences model
      n = length rs

      -- 入力側で基準量 i を使う変数の指数を合計し、
      -- 出力基準量が i なら、出力で割る分の 1 を引く。
      --
      -- 同じ基準量を使う入力変数が複数あれば指数を合算する。
      exponent t i =
        sum
          [ p
          | (p,v) <-
              zip (termPowers t) (modelVariables model)
          , variableReference v == Just i
          ]
        - (if modelOutput model == Just i then 1 else 0)

      -- 各項に対応する指数行を生成する。
      -- 異なる項の基準係数が同じなら、重複行ができてもよい。
      rows =
        [ [exponent t i | i <- [0..n-1]]
        | t <- modelTerms model
        ]

  pure (Contract (modelName model) rs rows)

-- 各元係数 c_beta に基準係数 k_beta を掛け、
-- 正規化後の式の項を生成する。
normalizedTerms :: Model -> Either String [Term]
normalizedTerms model = do
  c <- contractFromModel model

  let
      factors =
        map
          (factorValue
            (map referenceScale (modelReferences model)))
          (factorExponents c)

  pure
    [ Term (termCoefficient t * k) (termPowers t)
    | (t,k) <- zip (modelTerms model) factors
    ]

-- ============================================================
-- 候補式の係数比較と数値評価
-- ============================================================

-- 同類項をまとめ、零係数項を除き、指数順に並べる。
-- 候補式には同類項の重複があっても、この処理で整理できる。
combine :: [Term] -> [Term]
combine ts =
  [ Term c p
  | p <- sort (nub (map termPowers ts))
  , let c =
          sum
            [ termCoefficient t
            | t <- ts
            , termPowers t == p
            ]
  , c /= 0
  ]

-- 独立な形式単項式の係数を比較する。
-- 一つの数値入力で結果が一致するかを調べる処理ではない。
--
-- 必要式・候補式の一方にしかない項も、他方の係数を 0 として比較。
-- 入力変数間に追加関係がある場合の同値判定は扱わない。
checkCandidate :: Model -> [Term] -> Either String [Mismatch]
checkCandidate model candidate = do
  expected <- normalizedTerms model

  ensure
    (all
      ((== length (modelVariables model)) . length . termPowers)
      candidate)
    "Candidate term arity mismatch"

  let
      a = combine expected
      b = combine candidate

      powers = sort (nub (map termPowers (a ++ b)))

      coeff ts p =
        sum
          [ termCoefficient t
          | t <- ts
          , termPowers t == p
          ]

  pure
    [ Mismatch p x y
    | p <- powers
    , let x = coeff a p
    , let y = coeff b p
    , x /= y
    ]

-- Laurent 多項式を指定入力で評価する。
--
-- 数値入力は正規化値を想定するが、ここでは単なる有理数リスト。
-- 次元や基準量識別情報は保持・検査しない。
evaluateTerms :: Int -> [Term] -> [Rational] -> Either String Rational
evaluateTerms arity ts xs = do
  ensure
    (length xs == arity
      && all ((== arity) . length . termPowers) ts)
    "Evaluation arity mismatch"

  -- 負指数を持つ変数への零入力を拒否する。
  ensure
    (all
      (\t -> all
        (\(x,p) -> x /= 0 || p >= 0)
        (zip xs (termPowers t)))
      ts)
    "Zero denominator in Laurent monomial"

  pure
    (sum
      [ termCoefficient t
          * product (zipWith (^^) xs (termPowers t))
      | t <- ts
      ])

-- ============================================================
-- 具体例の登録
-- ============================================================

-- SI 次元座標の順序は、長さ・質量・時間・電流。
-- 電圧次元 = 抵抗次元 + 電流次元。
vDim, rDim, iDim :: Dim
vDim = [2,1,-3,-1]
rDim = [2,1,-3,-2]
iDim = [0,0,0,1]

currentModel, ratioModel, twoInputModel :: Model

-- 電流換算:
--   I = (Vo - Voff) / (G * Rs)
--
-- 基準量順:
--   [VB, RB, IB] = [1 V, 1 ohm, 0.1 A]
--
-- 入力順:
--   [vo, voff, gain, rs]
--
-- 各項の基準係数は VB/(RB*IB) = 10 = kappa^(-1)。
currentModel = Model "Current conversion"
  [ Reference "VB" vDim 1
  , Reference "RB" rDim 1
  , Reference "IB" iDim (1%10)
  ]
  [ Variable "vo" (Just 0)
  , Variable "voff" (Just 0)
  , Variable "gain" Nothing
  , Variable "rs" (Just 1)
  ]
  (Just 2)
  [ Term 1 [1,0,-1,-1]
  , Term (-1) [0,1,-1,-1]
  ]

-- 電圧比:
--   V1 / V2
--
-- 出力は無次元で基準 1。
-- 基準量比 U1/U2 = 10 が正規化式の係数になる。
ratioModel = Model "Voltage ratio"
  [ Reference "U1" vDim 1
  , Reference "U2" vDim (1%10)
  ]
  [ Variable "v1" (Just 0)
  , Variable "v2" (Just 1)
  ]
  Nothing
  [Term 1 [1,-1]]

-- 二入力モデル:
--   V = R1*I1 + R2*I2
--
-- 基準量順:
--   [VB, RB1, IB1, RB2, IB2]
--
-- 全基準量の初期数値を 1 とし、初期基準係数は二つとも 1。
twoInputModel = Model "Two-input model"
  [ Reference "VB" vDim 1
  , Reference "RB1" rDim 1
  , Reference "IB1" iDim 1
  , Reference "RB2" rDim 1
  , Reference "IB2" iDim 1
  ]
  [ Variable "r1" (Just 1)
  , Variable "i1" (Just 2)
  , Variable "r2" (Just 3)
  , Variable "i2" (Just 4)
  ]
  (Just 0)
  [ Term 1 [1,1,0,0]
  , Term 1 [0,0,1,1]
  ]

-- 電力基準量の比 PB1/PB2 の保存だけを登録する。
--
-- 正規化インピーダンス行列の全成分の保存を
-- 検査する Contract ではない。
--
-- 初期値:
--   PB1/PB2 = (1*1)/(1*2) = 1/2
reciprocityContract :: Contract
reciprocityContract = Contract "Reciprocity power-base ratio"
  [ Reference "VB1" vDim 1
  , Reference "IB1" iDim 1
  , Reference "VB2" vDim 1
  , Reference "IB2" iDim 2
  ]
  [[1,1,-1,-1]]

-- 重み付き相反関係の数値比較。
--
-- z12, z21 は、ここでは正規化後の相互係数を表す。
-- audit による基準量変更の判定とは独立した補助関数。
reciprocityHolds
  :: Rational
  -> Rational
  -> Rational
  -> Rational
  -> Either String Bool
reciprocityHolds pb1 pb2 z12 z21 = do
  ensure (pb1 > 0 && pb2 > 0)
         "Power bases must be positive"
  pure (pb1 * z12 == pb2 * z21)

-- ============================================================
-- 試験用の IO 補助関数
-- ============================================================

-- 検証エラーならメッセージを表示して終了し、
-- 成功なら値を IO 内へ取り出す。
must :: Either String a -> IO a
must = either die pure

-- 条件を満たさない試験があれば、その時点で終了する。
assert :: String -> Bool -> IO ()
assert label p =
  unless p (die ("FAILED: " ++ label))

-- 拒否されるべき入力が Left を返すかを確認する。
-- エラーメッセージの内容までは比較しない。
expectLeft :: String -> Either String a -> IO ()
expectLeft label x = case x of
  Left _ -> pure ()
  Right _ -> die ("FAILED (expected rejection): " ++ label)

-- ============================================================
-- 登録回帰試験
-- ============================================================

runTests :: IO ()
runTests = do

  -- ----------------------------------------------------------
  -- 電流換算: 必要な基準係数と省略不可の確認
  -- ----------------------------------------------------------

  cc <- must (contractFromModel currentModel)
  ca <- must (audit PreserveFactorsOnly cc [1,1,1])

  -- 正項・負項とも基準係数は +10。
  -- 負項の負号は元の termCoefficient に含まれる。
  assert "current factor is inverse kappa"
    (initialFactors ca == [10,10])

  assert "current omission rejected"
    (not (initialOmissionAllowed ca))

  correct <- must (normalizedTerms currentModel)

  -- 自動導出した正規化式との比較では不一致なし。
  good <- must (checkCandidate currentModel correct)

  -- 元係数だけの式を候補とすると、基準係数の脱落を検出する。
  bad <- must
    (checkCandidate currentModel (modelTerms currentModel))

  assert "correct current coefficients"
    (null good)

  assert "missing current factor detected"
    (length bad == 2)

  -- ----------------------------------------------------------
  -- 電流換算: 指定入力で正しい式と脱落式を評価
  -- ----------------------------------------------------------

  x <- must
    (evaluateTerms 4 correct [1%5,0,10,1%10])

  y <- must
    (evaluateTerms 4 (modelTerms currentModel)
      [1%5,0,10,1%10])

  -- x, y は復元電流ではなく正規化電流。
  assert "current correct/wrong"
    (x == 2 && y == 1%5)

  -- 表の復元電流と相対偏りを、登録した出力基準量で確認する。
  -- ここでは電流モデルの出力基準量が IB（添字2）である。
  let ib = referenceScale (modelReferences currentModel !! 2)
      restoredCorrect = ib * x
      restoredWrong = ib * y
      relativeBias =
        (restoredWrong - restoredCorrect) / restoredCorrect

  assert "restored correct current"
    (restoredCorrect == 1%5)

  assert "restored current with omitted factor"
    (restoredWrong == 1%50)

  assert "relative bias from omitted factor"
    (relativeBias == (-9)%10)

  -- vo == voff の入力では、誤った式でも結果が 0 になる。
  cz <- must
    (evaluateTerms 4 correct [1%5,1%5,10,1%10])

  wz <- must
    (evaluateTerms 4 (modelTerms currentModel)
      [1%5,1%5,10,1%10])

  -- 一点で結果が一致しても、係数不一致は消えない。
  assert "one input hides error"
    (cz == 0 && wz == 0 && not (null bad))

  -- ----------------------------------------------------------
  -- 基準量変更: 省略可能性と係数保存の区別
  -- ----------------------------------------------------------

  -- VB だけを 1/10 倍すると、係数は 10 から 1 へ変わる。
  changed <- must
    (audit PreserveFactorsOnly cc [1%10,1,1])

  -- 変更後は省略可能だが、変更前の係数は保存されない。
  assert "new omission allowed but coefficients changed"
    (changedOmissionAllowed changed
      && not (factorPreserved changed))

  -- 基準量の数値を実際に変更したモデルを作る。
  let
      rs2 =
        zipWith
          (\r l -> r {referenceScale = referenceScale r*l})
          (modelReferences currentModel)
          [1%10,1,1]

      newModel =
        currentModel {modelReferences = rs2}

  newTerms <- must (normalizedTerms newModel)

  -- 同じ電圧を表すには、電圧の正規化値を 10 倍する。
  -- 出力の電流基準量は変更していないので、出力値は同じになる。
  newValue <- must
    (evaluateTerms 4 newTerms [2,0,10,1%10])

  assert "updated coefficient covariance"
    (newValue == x)

  -- 基準量変更後に古い係数を使い続ける誤りを検出する。
  stale <- must (checkCandidate newModel correct)

  assert "stale coefficients detected"
    (not (null stale))

  -- ----------------------------------------------------------
  -- 電圧比: 基準量比の脱落検出と数値評価
  -- ----------------------------------------------------------

  rc <- must (contractFromModel ratioModel)
  ra <- must (audit PreserveFactorsOnly rc [1,1])

  assert "ratio factor"
    (initialFactors ra == [10])

  rb <- must
    (checkCandidate ratioModel (modelTerms ratioModel))

  rt <- must (normalizedTerms ratioModel)
  rv <- must (evaluateTerms 2 rt [2,10])

  rawRatio <- must
    (evaluateTerms 2 (modelTerms ratioModel) [2,10])

  assert "voltage ratio without reference-factor correction"
    (rawRatio == 1%5)

  assert "ratio omission and value"
    (not (null rb) && rv == 2)

  -- ----------------------------------------------------------
  -- 二入力例: 同じ変更を二つの仕様で判定
  -- ----------------------------------------------------------

  tc <- must (contractFromModel twoInputModel)

  -- 各入力枝で抵抗倍率と電流倍率の積は 1。
  -- ただし、枝間の抵抗倍率は 2 と 3 で異なる。
  let independent = [1,2,1%2,3,1%3]

  tf <- must
    (audit PreserveFactorsOnly tc independent)

  td <- must
    (audit CommonDimensionScaling tc independent)

  assert "independent bases valid under factor-only policy"
    (factorPreserved tf
      && policySatisfied tf
      && not (allDimensionRelationsPreserved tf))

  assert "same bases violate common-scale specification"
    (not (policySatisfied td))

  -- 不足する独立条件は 1 本であり、
  -- 今回の変更はその追加条件を満たさない。
  assert "two-input gap and completion"
    (remainingDegrees td == 1
      && length (additionalChecks td) == 1
      && any ((/= 1) . snd) (additionalChecks td))

  -- ----------------------------------------------------------
  -- 追加条件を E に組み込む
  -- ----------------------------------------------------------

  let
      extraRows = map fst (additionalChecks td)

      tc2 =
        tc
          { factorExponents =
              factorExponents tc ++ extraRows
          }

  t2 <- must
    (audit CommonDimensionScaling tc2 independent)

  -- 拡張後は不足数が 0。
  -- その結果、元の変更を拡張後の係数保存検査でも拒否できる。
  assert "completion eliminates gap"
    (remainingDegrees t2 == 0
      && not (factorPreserved t2))

  -- 抵抗共通倍率 2、電流共通倍率 3、電圧倍率 6 の変更。
  coherent <- must
    (audit CommonDimensionScaling tc [6,2,3,2,3])

  assert "coherent scaling accepted"
    (policySatisfied coherent)

  -- ----------------------------------------------------------
  -- 相反比: 比の保存と共通次元倍率の区別
  -- ----------------------------------------------------------

  pr <- must
    (audit PreserveFactorsOnly reciprocityContract
      [2,1%2,3,1%3])

  assert "reciprocity contract gap"
    (remainingDegrees pr == 1
      && factorPreserved pr
      && not (allDimensionRelationsPreserved pr))

  -- 初期比が 1/2 なので、保存されても省略はできない。
  assert "unequal power bases cannot be omitted"
    (initialFactors pr == [1%2]
      && not (initialOmissionAllowed pr))

  -- 重み付き相反関係は成立するが、通常の対称性はない例。
  rh <- must (reciprocityHolds 1 2 2 1)

  assert "weighted reciprocity without ordinary symmetry"
    (rh && (2 :: Rational) /= 1)

  -- ----------------------------------------------------------
  -- 不正入力の拒否
  -- ----------------------------------------------------------

  expectLeft "zero multiplier"
    (audit PreserveFactorsOnly cc [0,1,1])

  expectLeft "multiplier arity"
    (audit PreserveFactorsOnly cc [1])

  expectLeft "nonzero-dimensional factor"
    (audit PreserveFactorsOnly
      (cc {factorExponents = [[1,0,0]]})
      [1,1,1])

  expectLeft "dimensionally wrong model"
    (contractFromModel
      (currentModel
        { modelTerms = [Term 1 [1,0,0,0]]
        }))

  expectLeft "zero reference"
    (contractFromModel
      (currentModel
        { modelReferences =
            [ Reference "VB" vDim 0
            , Reference "RB" rDim 1
            , Reference "IB" iDim 1
            ]
        }))

  expectLeft "candidate arity"
    (checkCandidate currentModel [Term 1 [1]])

  expectLeft "zero denominator"
    (evaluateTerms 4 correct [1,0,0,1])

  -- ----------------------------------------------------------
  -- 具体的な二入力行列に対する有限回帰試験
  -- ----------------------------------------------------------

  -- 各倍率を {1,2} から選び、2^5 = 32 点を全て検査する。
  --
  -- これは具体的な行列に対するソフトウェア回帰試験であり、
  -- 一般の行列についての恒等性・包含関係の証明ではない。
  let
      grid =
        [ [a,b,c,d,e]
        | a <- [1,2]
        , b <- [1,2]
        , c <- [1,2]
        , d <- [1,2]
        , e <- [1,2]
        ]

  forM_ grid $ \ls -> do
    z <- must (audit PreserveFactorsOnly tc ls)
    q <- must (audit PreserveFactorsOnly tc2 ls)

    -- GD なら GF であることを確認。
    -- 論理式「not GD || GF」は含意 GD => GF を表す。
    assert "finite regression: GD subset GF"
      (not (allDimensionRelationsPreserved z)
        || factorPreserved z)

    -- 追加条件で補完した後は、GF と GD の判定が一致する。
    assert "finite regression: completed GF equals GD"
      (factorPreserved q
        == allDimensionRelationsPreserved q)

-- ============================================================
-- 検査結果の表示
-- ============================================================

printAudit :: Policy -> Contract -> [Rational] -> IO ()
printAudit policy contract ls = do
  a <- must (audit policy contract ls)

  putStrLn
    ("\n" ++ contractName contract ++ " / " ++ show policy)

  putStrLn
    ("Reference order: "
      ++ show (map referenceName (contractReferences contract)))

  putStrLn
    ("Initial factors: " ++ show (initialFactors a))

  putStrLn
    ("Factor multipliers: " ++ show (factorMultipliers a))

  putStrLn
    ("Changed factors: " ++ show (changedFactors a))

  putStrLn
    ("Initial omission allowed: "
      ++ show (initialOmissionAllowed a))

  putStrLn
    ("Changed omission allowed: "
      ++ show (changedOmissionAllowed a))

  putStrLn
    ("Factor-preserving (GF): "
      ++ show (factorPreserved a))

  putStrLn
    ("Common dimension scaling (GD): "
      ++ show (allDimensionRelationsPreserved a))

  -- delta は検査構造の不足数であり、
  -- 今回の変更における不合格条件数ではない。
  putStrLn
    ("delta: " ++ show (remainingDegrees a))

  putStrLn
    ("Additional checks (exponents, multiplier product): "
      ++ show (additionalChecks a))

  putStrLn
    ("Selected policy satisfied: "
      ++ show (policySatisfied a))

-- ============================================================
-- エントリーポイント
-- ============================================================

main :: IO ()
main = do
  -- 一つでも登録試験が失敗すると、ここで異常終了する。
  runTests

  -- 全登録試験を通過した場合だけ表示する。
  -- プログラム全体や一般理論の正しさの証明を意味しない。
  putStrLn "All registered regression checks passed."

  -- 電流換算と電圧比について、
  -- 変更なしの検査結果と係数脱落候補の不一致を表示する。
  forM_ [currentModel, ratioModel] $ \model -> do
    c <- must (contractFromModel model)

    printAudit
      PreserveFactorsOnly
      c
      (replicate (length (contractReferences c)) 1)

    differences <- must
      (checkCandidate model (modelTerms model))

    putStrLn
      ("Candidate with reference factors omitted: "
        ++ show differences)

  tc <- must (contractFromModel twoInputModel)

  -- 同じ変更でも、要求仕様によって最終合否が異なる例。
  printAudit PreserveFactorsOnly
    tc [1,2,1%2,3,1%3]

  printAudit CommonDimensionScaling
    tc [1,2,1%2,3,1%3]

  -- 共通次元倍率に従う変更。
  printAudit CommonDimensionScaling
    tc [6,2,3,2,3]

  -- 電力基準量の比だけを保存する変更。
  printAudit PreserveFactorsOnly
    reciprocityContract [2,1%2,3,1%3]