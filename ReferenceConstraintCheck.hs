module Main where

import Control.Monad (unless, forM_)
import Data.List (findIndex, foldl', nub, sort, transpose)
import Data.Ratio ((%), numerator, denominator)
import System.Exit (die)

-- 基準係数の省略条件と基準量変更の検査。
-- 有理数演算を使い、係数比較と変更仕様の判定を行う。


-- 基本型と登録情報

-- 次元座標は長さ・質量・時間・電流の順。
type Dim = [Integer]

type Row = [Rational]
type Matrix = [Row]

-- referenceScale は固定した整合的単位系での基準値 b_i。
-- 正規化値や基準量の変更倍率とは区別する。
data Reference = Reference
  { referenceName :: String
  , referenceDim :: Dim
  , referenceScale :: Rational
  } deriving (Eq, Show)

-- Just i は i 番目の基準量を使う変数（0始まり）。
-- Nothing は無次元変数を表し、基準値は1とする。
data Variable = Variable
  { variableName :: String
  , variableReference :: Maybe Int
  } deriving (Eq, Show)

-- 整数指数の単項式。指数の順序は modelVariables に合わせる。
-- [vo, voff, gain, rs] に対する [1,0,-1,-1] は vo/(gain*rs)。
data Term = Term
  { termCoefficient :: Rational
  , termPowers :: [Integer]
  } deriving (Eq, Show)

-- modelTerms には基準係数を掛ける前の式を登録する。
-- 出力が無次元なら modelOutput は Nothing。
data Model = Model
  { modelName :: String
  , modelReferences :: [Reference]
  , modelVariables :: [Variable]
  , modelOutput :: Maybe Int
  , modelTerms :: [Term]
  } deriving (Eq, Show)

-- 検査対象の係数 k_a = product_i b_i^(e_ai) を指数行で登録する。
-- Model を介さず、無次元積だけを直接登録することもできる。
data Contract = Contract
  { contractName :: String
  , contractReferences :: [Reference]
  , factorExponents :: [[Integer]]
  } deriving (Eq, Show)

-- 登録係数の保存だけを要求するか、共通次元倍率まで要求するか。
data Policy = PreserveFactorsOnly | CommonDimensionScaling
  deriving (Eq, Show)

data Audit = Audit
  { initialFactors :: [Rational]
  , factorMultipliers :: [Rational]  -- k_a' / k_a
  , changedFactors :: [Rational]
  , initialOmissionAllowed :: Bool
  , changedOmissionAllowed :: Bool
  , factorPreserved :: Bool
  , allDimensionRelationsPreserved :: Bool
  , allDimensionChecks :: [([Integer], Rational)]
  , additionalChecks :: [([Integer], Rational)]
  , remainingDegrees :: Int
      -- 元の検査行列に対する不足数 delta。不合格件数ではない。
  , policySatisfied :: Bool
  } deriving (Eq, Show)

data Mismatch = Mismatch
  { mismatchPowers :: [Integer]
  , expectedCoefficient :: Rational
  , suppliedCoefficient :: Rational
  } deriving (Eq, Show)


-- 登録情報の検証

ensure :: Bool -> String -> Either String ()
ensure p msg = if p then Right () else Left msg

-- 基準値の正値性、次元ベクトルの長さ、識別名の重複を確認する。
validateReferences :: [Reference] -> Either String Int
validateReferences [] =
  Left "No references"

validateReferences rs@(r : _) = do
  let d = length (referenceDim r)

  ensure (d > 0)
         "Empty dimension vector"

  ensure (all ((== d) . length . referenceDim) rs)
         "Dimension-vector length mismatch"

  ensure (all ((> 0) . referenceScale) rs)
         "Reference scales must be positive"

  ensure (length (nub (map referenceName rs)) == length rs)
         "Duplicate reference IDs"

  pure d


-- 有理数による行列計算

-- 簡約行階段形とピボット列を返す。
-- 各行が n 成分であることは呼出し側で確認する。
rref :: Int -> Matrix -> (Matrix, [Int])
rref n input = go 0 0 input []
  where
    go r c a piv
      | r >= length a || c >= n = (a, piv)
      | otherwise =
          case findIndex ((/= 0) . (!! c)) (drop r a) of
            Nothing ->
              go r (c + 1) a piv

            Just k ->
              let
                  j = r + k

                  swapped =
                    [ if i == r then a !! j
                      else if i == j then a !! r
                      else row
                    | (i, row) <- zip [0..] a
                    ]

                  pivot =
                    map (/ ((swapped !! r) !! c)) (swapped !! r)

                  reduced =
                    [ if i == r then pivot
                      else zipWith (-) row
                             (map (* (row !! c)) pivot)
                    | (i, row) <- zip [0..] swapped
                    ]

              in go (r + 1) (c + 1) reduced (piv ++ [c])

rank :: Int -> Matrix -> Int
rank n = length . snd . rref n

-- A z = 0 の有理数基底。基底ベクトルを行として返す。
nullspace :: Int -> Matrix -> Matrix
nullspace n a =
  let
      (r, piv) = rref n a
      free = filter (`notElem` piv) [0..n - 1]

      column f j =
        case lookup j (zip piv r) of
          Just row -> negate (row !! f)
          Nothing -> if j == f then 1 else 0

  in [ [column f j | j <- [0..n - 1]] | f <- free ]

-- 分母を払い、成分の最大公約数と符号をそろえる。
-- 個々のベクトルの整数化であり、整数核の格子基底は求めない。
primitive :: Row -> [Integer]
primitive row =
  let
      l = foldl' lcm 1 (map denominator row)
      ints = map (\x -> numerator (x * fromInteger l)) row
      g = foldl' gcd 0 (map abs ints)
      v = if g == 0 then ints else map (`div` g) ints

  in case dropWhile (== 0) v of
       (x : _) | x < 0 -> map negate v
       _ -> v

-- 基準量の次元ベクトルを列に並べる。
dimensionMatrix :: [Reference] -> Matrix
dimensionMatrix =
  transpose . map (map fromInteger . referenceDim)

-- E の行空間を ker M まで補う行を選ぶ。
-- 既存行と、それまでに追加した行から独立なものだけを採用する。
-- E の各行が ker M に属することを前提とする。
completion :: Int -> Matrix -> Matrix -> Matrix
completion n e m =
  snd (foldl' step (e, []) (nullspace n m))
  where
    step (rows, added) z
      | rank n (rows ++ [z]) > rank n rows =
          (rows ++ [z], added ++ [z])
      | otherwise =
          (rows, added)


-- 基準係数と基準量変更

-- 基準値を渡せば係数、変更倍率を渡せば係数の変更比になる。
-- 指数には負の整数も許す。リスト長は呼出し側で確認する。
factorValue :: [Rational] -> [Integer] -> Rational
factorValue scales exponents =
  product (zipWith (^^) scales exponents)

-- 登録係数の保存と、全次元関係の保存を別々に判定する。
-- 対数は計算せず、有理数の倍率積を直接比較する。
-- 倍率が正なので、核基底の分母払いでも検査条件は変わらない。
audit :: Policy -> Contract -> [Rational] -> Either String Audit
audit policy contract lambdas = do
  d <- validateReferences rs

  let
      n = length rs
      eInt = factorExponents contract
      e = map (map fromInteger) eInt
      m = dimensionMatrix rs

  ensure (length lambdas == n && all (> 0) lambdas)
         "Invalid positive multipliers"

  ensure (all ((== n) . length) eInt)
         "Exponent-vector length mismatch"

  -- 登録した係数が無次元かを確認する：M E^T = 0。
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
      old = map (factorValue scales) eInt
      ratios = map (factorValue lambdas) eInt
      new = zipWith (*) old ratios

      -- ker M を張る指数による全関係の検査。
      full =
        [ (z, factorValue lambdas z)
        | z <- map primitive (nullspace n m)
        ]

      -- 既存の係数検査を補う追加条件。
      extra =
        [ (z, factorValue lambdas z)
        | z <- map primitive (completion n e m)
        ]

      gf = all (== 1) ratios
      gd = all ((== 1) . snd) full

      -- delta は検査構造の不足数であり、今回の不合格数ではない。
      delta = n - rank n e - rank n m

      -- M E^T = 0 の下では gd から gf が従う。
      ok = gf && (policy == PreserveFactorsOnly || gd)

  pure
    (Audit
      old ratios new
      (all (== 1) old)
      (all (== 1) new)
      gf gd full extra delta ok)

  where
    rs = contractReferences contract


-- モデルからの係数導出

-- 登録した各項の次元と出力次元が一致するかを確認する。
-- 物理モデル自体の妥当性を検証するものではない。
validateModel :: Model -> Either String ()
validateModel model = do
  d <- validateReferences rs

  let
      n = length rs
      valid Nothing = True
      valid (Just i) = i >= 0 && i < n

  ensure
    (all (valid . variableReference) vs
      && valid (modelOutput model))
    "Reference index out of range"

  ensure
    (length (nub (map variableName vs)) == length vs)
    "Duplicate variable names"

  ensure (not (null ts))
         "Empty model"

  ensure
    (all ((== length vs) . length . termPowers) ts)
    "Term arity mismatch"

  -- 元モデルは、同類項をまとめて零項を除いた形で登録する。
  ensure
    (all ((/= 0) . termCoefficient) ts)
    "Zero term: combine like terms first"

  ensure
    (length (nub (map termPowers ts)) == length ts)
    "Duplicate monomials"

  let
      dimOf Nothing = replicate d 0
      dimOf (Just i) = referenceDim (rs !! i)

      outDim = dimOf (modelOutput model)

      dimTerm t =
        foldl' (zipWith (+)) (replicate d 0)
          [ map (* p) (dimOf (variableReference v))
          | (p, v) <- zip (termPowers t) vs
          ]

  ensure
    (all ((== outDim) . dimTerm) ts)
    "Dimensionally inhomogeneous model"

  where
    rs = modelReferences model
    vs = modelVariables model
    ts = modelTerms model

-- 入力基準量の指数を合算し、出力基準量の指数を1引いて E を作る。
contractFromModel :: Model -> Either String Contract
contractFromModel model = do
  validateModel model

  let
      rs = modelReferences model
      n = length rs

      exponent t i =
        sum
          [ p
          | (p, v) <-
              zip (termPowers t) (modelVariables model)
          , variableReference v == Just i
          ]
        - (if modelOutput model == Just i then 1 else 0)

      rows =
        [ [exponent t i | i <- [0..n - 1]]
        | t <- modelTerms model
        ]

  pure (Contract (modelName model) rs rows)

-- 各項の元係数に、導出した基準係数を掛ける。
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
    | (t, k) <- zip (modelTerms model) factors
    ]


-- 候補式の比較と数値評価

-- 同類項をまとめ、零項を除いて指数順に並べる。
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

-- 独立な形式変数について係数を比較する。
-- 一点での値の一致や、変数間の追加関係による同値性は扱わない。
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

-- 指定入力で Laurent 多項式を評価する。
-- 入力は数値だけを受け取り、次元や基準量の情報は検査しない。
evaluateTerms :: Int -> [Term] -> [Rational] -> Either String Rational
evaluateTerms arity ts xs = do
  ensure
    (length xs == arity
      && all ((== arity) . length . termPowers) ts)
    "Evaluation arity mismatch"

  ensure
    (all
      (\t -> all
        (\(x, p) -> x /= 0 || p >= 0)
        (zip xs (termPowers t)))
      ts)
    "Zero denominator in Laurent monomial"

  pure
    (sum
      [ termCoefficient t
          * product (zipWith (^^) xs (termPowers t))
      | t <- ts
      ])


-- 具体例

-- 使用する次元：電圧 = 抵抗 + 電流。
vDim, rDim, iDim :: Dim
vDim = [2,1,-3,-1]
rDim = [2,1,-3,-2]
iDim = [0,0,0,1]

currentModel, ratioModel, twoInputModel :: Model

-- 電流換算：I = (Vo - Voff)/(G*Rs)
-- 基準量 [VB, RB, IB] = [1 V, 1 ohm, 0.1 A]。
-- 入力順は [vo, voff, gain, rs]。両項の基準係数は10。
currentModel = Model "Current conversion"
  [ Reference "VB" vDim 1
  , Reference "RB" rDim 1
  , Reference "IB" iDim (1 % 10)
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

-- 電圧比 V1/V2。無次元出力だが、基準量比 U1/U2 = 10 が残る。
ratioModel = Model "Voltage ratio"
  [ Reference "U1" vDim 1
  , Reference "U2" vDim (1 % 10)
  ]
  [ Variable "v1" (Just 0)
  , Variable "v2" (Just 1)
  ]
  Nothing
  [Term 1 [1,-1]]

-- 二入力モデル：V = R1*I1 + R2*I2
-- 基準量順は [VB, RB1, IB1, RB2, IB2]。
-- 初期基準値はすべて1。
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

-- 電力基準比 PB1/PB2 の保存を調べる。初期値は1/2。
-- 正規化インピーダンス行列の全成分を検査するものではない。
reciprocityContract :: Contract
reciprocityContract = Contract "Reciprocity power-base ratio"
  [ Reference "VB1" vDim 1
  , Reference "IB1" iDim 1
  , Reference "VB2" vDim 1
  , Reference "IB2" iDim 2
  ]
  [[1,1,-1,-1]]

-- 正規化相互係数の重み付き相反関係を確認する。
-- 基準量変更の audit とは別の数値比較。
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


-- 試験用の補助関数

-- 検証エラーがあれば、その内容を表示して終了する。
must :: Either String a -> IO a
must = either die pure

assert :: String -> Bool -> IO ()
assert label p =
  unless p (die ("FAILED: " ++ label))

-- 拒否されることだけを確認し、エラー文面までは比較しない。
expectLeft :: String -> Either String a -> IO ()
expectLeft label x =
  case x of
    Left _ -> pure ()
    Right _ -> die ("FAILED (expected rejection): " ++ label)


-- 回帰試験

runTests :: IO ()
runTests = do
  -- 電流換算：係数の導出と脱落の検出。
  cc <- must (contractFromModel currentModel)
  ca <- must (audit PreserveFactorsOnly cc [1,1,1])

  -- 負項の符号は元係数に含まれるので、基準係数は両項とも10。
  assert "current factor is inverse kappa"
    (initialFactors ca == [10,10])

  assert "current omission rejected"
    (not (initialOmissionAllowed ca))

  correct <- must (normalizedTerms currentModel)
  good <- must (checkCandidate currentModel correct)

  bad <- must
    (checkCandidate currentModel (modelTerms currentModel))

  assert "correct current coefficients"
    (null good)

  assert "missing current factor detected"
    (length bad == 2)

  -- 同じ入力で正規化電流を比較する。
  x <- must
    (evaluateTerms 4 correct [1 % 5,0,10,1 % 10])

  y <- must
    (evaluateTerms 4 (modelTerms currentModel)
      [1 % 5,0,10,1 % 10])

  assert "current correct/wrong"
    (x == 2 && y == 1 % 5)

  -- 出力基準量 IB で電流を復元し、脱落による相対偏りを確認する。
  let
      ib = referenceScale (modelReferences currentModel !! 2)
      restoredCorrect = ib * x
      restoredWrong = ib * y
      relativeBias =
        (restoredWrong - restoredCorrect) / restoredCorrect

  assert "restored correct current"
    (restoredCorrect == 1 % 5)

  assert "restored current with omitted factor"
    (restoredWrong == 1 % 50)

  assert "relative bias from omitted factor"
    (relativeBias == (-9) % 10)

  -- vo = voff では両式とも0になるが、形式的な不一致は残る。
  cz <- must
    (evaluateTerms 4 correct [1 % 5,1 % 5,10,1 % 10])

  wz <- must
    (evaluateTerms 4 (modelTerms currentModel)
      [1 % 5,1 % 5,10,1 % 10])

  assert "one input hides error"
    (cz == 0 && wz == 0 && not (null bad))

  -- VB を1/10倍すると係数は10から1になる。
  -- 省略可能になることと、元の係数を保存することは別。
  changed <- must
    (audit PreserveFactorsOnly cc [1 % 10,1,1])

  assert "new omission allowed but coefficients changed"
    (changedOmissionAllowed changed
      && not (factorPreserved changed))

  let
      rs2 =
        zipWith
          (\r l -> r {referenceScale = referenceScale r * l})
          (modelReferences currentModel)
          [1 % 10,1,1]

      newModel =
        currentModel {modelReferences = rs2}

  newTerms <- must (normalizedTerms newModel)

  -- 同じ物理入力に合わせて電圧の正規化値も変更する。
  newValue <- must
    (evaluateTerms 4 newTerms [2,0,10,1 % 10])

  assert "updated coefficient covariance"
    (newValue == x)

  -- 基準量を変えた後に旧係数を使うと不一致になる。
  stale <- must (checkCandidate newModel correct)

  assert "stale coefficients detected"
    (not (null stale))

  -- 電圧比：基準量比を落とした候補式との比較。
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
    (rawRatio == 1 % 5)

  assert "ratio omission and value"
    (not (null rb) && rv == 2)

  -- 二入力モデル：同じ変更を二つの仕様で判定する。
  tc <- must (contractFromModel twoInputModel)

  -- 各枝の抵抗・電流倍率の積は1だが、抵抗倍率は枝間で異なる。
  let independent = [1,2,1 % 2,3,1 % 3]

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

  assert "two-input gap and completion"
    (remainingDegrees td == 1
      && length (additionalChecks td) == 1
      && any ((/= 1) . snd) (additionalChecks td))

  -- 追加条件を登録し、拡張後の係数保存検査でも元の変更を拒否する。
  let
      extraRows = map fst (additionalChecks td)

      tc2 =
        tc
          { factorExponents =
              factorExponents tc ++ extraRows
          }

  t2 <- must
    (audit CommonDimensionScaling tc2 independent)

  assert "completion eliminates gap"
    (remainingDegrees t2 == 0
      && not (factorPreserved t2))

  -- 電圧6倍、抵抗2倍、電流3倍なら共通次元倍率の条件を満たす。
  coherent <- must
    (audit CommonDimensionScaling tc [6,2,3,2,3])

  assert "coherent scaling accepted"
    (policySatisfied coherent)

  -- 電力基準比は保存されても、共通次元倍率の条件は満たさない。
  pr <- must
    (audit PreserveFactorsOnly reciprocityContract
      [2,1 % 2,3,1 % 3])

  assert "reciprocity contract gap"
    (remainingDegrees pr == 1
      && factorPreserved pr
      && not (allDimensionRelationsPreserved pr))

  assert "unequal power bases cannot be omitted"
    (initialFactors pr == [1 % 2]
      && not (initialOmissionAllowed pr))

  -- 非対称な正規化行列でも、重み付き相反関係は成立する。
  rh <- must (reciprocityHolds 1 2 2 1)

  assert "weighted reciprocity without ordinary symmetry"
    (rh && (2 :: Rational) /= 1)

  -- 不正入力を拒否できることを確認する。
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

  -- {1,2}^5 の32点で、包含関係と補完後の一致を回帰検査する。
  -- この有限検査を一般の場合の証明とするものではない。
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

    assert "finite regression: GD subset GF"
      (not (allDimensionRelationsPreserved z)
        || factorPreserved z)

    assert "finite regression: completed GF equals GD"
      (factorPreserved q
        == allDimensionRelationsPreserved q)


-- 結果の表示

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

  -- 元の検査行列に対する不足数。
  putStrLn
    ("delta: " ++ show (remainingDegrees a))

  putStrLn
    ("Additional checks (exponents, multiplier product): "
      ++ show (additionalChecks a))

  putStrLn
    ("Selected policy satisfied: "
      ++ show (policySatisfied a))


main :: IO ()
main = do
  runTests
  putStrLn "All registered regression checks passed."

  -- 変更なしの場合の係数と、省略候補の不一致を表示する。
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

  -- 同じ変更を係数保存仕様と共通次元倍率仕様で比較する。
  printAudit PreserveFactorsOnly
    tc [1,2,1 % 2,3,1 % 3]

  printAudit CommonDimensionScaling
    tc [1,2,1 % 2,3,1 % 3]

  printAudit CommonDimensionScaling
    tc [6,2,3,2,3]

  -- 電力基準比を保存する変更。
  printAudit PreserveFactorsOnly
    reciprocityContract [2,1 % 2,3,1 % 3]
