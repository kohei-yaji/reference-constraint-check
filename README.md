# 基準量変更における係数保存検査の充足性判定と最小補完

同名論文のHaskell参照実装です。
基準係数の検査、基準量変更の仕様判定、不足する検査条件の構成を行います。

## 実行方法

動作確認環境：GHC 9.10.3（GHCi）

GHCiで次を実行します。

```text
:load ReferenceConstraintCheck.hs
:main
```

登録した回帰試験が全て成功すると、次のメッセージが表示されます。

```text
All registered regression checks passed.
```

続いて、主要な監査結果と候補式の係数不一致が表示されます。
回帰試験内で確認する全ての数値を表示するわけではありません。

## 検査内容

有理数による厳密計算を用いて、次を確認します。

- 基準係数の導出と省略可能性
- 候補式との単項式係数比較
- 係数保存仕様と共通次元倍率仕様への適合
- 不足数と追加検査条件
- 不正入力の拒否

登録例は、電流換算、電圧比、二入力モデル、基準量の積の比です。
重み付き相反関係の数値比較と、二入力モデルの全32点の回帰試験も含みます。

## 代表的な実行結果

登録回帰試験では、次の成功メッセージを確認しました。

```text
All registered regression checks passed.
```

二入力モデルに変更倍率 `(1,2,1/2,3,1/3)` を与えると、
次の結果になります。

```text
Factor-preserving (GF): True
Common dimension scaling (GD): False
delta: 1
Additional checks (exponents, multiplier product): [([0,1,0,-1,0],2 % 3)]
```

選択した係数は保存されますが、共通次元倍率仕様には適合しません。
不足数は1であり、追加条件に対応する倍率積は `2/3` となります。
この変更は、係数保存仕様では合格、共通次元倍率仕様では不合格です。

## 論文中の検査と試験項目の対応

| 検査 | 主な試験ラベル |
|---|---|
| 電流換算の基準係数 | `current factor is inverse kappa` |
| 電流換算の係数脱落 | `missing current factor detected` |
| 正しい候補式との比較 | `correct current coefficients` |
| 復元電流と相対偏り | `restored correct current`、`restored current with omitted factor`、`relative bias from omitted factor` |
| 零出力での誤りの見逃し | `one input hides error` |
| 基準変更後の式と入力の更新 | `updated coefficient covariance` |
| 旧係数の不一致 | `stale coefficients detected` |
| 電圧比 | `ratio factor`、`ratio omission and value` |
| 二入力モデルの仕様判定 | `independent bases valid under factor-only policy`、`same bases violate common-scale specification` |
| 不足数と検査補完 | `two-input gap and completion`、`completion eliminates gap` |
| 共通倍率の適合例 | `coherent scaling accepted` |
| 電力基準比 | `reciprocity contract gap`、`unequal power bases cannot be omitted` |
| 重み付き相反関係 | `weighted reciprocity without ordinary symmetry` |
| 32点の回帰試験 | `finite regression: GD subset GF`、`finite regression: completed GF equals GD` |

不正入力7例は、`expectLeft`で拒否されることを確認します。


## 出力の読み方

| 出力項目 | 意味 |
|---|---|
| `Reference order` | 基準量と変更倍率の並び順 |
| `Initial factors` | 変更前の基準係数 |
| `Factor multipliers` | 各基準係数の変更比 |
| `Changed factors` | 変更後の基準係数 |
| `Initial omission allowed` | 変更前の基準係数が全て1か |
| `Changed omission allowed` | 変更後の基準係数が全て1か |
| `Factor-preserving (GF)` | 選択した係数が全て保存されるか |
| `Common dimension scaling (GD)` | 次元関係に対応する全ての無次元積の値が保存されるか |
| `delta` | 元の係数検査に不足する独立条件数 |
| `Additional checks` | 追加指数と、その変更倍率積 |
| `Selected policy satisfied` | 選択した仕様に対する最終合否 |

`delta` は検査構造に依存する値です。今回の倍率が共通次元倍率仕様に適合していても、元の検査に不足があれば `delta > 0` と表示されます。

候補式の不一致は、次の情報で表示します。

- `mismatchPowers`：不一致がある単項式の指数
- `expectedCoefficient`：登録モデルから導出した必要係数
- `suppliedCoefficient`：候補式の係数

## 主な関数

| 関数 | 役割 |
|---|---|
| `contractFromModel` | 登録モデルから係数指数行列を導出 |
| `normalizedTerms` | 基準係数を反映した正規化式を生成 |
| `checkCandidate` | 必要式と候補式の単項式係数を比較 |
| `evaluateTerms` | 指定した正規化入力で式を評価 |
| `audit` | 基準量変更と選択仕様への適合を判定 |
| `completion` | 不足する独立な検査関係を選択 |
| `reciprocityHolds` | 重み付き相反関係の両辺を数値比較 |
| `runTests` | 登録された回帰試験を実行 |
| `main` | 回帰試験を実行後、主要な検査結果を表示 |


## 注意事項

対象は、有理数係数・整数指数の有限Laurent多項式と、正の有理数の基準値・変更倍率です。

元モデルは同類項をまとめ、零項を除いて登録します。
候補式の同類項と零項は比較時に整理します。

候補式は独立な形式変数について比較し、変数間の追加関係に基づく同値性は扱いません。
数値評価の入力は有理数列であり、入力値自体の次元や基準量との対応は検査しません。

不足数 `delta` は、既存の係数保存検査を共通次元倍率仕様と一致させるために追加すべき独立な線形条件の最小数です。今回の変更で不合格となった条件数ではありません。

回帰試験の成功は、登録例での動作確認を意味し、一般理論や実装全体の正しさを証明するものではありません。
