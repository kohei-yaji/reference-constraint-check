# reference-constraint-check
Haskell reference implementation for checking reference factors and reference changes in quantity equations.
# 基準係数と基準量変更の整合性検査

論文「量方程式における基準係数の省略条件と基準量変更の整合性」のHaskell参照実装です。

## 実行方法

GHCiで次を実行します。

```text
:load ReferenceConstraintCheck.hs
:main
```

登録した回帰試験が全て成功すると、次のメッセージと検査結果が表示されます。

```text
All registered regression checks passed.
```

## 検査内容

有理数による厳密計算を用いて、次を確認します。

- 基準係数の導出と省略可能性
- 候補式との単項式係数比較
- 係数保存仕様と共通次元倍率仕様への適合
- 不足数と追加検査条件
- 不正入力の拒否

登録例は、電流換算、電圧比、二入力モデル、基準量の積の比です。
重み付き相反関係の数値比較と、二入力モデルの全32点の回帰試験も含みます。


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
| `Common dimension scaling (GD)` | 全次元関係が保存されるか |
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
不足数 `delta` は、共通次元倍率仕様との一致に必要な追加の独立条件数であり、今回の変更で不合格となった条件数ではありません。
回帰試験の成功は、登録例での動作確認を意味し、一般理論や実装全体の正しさを証明するものではありません。
