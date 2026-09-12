# Go バックエンドの性能調査

Date: 2026-09-12
Commit: b4523a11 (feat/go-backend)
Machine: Darwin arm64

Go バックエンド（#482）が Level 1 で 0.41s、Zig の 0.24s に対して 1.7 倍遅い。その内訳を実測で特定し、改善案が本当に効くかを 1 件ずつ検証した記録。

**結論から**: 当初有望に見えた案の大半は効かないか逆効果だった。効いたのは文字列プリミティブのスカラキャッシュ（8%）だけで、残る唯一の構造的レバーは dispatch 回数の削減である。1 dispatch あたりの費用は Go の ABI が決める固定費で、下げる手段が無い。

---

## 1. 測定手順の注意

以降の数字を再現・比較する際に必ず守ること。両方とも実測で確認した罠である。

### macOS の Go CPU プロファイラはこのワークロードでは使えない

`runtime/pprof` で CPU プロファイルを取ると、サンプルの 72〜82% が `runtime.kevent` に落ちる。ブロック中の netpoll スレッドにプロファイリングシグナルが届くためで、実作業が埋もれて読めない。`GOMAXPROCS=1` でも `kevent` が 72% を占めた。

```
      flat  flat%   sum%        cum   cum%
     1.13s 71.97% 71.97%      1.13s 71.97%  runtime.kevent
     0.08s  5.10% 77.07%      0.08s  5.10%  runtime.madvise
```

`-focus=main\.main` で絞れば読めるが、それでも 10.46s のサンプル中 `main.run` は 0.97s しか拾えず、配分を信用できない。

**代わりに使った手法**: `malgo compile --target go` が残す `OUT.go` を直接パッチしてビルドし、wall clock を測る。プロファイラを介さないので歪みが無く、しかも「その変更が本当に速いか」を直接答える。本調査の数字はすべてこの方法による。

### パス長で 3 倍変わる

同じ入力でも、コマンドライン引数のパスの長さで実行時間が 3 倍変わる。

| | 相対パス | 絶対パス |
|---|---|---|
| Go | 0.41s | 1.27s |
| Zig | 0.24s | 0.68s |

自己ホスト評価器はモジュール名を文字列として持ち回るので、パスが長いと仕事量そのものが増える。dispatch 回数も 11.3M → 31.8M と 2.8 倍になる。

**測定は必ずリポジトリルートから相対パスで行う**（`./malgoc test/testcases/malgo/Fib.mlg`）。比率（Go/Zig ≈ 1.7）はどちらでも変わらないので、既存の比較結果は有効である。

---

## 2. 収支

Level 1 評価器（`runtime/malgo/compiler/Main.mlg` を `--opt release-fast` でコンパイル）に `test/testcases/malgo/Fib.mlg` を食わせた 1 回の実行。

| 項目 | 値 |
|---|---|
| wall clock | 0.41s |
| dispatch 回数 | 11,275,440 |
| Go の malloc 回数 | 11,107,245 |
| うちランタイムの値コンストラクタ | 1,945,517 (17.5%) |
| 残り（クロージャ + 可変長スライス） | 約 9.2M (82.5%) |

**dispatch と malloc がほぼ 1:1。** CPS なので継続ごとに Go のクロージャが 1 個できる。

割り当て元を `runtime/pprof` のヒーププロファイルで特定した（ヒーププロファイルは CPU と違って信頼できる）。

```
   3377744 14.98%  main.mkStruct (inline)
    742059  3.29%  main.m_..._subInt32.func1.1.1
    731254  3.24%  main.m_..._fib.func1.1.1
    ...
```

`.funcN.N` はすべて生成コード中の `Fn(func(args []Value) Action {...})`、つまり join point のクロージャである。行番号まで落とすと `$select_N = Fn(...)` や `$return_N = Fn(...)` を指す。

**`mkInt32` は上位 200 件に 1 度も現れない。** intern テーブル（`-128..1024`）が完全に吸収している。

---

## 3. 微小ベンチによる単価

トランポリンと同じ形（`Action{Code, Argv [2]Value, Argc}` を返し、`run` がループで dispatch）を切り出して 3,100 万回まわした。

| 構成 | ns/dispatch |
|---|---|
| 素の dispatch のみ | 4.8 〜 5.5 |
| + 1 クロージャ確保 | 17.5 |
| + 1 `mkStruct` | 58.9 |
| + 1 型アサーション | **5.7**（素の dispatch と誤差内） |

読み:

- **素の dispatch は 11.3M × 5ns ≈ 0.077s、全体の 19%。これが下限である。**
- クロージャ 1 個の確保は +12.7ns。join point 1 個あたりのコストはこれに dispatch 1 回分が乗る。
- **型アサーションは実質ゼロ**（誤差内で floor より速く出た）。

---

## 4. 検証した改善案

### 4.1 interface boxing の除去 — 効果なし

当初の仮説は「`Value` が interface なので、スカラのボクシングが効いている」だった。**測定はこれを否定した。**

- 型アサーションのコストは誤差内でゼロ（前掲）
- スカラの確保は `mkString` 0.94% + `mkInt64` 0.86% = **全 malloc の 1.8%**
- `mkInt32` は intern が吸収して 0%
- fib-deep に至ってはスカラ構築が 0 回

仮に interface をやめても、消えるのは割り当ての 2% だけで、dispatch は 1ns も速くならない。

なお **NaN boxing とタグ付きポインタは Go では実現不可能**である。GC がポインタ型のワードを実ポインタとして辿るため、下位ビットにタグを詰めた値は不正になる。`uintptr` に逃がせば GC が辿らず、参照先が回収される。ハンドルテーブルを自前で持てば回避できるが、それは自前 GC を書くことになる。

interface をやめる唯一安全な道は、値渡しのタグ付き構造体（`struct { obj *Object; bits uint64 }`、16 バイトで interface と同幅）だが、`Fn` は pointer-shaped なので今は interface に確保ゼロで入る。タグ付き構造体では 3 ワード目が必要になり、`Action` が 48→64 バイトに太る。**2% の割り当て削減と引き換えに最も熱いデータ構造を 1.3 倍にする取引**であり、割に合わない。

### 4.2 `Str` に `[]rune` をキャッシュ — 逆効果

CPU プロファイル（絶対パス条件、歪みあり）で `unicode/utf8.RuneCountInString` と `byteOffsetOfScalar` が上位に出たため、`Str` に `[]rune` を持たせて O(1) 添字にした。

**0.41s → 0.78s（2 倍遅い）。**

理由: 自己ホスト評価器は短命な文字列（部分文字列、トークン）を大量に作る。その 1 つ 1 つに対して初回アクセスで 4 バイト/文字の rune スライスを確保するので、**確保コストが O(n²) スキャンより高くつく**。

この失敗は、確保が支配的だという仮説を裏付ける材料にもなった。

### 4.3 `Str` にスカラのみキャッシュ — 有効、8%

確保を伴わないスカラ 2 つ（コードポイント数と ASCII フラグ）だけを `Str` に持たせ、初回アクセスで 1 度走査して埋める。ASCII なら `malgo_string_at` / `malgo_substring` をバイト添字で O(1) に落とす。

**0.41s → 0.38s（約 8%）。**

Malgo のソーステキストとトークンがほぼ ASCII なので効く。非 ASCII は現行経路に落ちるだけで遅くはならない。

Zig ランタイムも同じ O(n) 走査を持っている（`utf8ByteOffsetOfScalar`、`malgo_string_length`）。同じ手が効く可能性が高いが未測定。

### 4.4 `mkStruct` の可変長引数除去 — 誤差レベル

`mkStruct(tag string, fields ...Value)` は呼び出しごとにスライスの裏配列と `Struct` の 2 回確保する。`Struct` に小さい配列を埋め込んだ固定アリティ版（`mkStruct0`〜`mkStruct3`）を作り、生成ソースの 606 箇所を書き換えた。

**0.41s → 0.40s（2.5%）。**

4.4M 回の確保を消してこれだけ、ということは**確保の単価が約 2.3ns と安い**ということである。3.3 の微小ベンチが示す 58.9ns は、ベンチ内で `Struct` が次の反復まで生き残り GC のマーク対象になるための過大評価だった。

### 4.5 トランポリンの形の変更 — 割に合わない

`Action` を値で返す代わりに、呼び出し先が `*Action` に書き込む形にすると、48 バイトの構造体コピーが消える。

現実的な引数トラフィック（毎回 args を読んで新しい値を渡す）での実測:

| 形 | ns/dispatch |
|---|---|
| v1 現行（`Action` を返す） | 4.84 〜 5.52 |
| v4 `*Action` に書き込む | **4.03 〜 4.07** |
| v1 + クロージャ 1 個 | 17.5 〜 17.9 |
| v4 + クロージャ 1 個 | 14.4 |

素の dispatch で約 17% 速いが、全体では 11.3M × 1ns = 0.011s、**2.7%** にしかならない。呼び出し規約を全面的に変える代償に見合わない。

引数を読まない自明なハンドラでは 1.3ns（-73%）が出たが、これは引数トラフィックが無い場合の値で、実際のコードには当てはまらない。

### 4.6 generics — 割に合わない

算術・比較の 60 個近い一行関数（`func malgo_add_int32_t(a0, a1 Value) Value { return mkInt32(asI32(a0) + asI32(a1)) }` 形）を generics でまとめられれば、「ランタイムを小さく」に効く。

| 実装 | ns/op | 対 手書き |
|---|---|---|
| 手書き（現行） | 1.8 | — |
| generics（単相化、関数値なし） | 2.5 | **+44%** |
| generics（演算を関数値で渡す） | 4.4 | +150% |

単相化しても 44% 遅い。型制約にメソッド（`get() T`）を要求する形になるため、直接のフィールド読みがインタフェース呼び出しに変わるからである。縮むのは `runtime.go` 968 行のうち約 40 行（4%）だけ。

### 4.7 リフレクション — 使えない

同じベンチで **8.3ns/op、手書きの 4.5 倍**。

加えて、primitive を名前で登録する設計にすると失敗モードが退化する。現在は emitter が `malgo_add_int32_t(a, b)` を直接の Go 呼び出しとして吐くので、アリティ不一致は**生成プログラムの Go コンパイルエラー**になる。レジストリ経由にすると実行時エラーに落ちる。

---

## 5. 先行研究: luajit-remake / Deegen

[luajit-remake](https://github.com/luajit-remake/luajit-remake) は Haoran Xu による LuaJIT の再実装で、その中核が **Deegen**（[論文](https://arxiv.org/html/2411.11469)、[ブログ 2022-11](https://sillycross.github.io/2022/11/22/2022-11-22/)、[2023-05](https://sillycross.github.io/2023/05/12/2023-05-12/)）というメタコンパイラである。バイトコードの意味論を 1 度書くと、インタプリタとベースライン JIT の両方を自動生成する。インタプリタは LuaJIT のそれより 31〜34% 速い。

### 我々のトランポリンは Deegen が名指しで置き換えているものである

Deegen の中核技術は **musttail dispatch** である。各バイトコードハンドラが次のハンドラを `[[clang::musttail]]` で末尾呼び出しし、機械語では call ではなく jump になる。スタックは伸びず、dispatch ループも Action の構築も要らない。

Deegen はさらに 2 つの制約を LLVM IR レベルで解いている。

1. **呼び出し規約**: 通常の C 規約では callee-saved レジスタの退避・復元が要り、x86-64 で 15 本中 6〜8 本を無駄にする。Deegen は GHC 呼び出し規約（callee-saved 0 本）を当てる。
2. **プロトタイプ一致**: `musttail` は呼び出し元と呼び出し先のシグネチャ一致を要求する。Deegen は全ハンドラのプロトタイプを統一する。

### Go では実現できない

Go に musttail は無く、呼び出し規約も選べない。Go アセンブリで自前の jump dispatch を書く道は GC のスタックマップと衝突する（`Value` ポインタが生存していることを GC に伝える手段が無くなる）。

**この結論は改善の方向を確定させる。1 dispatch あたり約 5ns は Go の ABI が決める固定費であり、下げられない。下げられるのは dispatch の回数だけである。**

### 転用できない技術

- **NaN boxing**: Deegen は採用しているが、Go の GC が許さない（4.1 参照）。Zig でも RC がタグ付きポインタと両立しない。
- **インラインキャッシュ**: Deegen の改善幅の大半はこれによる（dispatch ではなく）。Malgo は静的型付けで、コンストラクタ分岐は既に静的な `if` 連鎖なので、キャッシュする対象が無い。
- **copy-and-patch JIT**: 範囲外。

### Zig バックエンドへの示唆

`docs/zig-backend.md:109-113` は `@call(.always_tail)` を 2 つの理由で却下している。

> `@call(.always_tail)` is not a substitute: Zig requires the callee's signature to match the caller's, which rules out helpers like `applyCovalue(Value, Value)`, and a genuine tail call would release the frame holding the `&[_]rt.Value{...}` argument slice before the callee read it.

**Deegen はこの両方に答えている。** (1) は全ハンドラのプロトタイプを統一することで、(2) は引数をスライスではなくレジスタ渡しの固定パラメータにすることで解消する。`MAX_ARGS` が 2 に確定している（#407）ことも、固定パラメータ化と相性が良い。

Zig バックエンドのトランポリンを丸ごと外せる可能性があるので、#360 を再検討する根拠になる。ただし本調査では未検証である。

### Deegen の前提そのもの

Deegen は「意味論を 1 度書き、全ティアを生成する」というメタコンパイラである。Malgo は同じ問題を抱えている——99 個の primitive が `Eval.lean`・Scheme・Zig・Go の 4 箇所に独立実装され、`primitive-coverage` ゲートが allowlist でその乖離を追跡している。大きな投資だが、問題の形は一致している。

---

## 6. 残る唯一のレバー: dispatch 回数の削減

1 dispatch の単価が固定なら、削れるのは回数だけである。Go は Level 1 で 11.3M dispatch、Zig は 9.0M。この差は join point の扱いの違いに対応する。

現在の Go emitter は `Statement.join` を無条件に Go のクロージャにする（`Go.lean:279-285`）。Zig は `classifyJoins` で `Local`/`Escaping` に分け、`Local` なら束縛ごと消して使用箇所に本体を展開する（`ClosureConv.lean:344-350`）。

コーパス全体（`.golden/Malgo.Debug.PrettyIR/*/golden` の Join 段と ClosureConv 段の差分から集計、106 プログラム）:

| | 個数 | 割合 |
|---|---|---|
| ClosureConv に到達する `Statement.join` | 6933 | 100% |
| `Local` | 2437 | **35.2%** |
| `Escaping` | 4496 | 64.8% |

binder の種別ごとの偏り:

| 種別 | 全体 | Local | Local 率 |
|---|---|---|---|
| `select$` | 558 | 558 | **100%** |
| `apply$` | 2961 | 1574 | 53% |
| `project$` | 124 | 60 | 48% |
| `then$` | 1651 | 245 | 15% |
| `return$` | 1585 | 0 | 0% |

`select$` はパターンマッチごとに 1 個付き、その名前は `cut` の consumer 位置（唯一の非 escape 位置）にしか置かれないので必ず `Local` になる。`return$` は `Flat.do'` の binder で、呼び出し先に渡すために存在するので必ず escape する。

**`classifyJoins`（`ClosureConv.lean:40-254`）は `Malgo.Sequent.Core.Join` と `Std.TreeMap`/`TreeSet` にしか依存しておらず、`Ir.` への最初の参照は 278 行目。そのまま共有モジュールへ移せる。**

**重複展開は起きない。** `Local` な join 2437 個すべて使用箇所がちょうど 1 つである。`tellJoin`（`Join.lean:178-182`）が名前を作った直後に 1 箇所だけへ渡す構造による。

### 実装後の実測

`classifyJoins` を `Malgo.Sequent.Core.Escape` へ移し、Go emitter に `Local` join のインライン経路を入れた結果。

| | 変更前 | 変更後 | Zig |
|---|---|---|---|
| Level 1 wall clock | 0.41s | **0.30s** | 0.23〜0.27s |
| Level 1 dispatches | 11,275,440 | **9,028,448** | 9,028,449 |
| fib-deep dispatches | 23,307,383 | **18,815,850** | 18,815,851 |
| 生成 Go ソース | 13.0 MB | **11.9 MB** | — |

**dispatch 回数が Zig とほぼ完全に一致した**（どちらも差 1）。同じ畳み込みが同じだけ効いていることの裏付けになる。

生成ソースが**縮んだ**ことは、`Local` join の使用箇所が 1 つだという前提の実証でもある。インライン展開で増える分より、クロージャの定型コード（`var m_x Value` / `m_x = Fn(func(args []Value) Action {` / `})` / `_ = m_x`）が消える分のほうが大きい。

文字列変更と合わせて **0.41s → 0.30s（-27%）**。当初の推定 22% を上回った。残る Zig との差は 1 dispatch あたりの単価で、Go の ABI が決めるため下げられない。

---

## 7. まとめ

| 案 | 実測 | 判定 |
|---|---|---|
| `Str` にスカラのみキャッシュ | 0.41 → 0.38s | **採用**（8%） |
| Local join のインライン化 | dispatch -19%、合計で 0.41 → 0.30s | **採用**（-27%、Zig と dispatch 同数） |
| `mkStruct` の可変長引数除去 | 0.41 → 0.40s | 見送り（誤差） |
| トランポリンの形の変更 | 全体の 2.7% | 見送り |
| generics | 算術が +44% | 見送り |
| interface boxing の除去 | 割り当ての 2%、dispatch は不変 | 見送り |
| `[]rune` キャッシュ | 0.41 → 0.78s | 却下（逆効果） |
| リフレクション | 手書きの 4.5 倍 | 却下 |
| musttail dispatch | Go に手段が無い | 実現不可 |
