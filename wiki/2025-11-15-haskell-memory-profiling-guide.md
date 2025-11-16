# Haskellメモリプロファイリングガイド

Malgoコンパイラのメモリ使用量を分析するための実践的なガイド

## 基本的な手順

### 1. プロファイリングの有効化

プロジェクトルートに `cabal.project.local` を作成（または編集）:

```
profiling: True
profiling-detail: late-toplevel
```

または、コマンドラインで:

```bash
cabal configure --enable-profiling
```

### 2. プロジェクトのビルド

```bash
cabal clean
cabal build --enable-profiling
```

### 3. プロファイリング実行

```bash
# 基本的なメモリ統計
cabal run malgo -- eval examples/malgo/Hello.mlg +RTS -s -RTS

# ヒーププロファイル（コストセンター別）
cabal run malgo -- eval examples/malgo/Hello.mlg +RTS -hc -l -RTS

# ヒーププロファイル（型別）
cabal run malgo -- eval examples/malgo/Hello.mlg +RTS -hy -l -RTS

# ヒーププロファイル（モジュール別）
cabal run malgo -- eval examples/malgo/Hello.mlg +RTS -hm -l -RTS
```

## 主要なRTSオプション

### メモリ統計オプション

- **`+RTS -s`**: 基本的なメモリ統計（総メモリ使用量、GC回数など）
- **`+RTS -S`**: より詳細な統計情報をリアルタイムで出力

### ヒープ プロファイリングオプション

| オプション | 説明 | `-prof`必要 |
|-----------|------|------------|
| `-hc` | コストセンタースタック別 | ✓ |
| `-hm` | モジュール別 | ✓ |
| `-hd` | クロージャ記述別 | ✓ |
| `-hy` | 型別 | ✓ |
| `-hT` | クロージャタイプ別 | ✗ |
| `-hi` | 情報テーブルアドレス別 | `-finfo-table-map` |
| `-he` | エラ（世代）別 | ✓ |

### その他の有用なオプション

- **`-l`**: イベントログを有効化（推奨: eventlog2htmlで可視化）
- **`-i⟨secs⟩`**: サンプリング間隔（デフォルト: 0.1秒）
- **`-p`**: プロファイリングレポートを生成（`.prof`ファイル）
- **`-pj`**: JSONフォーマットのプロファイリングレポート

## プロファイル結果の可視化

### モダンな方法: eventlog2html

1. **インストール**:
```bash
cabal install eventlog2html
```

2. **プロファイル実行**:
```bash
cabal run malgo -- eval examples/malgo/Hello.mlg +RTS -hc -l -RTS
```

3. **可視化**:
```bash
eventlog2html malgo.eventlog
```

ブラウザで `malgo.eventlog.html` を開いて、インタラクティブなヒーププロファイルを確認できます。

### レガシーな方法: hp2ps

1. **実行** (eventlogなし):
```bash
cabal run malgo -- eval examples/malgo/Hello.mlg +RTS -hc -RTS
```

2. **PostScriptに変換**:
```bash
hp2ps -c malgo.hp
```

3. **表示**:
```bash
open malgo.ps  # macOS
```

## 高度なプロファイリング

### リテイナプロファイリング

どのデータ構造がGCを妨げているかを特定:

```bash
cabal run malgo -- eval examples/malgo/Hello.mlg +RTS -hr -l -RTS
```

### バイオグラフィカルプロファイリング

オブジェクトのライフサイクルステージ別（lag, use, drag, void）:

```bash
cabal run malgo -- eval examples/malgo/Hello.mlg +RTS -hb -l -RTS
```

### エラプロファイリング（新機能: GHC 9.2+）

長寿命オブジェクトを特定するため、各クロージャに割り当て時代をマーク:

```bash
cabal run malgo -- eval examples/malgo/Hello.mlg +RTS -he -l --automatic-era-increment -RTS
```

## Malgoプロジェクト向け推奨設定

### `cabal.project.local` の推奨設定

```
-- すべてのパッケージでプロファイリングを有効化
profiling: True
profiling-detail: late-toplevel

-- 依存関係もプロファイリング
package *
  profiling-detail: late-toplevel

-- より詳細なコストセンター（オプション）
package malgo
  ghc-options: -fprof-auto
```

### よく使うコマンド

```bash
# 1. 基本的なメモリ統計を表示
cabal run malgo -- eval examples/malgo/Hello.mlg +RTS -s

# 2. コストセンター別ヒーププロファイル（推奨）
cabal run malgo -- eval examples/malgo/Hello.mlg +RTS -hc -l -i0.01
eventlog2html malgo.eventlog

# 3. 型別ヒーププロファイル
cabal run malgo -- eval examples/malgo/Hello.mlg +RTS -hy -l
eventlog2html malgo.eventlog

# 4. 詳細なプロファイリングレポート
cabal run malgo -- eval examples/malgo/Hello.mlg +RTS -p -RTS
cat malgo.prof
```

## プロファイリング結果の読み方

### `-s` 統計の主要メトリクス

```
  1,234,567,890 bytes allocated in the heap
     12,345,678 bytes copied during GC
      1,234,567 bytes maximum residency (5 sample(s))
         12,345 bytes maximum slop
```

- **allocated**: ヒープに割り当てられた総バイト数
- **copied during GC**: GC中にコピーされたバイト数
- **maximum residency**: 最大メモリ常駐量（⚠️ メモリリークの指標）
- **maximum slop**: 未使用の割り当て済みメモリ

### ヒーププロファイルグラフの読み方

- **X軸**: 時間（秒）
- **Y軸**: メモリ使用量（バイト）
- **色分けされた領域**: 各カテゴリ（コストセンター、型、モジュールなど）

注目すべきパターン:
- **右肩上がりの線**: メモリリークの可能性
- **大きな山**: 一時的な大量メモリ使用
- **高い平坦部**: 常駐メモリの多さ

## トラブルシューティング

### プロファイリングが遅すぎる場合

```bash
# サンプリング間隔を増やす（精度は下がる）
+RTS -hc -i1.0

# コストセンターの詳細度を下げる
# cabal.project.local で profiling-detail: exported-functions
```

### メモリ使用量の差異

プロファイラーが報告するメモリ使用量は、OSが報告する値と異なる場合があります:
- プロファイリングオーバーヘッド（約30%）
- GC要件（通常ライブデータの2-3倍）
- プログラムテキスト、Cスタック
- 外部ライブラリによる非ヒープ割り当て

## 参考資料

- [GHC User's Guide - Profiling](https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html)
- [Cabal User's Guide - How to Enable Profiling](https://cabal.readthedocs.io/en/latest/how-to-enable-profiling.html)
- [eventlog2html](https://mpickering.github.io/eventlog2html/)
- [Well-Typed Blog: Eras Profiling](https://www.well-typed.com/blog/2024/01/ghc-eras-profiling/)
