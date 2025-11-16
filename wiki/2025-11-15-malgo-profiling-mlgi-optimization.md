# Malgoコンパイラ プロファイリング結果と.mlgi最適化提案

**日付**: 2025年11月15日
**目的**: メモリプロファイリングによる主要ボトルネックの特定と改善提案

## 1. プロファイリング実行サマリー

### 1.1 実行環境

- **コンパイラ**: GHC 9.12.2
- **最適化レベル**: `-O2`
- **プロファイリング**: `--enable-profiling`, `-p`, `-s`
- **テストプログラム**: `List.mlg`, `LazyList.mlg`, `Tarai.mlg`

### 1.2 実行結果

#### List.mlg (40行、リスト操作)

```
総アロケーション: 545,105,984 bytes (545 MB)
GCコピー:         411,102,760 bytes (411 MB)
最大residency:    108,968,824 bytes (109 MB)
総メモリ使用:     282 MiB

実行時間:
  MUT time:  0.202s (実際の計算)
  GC time:   0.217s (ガベージコレクション)
  Total:     0.429s

Productivity: 47.2% (理想は70%以上)
```

**問題点**: GC時間が実行時間の50%以上を占め、Productivityが50%未満と非常に低い。

#### LazyList.mlg (31行、遅延リスト)

```
総アロケーション: 535,325,152 bytes (535 MB)
GCコピー:         409,186,296 bytes (409 MB)
最大residency:    108,966,104 bytes (109 MB)
総メモリ使用:     282 MiB

実行時間:
  MUT time:  0.167s
  GC time:   0.207s
  Total:     0.384s

Productivity: 43.4% (さらに低下)
```

**問題点**: List.mlgと同様のパターン。遅延評価でもGC圧力が高い。

## 2. 主要ボトルネックの特定

### 2.1 トップコストセンター

プロファイルファイル（`malgo.prof`）から特定された主要なボトルネック：

| コストセンター | モジュール | 時間% | アロケーション% |
|--------------|----------|------|---------------|
| `$w$cpeek` | **Malgo.Module** | **30.6%** | **71.9%** ⚠️ |
| `$fOrdList_$s$ccompare1` | GHC.Classes | **29.0%** | 0.0% |
| `unsafeEff` | Effectful.Internal.Monad | 12.9% | 0.6% |
| `$w$cpeek` | Malgo.Prelude | 12.9% | 15.5% |
| その他 | - | ~14.6% | ~12.0% |

### 2.2 最大のボトルネック: Malgo.Module.$w$cpeek

**影響度**:
- **メモリアロケーションの71.9%を占める** ← 最重要課題
- 実行時間の30.6%を消費

**原因**:
- `.mlgi`（モジュールインターフェース）ファイルのデシリアライズ
- Storeライブラリの`peek`操作が大量のメモリを確保

**影響**:
- コンパイル時にインポートする全モジュールのインターフェースをロードする際に発生
- 依存関係が多いプロジェクトほど悪化

### 2.3 その他の主要ボトルネック

#### リスト比較 (29.0% time)

```haskell
$fOrdList_$s$ccompare1 :: GHC.Classes
```

**原因**:
- 依存関係解決やシンボル検索時のリスト比較
- おそらく `Ord` インスタンスの自動導出による非効率な比較

**改善案**:
- リスト → `Set` や `IntMap` への変更
- カスタム比較関数の実装

#### Effectfulモナドオーバーヘッド (14.5% time)

```haskell
unsafeEff, unsafeEff_ :: Effectful.Internal.Monad
```

**原因**:
- Effectfulエフェクトシステムのランタイムオーバーヘッド
- モナド操作の連鎖

**影響**:
- 改善は困難（アーキテクチャレベルの変更が必要）
- 中程度の優先度

## 3. .mlgiフォーマット詳細調査

### 3.1 現在の実装

#### 使用ライブラリ

```haskell
import Data.Store  -- Storeライブラリ
```

**選定理由**: 高速なバイナリシリアライゼーション

#### データ構造

**Interface型** (`src/Malgo/Interface.hs:24-34`):

```haskell
data Interface = Interface
  { moduleName :: ModuleName
  , infixInfo :: Map PsId (Assoc, Int)      -- 演算子情報
  , dependencies :: Set ModuleName           -- 依存モジュール
  , exportedIdentList :: [PsId]             -- エクスポート識別子
  , exportedTypeIdentList :: [PsId]         -- エクスポート型識別子
  }
  deriving stock (Show, Generic)
  deriving anyclass (Store)  -- 自動生成
```

**ArtifactPath型** (`src/Malgo/Module.hs:160-167`):

```haskell
data ArtifactPath = ArtifactPath
  { rawPath :: FilePath         -- 生のパス文字列
  , originPath :: Path Abs File -- ソースファイルの絶対パス
  , relPath :: Path Rel File    -- 相対パス
  , targetPath :: Path Abs File -- .malgo-workの絶対パス
  }
```

**問題点**:
- 同じパス情報が4つの異なる形式で保存される
- 各フィールドが個別にシリアライズ・デシリアライズされる

### 3.2 バイナリフォーマット分析

実際の`.mlgi`ファイルのhexdump:

```
00000000  01 3e 00 00 00 00 00 00  00 2f 00 00 00 55 00 00  |.>......./...U..|
00000010  00 73 00 00 00 65 00 00  00 72 00 00 00 73 00 00  |.s...e...r...s..|
```

**観察結果**:
- 各文字が**4バイト**でエンコードされている（UTF-32相当）
- パス文字列 "/Users/..." が膨大な容量を消費
- ファイルサイズ: 1-3KB（シンプルなモジュールでも）

**非効率性**:
- Text型のエンコーディングオーバーヘッド
- パス情報の重複保存
- Genericによる自動生成の非最適化

### 3.3 デシリアライズプロセス

**ロード処理** (`src/Malgo/Interface.hs:73-86`):

```haskell
loadInterface :: ModuleName -> Eff es Interface
loadInterface modName = do
  interfaces <- get
  case Map.lookup modName interfaces of
    Just interface -> pure interface        -- キャッシュヒット
    Nothing -> do
      modPath <- getModulePath modName
      ViaStore interface <- load modPath ".mlgi"  -- デシリアライズ ⚠️
      modify $ Map.insert modName interface       -- キャッシュ
      pure interface
```

**問題点**:
1. **一括デシリアライズ**: Interfaceの全フィールドを一度にロード
2. **正格評価の欠如**: thunkが蓄積
3. **キャッシュのみ**: ディスク上の永続キャッシュなし

**ボトルネック箇所**:

```haskell
ViaStore interface <- load modPath ".mlgi"
  ↓
fromByteString = decodeEx  -- Store.decodeEx を呼び出し
  ↓
instance Store Interface where  -- Generic自動生成
  peek = ...  -- ここで71.9%のアロケーション発生
```

### 3.4 メモリアロケーションの内訳

プロファイルから推定される主要なアロケーション源：

1. **Text/String** (推定40%):
   - `PsId = Text` の大量アロケーション
   - パス文字列の複製

2. **Map/Set構造** (推定20%):
   - `Map PsId (Assoc, Int)` の内部ノード
   - `Set ModuleName` の要素

3. **Path型** (推定15%):
   - 4つのPath値 × 複数のArtifactPath

4. **リスト構造** (推定10%):
   - `[PsId]` のcons cell
   - 中間リストの生成

## 4. 改善提案（ArtifactPath構造維持版）

### 4.1 フェーズ1: Store Instance最適化 + データ構造改善

**推定効果**: メモリアロケーション25-35%削減
**実装期間**: 2-3日
**リスク**: 低（下位互換性維持）

#### 変更内容

##### 1. Interface構造の最適化

**現在**:
```haskell
data Interface = Interface
  { moduleName :: ModuleName
  , infixInfo :: Map PsId (Assoc, Int)
  , dependencies :: Set ModuleName
  , exportedIdentList :: [PsId]              -- ⚠️ リスト
  , exportedTypeIdentList :: [PsId]          -- ⚠️ リスト
  }
  deriving anyclass (Store)  -- ⚠️ Generic自動生成
```

**改善後**:
```haskell
data Interface = Interface
  { moduleName :: {-# UNPACK #-} !ModuleName
  , infixInfo :: !(Map PsId (Assoc, Int))
  , dependencies :: !(Set ModuleName)
  , exportedIdentList :: !(Vector ShortText)      -- ✓ Vector + ShortText
  , exportedTypeIdentList :: !(Vector ShortText)  -- ✓ Vector + ShortText
  }

instance Store Interface where
  -- 手書き実装（後述）
```

**効果**:
- `Vector`: 連続メモリ配置でキャッシュ効率向上
- `ShortText`: 短い文字列（<64バイト）のメモリ効率向上
- `UNPACK`: フィールドのインライン化
- `!`: 正格評価でthunk削減

##### 2. Store Instance手書き実装

**テンプレート**:

```haskell
instance Store Interface where
  poke (Interface mn ii deps eil etil) = do
    poke mn
    poke ii
    poke deps
    poke eil  -- Vector ShortTextのpoke
    poke etil

  peek = do
    !mn <- peek    -- 正格評価
    !ii <- peek
    !deps <- peek
    !eil <- peek
    !etil <- peek
    pure $! Interface mn ii deps eil etil  -- 最終結果も正格
```

**効果**:
- Generic自動生成のオーバーヘッド削減
- 正格評価（`!`パターン、`$!`）によるthunk削減
- コンパイラ最適化の向上

##### 3. String Interning

**実装**:

```haskell
type InternTable = HashMap Text Int

-- モジュール名を整数IDで管理
data Interface = Interface
  { moduleNameId :: {-# UNPACK #-} !Int  -- インターン済みID
  , ...
  }

-- デシリアライズ時にインターンテーブルで共有
loadWithIntern :: InternTable -> ByteString -> (InternTable, Interface)
```

**効果**:
- 重複するモジュール名を共有メモリで管理
- `dependencies :: Set ModuleName` の重複削減

#### 実装ステップ

1. `PsId` → `ShortText` への型エイリアス追加
2. `Interface` の `exportedIdentList/exportedTypeIdentList` を `Vector ShortText` に変更
3. 手書き `Store Interface` instance 実装
4. `UNPACK` プラグマと正格性注釈追加
5. ベンチマーク実行（`weigh` パッケージ使用）

### 4.2 フェーズ2: 遅延デシリアライズ（オプション）

**推定効果**: さらに15-25%削減
**実装期間**: 3-4日
**リスク**: 中（アクセスパターン分析が必要）

#### アプローチ

**Hot/Cold分離**:

```haskell
data Interface = Interface
  { moduleCore :: !InterfaceCore      -- 常に必要（moduleName, dependencies）
  , infixInfo :: Lazy (Map PsId (Assoc, Int))  -- 遅延ロード
  , exportedLists :: Lazy ExportedLists        -- 遅延ロード
  }

data Lazy a = Lazy (IO a) | Forced !a

instance Store (Lazy a) where
  poke (Forced x) = poke x
  poke (Lazy _) = error "Cannot serialize unevaluated Lazy"

  peek = do
    offset <- getOffset  -- 現在のオフセットを記録
    skip sizeOfA         -- データをスキップ
    pure $ Lazy $ do
      seekTo offset      -- 必要時にシーク
      peek               -- その時点でデシリアライズ
```

**効果**:
- アクセスされないデータのデシリアライズを回避
- メモリフットプリント削減

**トレードオフ**:
- 実装複雑度の増加
- アクセス時のレイテンシ
- すべてのフィールドにアクセスする場合は無効

### 4.3 フェーズ3: FlatBuffers移行（長期）

**推定効果**: 50-70%削減（ゼロコピー）
**実装期間**: 5-7日
**リスク**: 高（大規模な変更、ツール依存）

#### 概要

**FlatBuffers**: Googleのゼロコピーシリアライゼーションライブラリ

**スキーマ例**:

```flatbuffers
namespace Malgo;

table Interface {
  module_name: string;
  infix_info: [InfixEntry];
  dependencies: [string];
  exported_idents: [string];
  exported_type_idents: [string];
}

table InfixEntry {
  name: string;
  assoc: Assoc;
  precedence: int;
}

enum Assoc: byte {
  LeftAssoc,
  RightAssoc,
  NonAssoc
}
```

**利点**:
- **ゼロコピー**: デシリアライズ不要、メモリマップで直接アクセス
- **ランダムアクセス**: 必要なフィールドのみ読み取り
- **前方互換性**: スキーマ進化のサポート

**欠点**:
- FlatBuffersコンパイラ（`flatc`）への依存
- Haskellバインディングの学習コスト
- 既存の`.mlgi`との互換性喪失

**実装例**:

```haskell
import FlatBuffers

loadInterfaceFB :: FilePath -> IO Interface
loadInterfaceFB path = do
  bytes <- BS.readFile path
  let root = getRootAsInterface bytes  -- ゼロコピー！
  pure $ Interface
    { moduleName = interfaceModuleName root
    , infixInfo = buildMap (interfaceInfixInfo root)
    , ...
    }
```

## 5. 推奨実装パス

### 5.1 即座の対応（今週）

1. **ベンチマーク環境の構築**
   ```haskell
   -- test/Bench/Interface.hs
   import Weigh

   main = mainWith $ do
     func "deserialize List.mlgi"
          (ViaStore interface <- load "List" ".mlgi")
          ()
   ```

2. **現状のベースライン測定**
   - メモリアロケーション
   - デシリアライズ時間
   - ファイルサイズ

### 5.2 短期（次2週間）

**フェーズ1を実装**:

1. `Vector ShortText` への移行
2. 手書き `Store Interface` instance
3. `UNPACK` / 正格性アノテーション
4. String interning（時間があれば）

**ベンチマーク**:
- 各変更後に効果を測定
- 後退がないか確認

### 5.3 中期（次1ヶ月）

**効果に応じて判断**:

- フェーズ1で30%以上改善 → 完了
- フェーズ1で改善が20%未満 → フェーズ2検討
- より大きな改善が必要 → フェーズ3の調査開始

### 5.4 長期（次四半期）

**FlatBuffers移行の判断基準**:

- プロジェクト規模が大きくなり、コンパイル時間が問題化
- 他の最適化で限界
- ゼロコピーの利点が開発コストを上回る

## 6. 測定とモニタリング

### 6.1 ベンチマーク指標

**追跡すべきメトリクス**:

| 指標 | 現在値 | 目標値 | 測定方法 |
|-----|-------|-------|---------|
| デシリアライズアロケーション | 71.9% | <40% | `+RTS -p` |
| 総メモリアロケーション | 545 MB | <350 MB | `+RTS -s` |
| Productivity | 47.2% | >65% | `+RTS -s` |
| .mlgiファイルサイズ | 1-3 KB | <1 KB | `ls -lh` |
| デシリアライズ時間 | - | <10ms/file | `criterion` |

### 6.2 継続的モニタリング

**CI/CDへの統合**:

```yaml
# .github/workflows/benchmark.yml
- name: Run memory benchmarks
  run: |
    cabal run malgo -- eval examples/malgo/List.mlg +RTS -s
    # 結果をartifactsとして保存
    # リグレッション検出
```

## 7. リスク分析

| リスク | 影響 | 確率 | 軽減策 |
|-------|-----|------|-------|
| フェーズ1の効果が限定的 | 中 | 低 | ベンチマークで早期検証 |
| .mlgi互換性の破壊 | 高 | 中 | バージョニング導入 |
| FlatBuffers学習コスト | 中 | 高 | PoC先行、段階的移行 |
| パフォーマンス後退 | 高 | 低 | 各変更後にベンチマーク |

## 8. まとめ

### 8.1 主要な発見

1. **最大のボトルネック**: `Malgo.Module.$w$cpeek`（71.9%アロケーション）
2. **根本原因**: Store/Genericの非効率なデシリアライズ + データ構造の冗長性
3. **Quick Win**: フェーズ1で25-35%の改善が期待可能

### 8.2 推奨アクション

**今すぐ**: ベンチマーク環境構築
**今週**: フェーズ1の実装開始
**来週**: ベンチマーク結果に基づく判断

### 8.3 長期ビジョン

効率的なモジュールシステムにより：
- **コンパイル時間の短縮**: 大規模プロジェクトでの効果
- **メモリ使用量の削減**: リソース制約環境での利用
- **開発体験の向上**: より高速なフィードバックループ

---

## 参考資料

- プロファイルファイル: `malgo.prof` (8MB)
- テストプログラム: `examples/malgo/List.mlg`, `LazyList.mlg`, `Tarai.mlg`
- 関連コード:
  - `src/Malgo/Module.hs`: ArtifactPath, Store instances
  - `src/Malgo/Interface.hs`: Interface型, loadInterface
  - `src/Malgo/Rename/Pass.hs`: Interface生成
