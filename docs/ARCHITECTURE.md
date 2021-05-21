# Architecture

This document describes the archtecture of malgo compiler.

## Overview

![](/docs/malgo_overview.png)

malgoコンパイラは大きく分けて2つのコンポーネントからなる。
1つ目は`malgo/`のmalgo-to-korielコンパイラで、malgoのソースコードから、中間表現であるkorielへの変換を行う。
2つ目は`koriel/`のkoriel-to-llvmコンパイラで、korielからLLVM IRへの変換を行う。

korielは、より低級な言語へのコンパイルを前提に設計された単純な関数型言語である。
以下のような言語機能を持つ。

* 単相型と関数型、Any型、タグ付き共用体型
* 明示的なキャスト
* let式による明示的なヒープアロケーション
* ネストしないパターンマッチ

## Detailed Design

### RIO

malgoのコードはRIOパターンを使って書かれている。

https://www.stackage.org/package/rio

### Uniq

コンパイラでは、ユニークな識別子が欲しくなることがある。
`Koriel.MonadUniq.getUniq`は、呼ばれるたびにユニークな`Int`を返す。

### Interface

### Driver

malgoコンパイラのエントリーポイントは`Language.Malgo.Driver.compile`である。

### Pass
### Rename
### TypeCheck
### UTerm
### Refine
### Desugar
### Optimize
### Flat
### Alpha
### CodeGen
