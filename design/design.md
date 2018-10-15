# Malgo

## 字句

### キーワード

type if then else let in rec true false
forall

### 演算子

    + - * / % +. -. *. /. == /= < > <= >= & |

## 構文

### 宣言

f : T : 型宣言

f x = e : 関数宣言

type T a = T1 a : 型の別名宣言

### 式

42, 3.14, 'c', "str", true : 定数

e1 op e2 : 中置演算子

if e1 then e2 else e3 : 条件分岐

let x = e1 in e2 : 変数宣言

let rec f x = e1 in e2 : 関数宣言

e1 e2 : 関数呼び出し

(e1, e2, ..., en) : タプル生成

e.1 : タプルアクセス

## 型システム

### 型

Int, Double, Char, Bool, ... : プリミティブ型

(T1, T2, ..., Tn) : タプル

T1 -> T2 : 関数型

Array T : 配列型

a : 型変数

### 型スキーム

forall {型変数}. 型

## 組み込み型

Int : 64bit符号付き整数
Double : 64bit符号付き整数
Char : 文字型
String : 文字列型
Bool : 真偽値型

## 組み込み関数

* {int, double, char, bool}ToString : 文字列変換
* print, println : 文字列出力
* getChar : 文字入力
* getLine : 文字列入力（一行）
* getContents : 文字列入力（EOFまで）
* newArray : 固定長配列作成
* readArray : 配列アクセス
* writeArray : 配列書き込み

## TODO

* data宣言(代数的データ型)
* パターンマッチ
* レコード型
