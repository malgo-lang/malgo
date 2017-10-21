## 文字列定数の宣言
MIRの段階で、出て来る定数文字列をすべてDEFに持ち上げる
グローバル変数とローカル変数の区別をする
CodegenStateに、execCodegenが走るときのグローバル宣言の識別子と型のマップ(globaltab)を追加
ローカル変数はsymtab,グローバル変数はglobaltabで管理

## llvm-hsへの依存の解消
llvmをビルドする必要がある->つらい!!
Text.PrettyPrintでなんとかなりそうなのでなんとかする

## rewrite

* 文法の修正(連続した式など)
* 中間表現の修正(Syntax, Typed, KNormal, ANormalに整理)
* LLVM出力ユーティリティの作成

### 文法と意味

```
extern println(str:String) = call "println" str
extern print(str:String) = call "print" str

def hello:String = "Hello, world!"

def f(a:Int):Int = a * 7

def main():Int = {
    println(hello)
    print("The answer is")
    let answer:Int = f(6)
    0
}
```

defは変数、関数宣言。

文字列リテラルはグローバル変数に持ち上げられる

関数呼び出しは<func>(<arg1>,<arg2>,...,<argn>)

式の区切りは;か改行

```bnf
;; *は0回以上の繰り返し +は1回以上の繰り返し ?は0か1回

<toplevel> := <decl>*

<decl> := <var_def>
        | <func_def>

<var_def> := "def" <ident> ":" <type> "=" <const_expr>

<func_def> := "def" <ident> "(" (<ident> ":" <type>)* ")" ":" <type> "=" <expr>

<const_expr> := <nat>
              | -<nat>
              | <float>
              | <char>
              | <string>
              | <bool>
              | <unit>
              | <const_expr> <op> <const_expr>

<expr> := <const_expr>
        | <expr> <op> <expr>
        | <ident> "(" (<expr> ",")* "," <expr>?")"
        | "(" <expr> ")"
        | "if" <expr> <block> else <block>
        | "let" (<ident> ":" <type> "=" <expr>)+ <block>
        | <block>

<block> := "{" [<expr> <term>]+ [expr]? "}"

<term> = "\n" | ";"

<op> :=  "*" | "/" | "%" | "+" | "-" | "<" | ">" | "==" | "!=" | "<=" | ">=" | "&&" | "||"

<ident> := [a-zA-Z_][a-zA-Z_0-9]*
<type> := [A-Z][a-zA-Z_0-9]*
<nat> := [0-9]+
<float> := [0-9]+ "." [0-9]+
<char> := "'" 何らかの文字 "'"
<string> := "\"" 何らかの文字* "\""
<bool> := "#t" | "#f"
<unit> := "(" ")"
;; 結合順序や向きは適当に
```

### 中間表現(Syntax, Typed, KNormal, ANormal, Canonical)

#### Syntax
パーサが吐くAST。もろもろの意味検査が行われたのちTypedに変換される。
ソースコードの位置情報を持つ。

```haskell
data Info -- ソースコードの位置情報

data Name = Sym String
          | Dummy

data Decl = DefVar Info Name Type Const
          | DefFun Info Name Type [(Name, Type)] Expr
          | ExVar Info Name Type -- 外部変数
          | ExFun Info Name Type [(Name, Type)] -- 外部関数

data Const = Int Info Integer
           | Float Info Double
           | Bool Info Bool
           | Char Info Char
           | String Info String
           | Unit Info
           | CBinOp Info Op Const Const

data Expr = Var Info Name
          | Const Const
          | Call Info Name [Expr]
          | Seq Info Expr Expr
          | Let Info Name Type Expr Expr
          | If Info Expr Expr Expr
          | BinOp Info Op Expr Expr

data Op -- 中間演算子の種類を表すタグ

data Type -- データ型
```

#### Typed
型付きAST。意味検査をパスしたことが保証されている。

```haskell
data Decl = DefVar Name Type Const
          | DefFun Name Type [(Name, Type)] Expr
          | ExVar Name Type -- 外部変数
          | ExFun Name Type [(Name, Type)] -- 外部関数

data Const = Int Integer
           | Float Double
           | Bool Bool
           | Char Char
           | String String
           | Unit
           | CBinOp Op Const Const

-- 第一引数のTypeが式の型を表す
data Expr = Var Type Name
          | Const Type Const
          | Call Type Name [Expr]
          | Seq Type Expr Expr
          | Let Type Name Type Expr Expr
          | If Type Expr Expr Expr
          | BinOp Type Op Expr Expr
```

#### KNormal
K正規化さたIR。二項演算や関数呼び出し、ifの条件部の引数が変数である事が保証されている。
式の連続(Sq)がダミー変数を使ったlet式に変換されている。
α変換やβ簡約などが行われる。

```haskell
-- α変換によって連番が振られる
-- 0で初期化
data Id = Id Int Name
        | Dummy

data Decl = DefVar Id Type Const
          | DefFun Id Type [(Id, Type)] Expr
          | ExVar Id Type -- 外部変数
          | ExFun Id Type [(Id, Type)] -- 外部関数

data Const = Int Integer
           | Float Double
           | Bool Bool
           | Char Char
           | String String
           | Unit

-- 第一引数のTypeが式の型を表す
data Expr = Var Type Id
          | Const Type Const
          | Call Type [Type] Id [Id] -- 第二引数が引数の型を表す
          | Let Type Id Type Expr Expr
          | If Type Id Expr Expr
          | BinOp Type (Type, Type) Op Id Id -- 第二引数が引数の型を表す
```

#### ANormal
A正規化されたIR。
letの代入値を表す式に以下のリストの要素が含まれていないことが保証されている。

* let
* 変数
* if

文と式の区別がある。

let、if、doは文。

* let: 変数宣言
* if: 条件分岐。分岐先は文のリストで保持する
* do: 返り値を利用しない関数呼び出し

ifには返り値を代入する変数の情報が付加されている。

文字列リテラルを与えられたローカル変数宣言をグローバル定数宣言に持ち上げる

```haskell
data Decl = DefVar Id Type Const
          | DefFun Id Type [(Id, Type)] [Stmt]
          | ExVar Id Type -- 外部変数
          | ExFun Id Type [(Id, Type)] -- 外部関数

data Const = Int Integer
           | Float Double
           | Bool Bool
           | Char Char
           | String String
           | Unit

-- 第一引数のTypeが式の型を表す
data Expr = Const Type Const
          | Call Type [Type] Id [Id] -- 第二引数が引数の型を表す
          | BinOp Type (Type, Type) Op Id Id -- 第二引数が引数の型を表す

data Stmt = Let Id Type Expr
          | If Id Type Id Expr Expr -- 第一引数は返り値が代入される変数名を表す
          | Do Expr
```

#### Canonical
Basic blockとphi関数により条件分岐を再構成したIR。
ifがジャンプ命令に変換されている。

phi関数の挿入を行うことで、変数の再定義がされていないことを保証する。

```haskell
data Decl = DefVar Id Type Const
          | DefFun Id Type [(Id, Type)] Id [BasicBlock] -- 第四引数は最初に実行されるbasic blockの名前
          | ExVar Id Type -- 外部変数
          | ExFun Id Type [(Id, Type)] -- 外部関数

data Const = Int Integer
           | Float Double
           | Bool Bool
           | Char Char
           | String String
           | Unit

data Instr = Const Const
           | Call Type [Type] Id [Id] -- 第二引数が引数の型を表す
           | BinOp Type (Type, Type) Op Id Id -- 第二引数が引数の型を表す
           | Phi Type [(Id, Id)] -- 第二引数のfstは変数、sndは直前のbasic block名

data Named a = Id := a
             | Do a

data Term = Ret Id
          | CJump Id Id Id -- 条件値、真のときの飛び先、偽のときの飛び先
          | Jump Id -- 飛び先

data BasicBlock = BasicBlock Id [Named Instr] (Named Term)
```

#### コンパイルパス

* パーサ -> Syntaxを生成
* 意味検査 -> Syntaxを検査
* 型検査 -> Exprに型情報を付加。Typedを生成
* 定数評価 -> 定数畳み込みを行う
* Seq2Let変換 -> SeqをLetに変換する
* k正規化 -> KNormalを生成
* α変換 -> Idにα変換のアルゴリズムで連番を降る
* β簡約
* a正規化 -> KNormalの入れ子になったletを解消し、ifの変換を行いながらANormalを生成
* 文字列リテラルの正規化 -> 文字列リテラルの変数宣言をグローバル定数宣言に持ち上げる
* 正準化 -> 関数の本体をbasic blockのリストに変換する。ifの返り値となる変数名と飛び先の名前のマップも返す
* phi挿入 -> 正準化の工程で手に入るマップを使い、phi関数を挿入してSSAに変換する
* LLVM出力 -> なんとかする
* アセンブル -> 最後は気合
