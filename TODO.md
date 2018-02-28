# TODO

## 定数畳み込み
BinOpに対する定数畳み込みとQuickCheckによるテストを実装

## 任意精度算術演算
GMPを使った任意精度算術演算を組み込みで提供

## アトム

英大文字か':'から始まる識別子をアトムとして扱う。
型名とは別の名前空間で管理される。

```
let val name : {Atom, Atom} = {Yuya, Kono}
    fun print_name(n : {Atom, Atom}) : Unit = print_atom(n.0); print_atom(n.1)
in print_name(name)
end
```

## レコード

```
let val person : {name : String, age : Int} = { name = "Yuya Kono", age = 19 }
    fun print_person(p : {name : String, age : Int}) : Unit = print_string(p.name); print_int(p.age)
in print_person(person)
end
```

## ヴァリアント

```
let val day = {}

```
