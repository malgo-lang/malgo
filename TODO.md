# TODO

1. Lexer, Parser, Syntaxが扱う文字列をTextに変更

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
