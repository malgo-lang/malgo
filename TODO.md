## 文字列定数の宣言
MIRの段階で、出て来る定数文字列をすべてDEFに持ち上げる
グローバル変数とローカル変数の区別をする
CodegenStateに、execCodegenが走るときのグローバル宣言の識別子と型のマップ(globaltab)を追加
ローカル変数はsymtab,グローバル変数はglobaltabで管理

## llvm-hsへの依存の解消
llvmをビルドする必要がある->つらい!!
Text.PrettyPrintでなんとかなりそうなのでなんとかする
