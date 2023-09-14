package typecheck

import "github.com/takoeight0821/malgo/internal/ast"

/*
Ideas:
- Do closure conversion before type checking. If we do this, we could skip the problem around the complexity of universal quantification.
	- Ref: 「型推論」特別講義 第9回 (プログラミング言語の基礎理論シリーズ） https://youtu.be/m9TihVqyV3Q?si=TgppSh72w3_Ubkbz
*/

type Type struct{}

// TcEnv stores all type information for each variables.
type TcEnv struct {
	typeMap map[ast.Ident]Type
}
