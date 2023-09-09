package ast

import (
	mapset "github.com/deckarep/golang-set/v2"
)

func FreeVariable(expr Expr) mapset.Set[Ident] {
	switch expr := expr.(type) {
	case Variable:
		return mapset.NewSetFromMapKeys[Ident](map[Ident]interface{}{expr.Ident: nil})
	case Apply:
		fvs := FreeVariable(expr.Func)
		for _, arg := range expr.Args {
			fvs = fvs.Union(FreeVariable(arg))
		}

		return fvs
	case Codata:
		fvs := mapset.NewSet[Ident]()
		for _, clause := range expr.Clauses {
			fvs = fvs.Union(FreeVariable(clause.Body))
			fvs = fvs.Difference(FreeVariable(clause.Pattern))
		}

		return fvs
	default:
		return mapset.NewSet[Ident]()
	}
}
