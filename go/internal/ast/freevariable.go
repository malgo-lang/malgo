package ast

import (
	mapset "github.com/deckarep/golang-set/v2"
)

func FreeVariable(expr Expr) mapset.Set[string] {
	switch e := expr.(type) {
	case Variable:
		return mapset.NewSetFromMapKeys[string](map[string]interface{}{e.Name: nil})
	case Apply:
		fvs := FreeVariable(e.Func)
		for _, arg := range e.Args {
			fvs = fvs.Union(FreeVariable(arg))
		}
		return fvs
	case Codata:
		fvs := mapset.NewSet[string]()
		for _, clause := range e.Clauses {
			fvs = fvs.Union(FreeVariable(clause.Body))
			fvs = fvs.Difference(FreeVariable(clause.Pattern))
		}
		return fvs
	default:
		return mapset.NewSet[string]()
	}
}
