package ast

import (
	mapset "github.com/deckarep/golang-set/v2"
)

func FreeVariable(i Info, expr Expr) mapset.Set[Ident] {
	switch expr := expr.(type) {
	case Variable:
		return mapset.NewSetFromMapKeys[Ident](map[Ident]interface{}{expr.Ident: nil})
	case Label:
		return mapset.NewSet[Ident]()
	case Literal:
		return mapset.NewSet[Ident]()
	case Apply:
		fvs := FreeVariable(i, expr.Func)
		for _, arg := range expr.Args {
			fvs = fvs.Union(FreeVariable(i, arg))
		}
		return fvs
	case Codata:
		fvs := mapset.NewSet[Ident]()
		for _, clause := range expr.Clauses {
			fvs = fvs.Union(FreeVariable(i, clause.Body))
			fvs = fvs.Difference(FreeVariable(i, clause.Pattern))
		}
		return fvs
	case This:
		return mapset.NewSet[Ident]()
	case Object:
		fvs := mapset.NewSet[Ident]()
		for _, field := range expr.Fields {
			fvs = fvs.Union(FreeVariable(i, field))
		}
		return fvs
	case LambdaCase:
		fvs := mapset.NewSet[Ident]()
		for _, clause := range expr.Cases {
			fvs = fvs.Union(FreeVariable(i, clause.Body))
			fvs = fvs.Difference(FreeVariable(i, clause.Pattern))
		}
		for _, param := range expr.Parameters {
			fvs.Remove(param)
		}
		return fvs
	case Lambda:
		fvs := FreeVariable(i, expr.Body)
		for _, param := range expr.Parameters {
			fvs.Remove(param)
		}
		return fvs
	case Switch:
		fvs := FreeVariable(i, expr.Target)
		for _, body := range expr.Cases {
			fvs = fvs.Union(FreeVariable(i, body))
		}
		return fvs
	case Select:
		fvs := FreeVariable(i, expr.Target)
		fvs = fvs.Union(FreeVariable(i, expr.Body))
		fvs.Remove(expr.Bind)
		return fvs
	default:
		panic(NewNotExprError(i.Input, expr))
	}
}
