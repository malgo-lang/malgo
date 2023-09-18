package alpha

import (
	"github.com/takoeight0821/malgo/internal/ast"
)

type Alpha struct {
	*ast.Info
	subst map[ast.Ident]ast.Node
}

// Rename all non-free variables in expr.
// And apply substitution for free variables.
func Convert(info *ast.Info, expr ast.Node, subst map[ast.Ident]ast.Node) ast.Node {
	alpha := &Alpha{
		info,
		subst,
	}
	return alpha.convert(expr)
}

func (a *Alpha) convert(expr ast.Node) ast.Node {
	switch expr := expr.(type) {
	case ast.Variable:
		if newVar, ok := a.subst[expr.Ident]; ok {
			return newVar
		}
		return expr
	case ast.Label:
		return expr
	case ast.Literal:
		return expr
	case ast.Apply:
		newArgs := make([]ast.Node, len(expr.Args))
		for i, arg := range expr.Args {
			newArgs[i] = a.convert(arg)
		}
		return ast.NewApply(a.convert(expr.Func), newArgs)
	case ast.Codata:
		newClauses := make([]ast.Clause, len(expr.Clauses))
		for i, clause := range expr.Clauses {
			newClauses[i] = a.convert(clause).(ast.Clause)
		}
		return ast.NewCodata(newClauses, expr.Pos())
	case ast.Clause:
		newPattern := a.bind(expr.Pattern)
		return ast.NewClause(newPattern, a.convert(expr.Body).(ast.Expr))
	case ast.This:
		return expr
	case ast.Object:
		newFields := make(map[ast.Ident]ast.Expr)
		for k, v := range expr.Fields {
			newFields[k] = a.convert(v).(ast.Expr)
		}
		return ast.NewObject(newFields, expr.Pos())
	case ast.LambdaCase:
		newParameters := make([]ast.Ident, len(expr.Parameters))
		for i, param := range expr.Parameters {
			newParameters[i] = a.NewName(param)
		}
		for i, param := range expr.Parameters {
			a.subst[param] = ast.NewVariable(newParameters[i], expr.Pos())
		}
		newCases := make([]ast.Clause, len(expr.Cases))
		for i, c := range expr.Cases {
			newPattern := a.bind(c.Pattern)
			newCases[i] = ast.NewClause(newPattern, a.convert(c.Body).(ast.Expr))
		}
		return ast.NewLambdaCase(newParameters, newCases)
	case ast.Lambda:
		newParameters := make([]ast.Ident, len(expr.Parameters))
		for i, param := range expr.Parameters {
			newParameters[i] = a.NewName(param)
		}
		for i, param := range expr.Parameters {
			a.subst[param] = ast.NewVariable(newParameters[i], expr.Pos())
		}
		newBody := a.convert(expr.Body).(ast.Expr)
		return ast.NewLambda(newParameters, newBody)
	case ast.Switch:
		newTarget := a.convert(expr.Target).(ast.Expr)
		newCases := make(map[ast.Ident]ast.Expr)
		for i, c := range expr.Cases {
			newCases[i] = a.convert(c).(ast.Expr)
		}
		return ast.NewSwitch(newTarget, newCases)
	case ast.Select:
		newTarget := a.convert(expr.Target).(ast.Expr)
		newBind := a.NewName(expr.Bind)
		a.subst[expr.Bind] = ast.NewVariable(newBind, expr.Pos())
		newBody := a.convert(expr.Body).(ast.Expr)
		return ast.NewSelect(newTarget, expr.Index, newBind, newBody)
	}
	panic(ast.NewNotExprError(a.Input, expr))
}

// Rename all free variables in pattern and bind them.
func (a *Alpha) bind(pattern ast.Pattern) ast.Pattern {
	panic("unimplemented")
}
