package rename

import (
	"fmt"

	"github.com/takoeight0821/malgo/internal/ast"
)

type RnID struct {
	RawName string
	Unique  int
}

func (id RnID) Name() string {
	return id.RawName + "_" + fmt.Sprint(id.Unique)
}

type RnEnv map[ast.Ident]RnID

func Rename(expr ast.Expr) ast.Expr {
	return renameExpr(RnEnv{}, expr)
}

func renameExpr(env RnEnv, expr ast.Expr) ast.Expr {
	switch expr := expr.(type) {
	case ast.Variable:
		if _, ok := env[expr.Ident]; !ok {
			return expr
		}

		return ast.NewVariable(env[expr.Ident], expr.Pos())
	case ast.Apply:
		newArgs := []ast.Node{}
		for _, arg := range expr.Args {
			newArgs = append(newArgs, renameExpr(env, arg))
		}

		return ast.NewApply(renameExpr(env, expr.Func), newArgs)
	case ast.Codata:
		newClauses := []ast.Clause{}
		for _, clause := range expr.Clauses {
			newClauses = append(newClauses, renameClause(env, clause))
		}

		return ast.NewCodata(newClauses, expr.Pos())
	default:
		return expr
	}
}

func renameClause(env RnEnv, clause ast.Clause) ast.Clause {
	newEnv, pattern := renamePattern(env, clause.Pattern)
	body := renameExpr(newEnv, clause.Body)
	return ast.NewClause(pattern, body)
}

func renamePattern(env RnEnv, pattern ast.Pattern) (RnEnv, ast.Pattern) {
	switch pattern := pattern.(type) {
	case ast.Variable:
		unique := 0
		if _, ok := env[pattern.Ident]; ok {
			unique = env[pattern.Ident].Unique + 1
		}
		env[pattern.Ident] = RnID{RawName: pattern.Ident.Name(), Unique: unique}
		return env, ast.NewVariable(ast.String(env[pattern.Ident].Name()), pattern.Pos())
	case ast.Apply:
		newArgs := []ast.Node{}
		for _, arg := range pattern.Args {
			newEnv, newArg := renamePattern(env, arg.(ast.Pattern))
			newArgs = append(newArgs, newArg)
			env = newEnv
		}
		return env, ast.NewApply(renameExpr(env, pattern.Func), newArgs)
	default:
		return env, pattern
	}
}
