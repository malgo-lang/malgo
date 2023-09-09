package rename

import (
	"fmt"

	"github.com/takoeight0821/malgo/internal/ast"
)

type RnId struct {
	RawName string
	Unique  int
}

func (id RnId) Name() string {
	return id.RawName + "_" + fmt.Sprint(id.Unique)
}

type RnEnv map[ast.Ident]RnId

func Rename(expr ast.Expr) ast.Expr {
	return renameExpr(RnEnv{}, expr)
}

func renameExpr(env RnEnv, expr ast.Expr) ast.Expr {
	switch e := expr.(type) {
	case ast.Variable:
		if _, ok := env[e.Ident]; !ok {
			return e
		}
		return ast.NewVariable(env[e.Ident], e.Pos())
	case ast.Apply:
		newArgs := []ast.Node{}
		for _, arg := range e.Args {
			newArgs = append(newArgs, renameExpr(env, arg))
		}
		return ast.NewApply(renameExpr(env, e.Func), newArgs)
	case ast.Codata:
		newClauses := []ast.Clause{}
		for _, clause := range e.Clauses {
			newClauses = append(newClauses, renameClause(env, clause))
		}
		return ast.NewCodata(newClauses, e.Pos())
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
	switch p := pattern.(type) {
	case ast.Variable:
		unique := 0
		if _, ok := env[p.Ident]; ok {
			unique = env[p.Ident].Unique + 1
		}
		env[p.Ident] = RnId{RawName: p.Ident.Name(), Unique: unique}
		return env, ast.NewVariable(ast.String(env[p.Ident].Name()), p.Pos())
	case ast.Apply:
		newArgs := []ast.Node{}
		for _, arg := range p.Args {
			newEnv, newArg := renamePattern(env, arg.(ast.Pattern))
			newArgs = append(newArgs, newArg)
			env = newEnv
		}
		return env, ast.NewApply(renameExpr(env, p.Func), newArgs)
	default:
		return env, pattern
	}
}
