package rename

import (
	"fmt"
	"log"

	"github.com/takoeight0821/malgo/internal/ast"
)

type RnID struct {
	RawName string
	Unique  int
}

func (id RnID) Name() string {
	return id.RawName + "_" + fmt.Sprint(id.Unique)
}

type renamer struct {
	names map[string]int
}

type rnEnv map[ast.Ident]RnID

func newRenamer() *renamer {
	return &renamer{
		names: map[string]int{},
	}
}

func (r *renamer) newName(name ast.Ident) RnID {
	if _, ok := r.names[name.Name()]; !ok {
		r.names[name.Name()] = 0
		return RnID{RawName: name.Name(), Unique: 0}
	}

	r.names[name.Name()]++
	return RnID{RawName: name.Name(), Unique: r.names[name.Name()]}
}

func appendEnv(env rnEnv, name ast.Ident, rn RnID) rnEnv {
	newEnv := rnEnv{}
	for k, v := range env {
		newEnv[k] = v
	}
	newEnv[name] = rn
	return newEnv
}

func Rename(expr ast.Expr) ast.Expr {
	renamer := newRenamer()
	return renamer.renameExpr(rnEnv{}, expr)
}

func (r *renamer) renameExpr(env rnEnv, expr ast.Expr) ast.Expr {
	if !expr.IsExpr() {
		log.Panicf("[error] rename.renameExpr: %T is not an expression", expr)
	}

	switch expr := expr.(type) {
	case ast.Variable:
		if _, ok := env[expr.Ident]; !ok {
			panic("unbound variable: " + expr.Ident.Name())
		}

		return ast.NewVariable(env[expr.Ident], expr.Pos())
	case ast.Apply:
		newArgs := []ast.Node{}
		for _, arg := range expr.Args {
			newArgs = append(newArgs, r.renameExpr(env, arg))
		}

		return ast.NewApply(r.renameExpr(env, expr.Func), newArgs)
	case ast.Codata:
		newClauses := []ast.Clause{}
		for _, clause := range expr.Clauses {
			newClauses = append(newClauses, r.renameClause(env, clause))
		}

		return ast.NewCodata(newClauses, expr.Pos())
	default:
		return expr
	}
}

func (r *renamer) renameClause(env rnEnv, clause ast.Clause) ast.Clause {
	newEnv, pattern := r.renamePattern(env, clause.Pattern)
	body := r.renameExpr(newEnv, clause.Body)
	return ast.NewClause(pattern, body)
}

func (r *renamer) renamePattern(env rnEnv, pattern ast.Pattern) (rnEnv, ast.Pattern) {
	if !pattern.IsPattern() {
		log.Panicf("[error] rename.renamePattern: %T is not a pattern", pattern)
	}

	switch pattern := pattern.(type) {
	case ast.Variable:
		newName := r.newName(pattern.Ident)
		newEnv := appendEnv(env, pattern.Ident, newName)
		return newEnv, ast.NewVariable(newEnv[pattern.Ident], pattern.Pos())
	case ast.Apply:
		newArgs := []ast.Node{}
		newEnv := env
		for _, arg := range pattern.Args {
			var newArg ast.Node
			newEnv, newArg = r.renamePattern(newEnv, arg.(ast.Pattern))
			newArgs = append(newArgs, newArg)
		}
		newEnv, fun := r.renamePattern(newEnv, pattern.Func.(ast.Pattern))
		return newEnv, ast.NewApply(fun, newArgs)
	default:
		return env, pattern
	}
}
