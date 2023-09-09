package rename

import (
	"fmt"
	"log"

	"github.com/takoeight0821/malgo/internal/ast"
)

// Check all variables are bound and replace them with RnID.
func Rename(expr ast.Expr) ast.Expr {
	return newRenamer().renameExpr(rnEnv{}, expr)
}

// Unique identifier.
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
	newEnv, pattern := r.renamePattern(clause.Pattern)
	for k, v := range env {
		newEnv[k] = v
	}
	body := r.renameExpr(newEnv, clause.Body)
	return ast.NewClause(pattern, body)
}

// Return new environment and renamed pattern.
// Resulting environment does not contain any bindings for variables in the given environment,
// so caller has to merge it.
func (r *renamer) renamePattern(pattern ast.Pattern) (rnEnv, ast.Pattern) {
	if !pattern.IsPattern() {
		log.Panicf("[error] rename.renamePattern: %T is not a pattern", pattern)
	}

	switch pattern := pattern.(type) {
	case ast.Variable:
		newName := r.newName(pattern.Ident)
		newEnv := rnEnv{pattern.Ident: newName}
		return newEnv, ast.NewVariable(newEnv[pattern.Ident], pattern.Pos())
	case ast.Apply:
		newEnv := rnEnv{}
		newArgs := []ast.Node{}
		for _, arg := range pattern.Args {
			argEnv, newArg := r.renamePattern(arg.(ast.Pattern))
			newArgs = append(newArgs, newArg)
			for k, v := range argEnv {
				if _, ok := newEnv[k]; ok {
					panic(fmt.Sprintf("at %d: %s is already bound", pattern.Pos(), k.Name()))
				}
				newEnv[k] = v
			}
		}
		funEnv, fun := r.renamePattern(pattern.Func.(ast.Pattern))
		for k, v := range funEnv {
			newEnv[k] = v
		}
		return newEnv, ast.NewApply(fun, newArgs)
	default:
		return rnEnv{}, pattern
	}
}
