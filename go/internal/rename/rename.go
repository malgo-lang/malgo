package rename

import (
	"fmt"

	"github.com/takoeight0821/malgo/internal/ast"
)

// Check all variables are bound and replace them with RnID.
func Rename(info *ast.Info, expr ast.Expr) ast.Expr {
	return newRenamer(info).renameExpr(expr)
}

type renamer struct {
	*ast.Info
	env *rnEnv
}

type rnEnv struct {
	current map[ast.Ident]ast.ID
	parent  *rnEnv
}

func (env rnEnv) String() string {
	str := "{"
	for k, v := range env.current {
		str += fmt.Sprintf("%s -> %s\n", k.Name(), v.Name())
	}
	if env.parent != nil {
		str += env.parent.String()
	}
	str += "}"
	return str
}

func newRnEnv() *rnEnv {
	return &rnEnv{
		current: map[ast.Ident]ast.ID{},
		parent:  nil,
	}
}

func (r *renamer) bind(pos int, ident ast.Ident, renamedIdent ast.ID) {
	if _, ok := r.env.current[ident]; ok {
		err := AlreadyBoundError{input: r.Input, pos: pos, ident: ident}
		panic(err)
	}

	r.env.current[ident] = renamedIdent
}

func (r *renamer) push() {
	r.env = &rnEnv{
		current: map[ast.Ident]ast.ID{},
		parent:  r.env,
	}
}

func (r *renamer) pop() {
	r.env = r.env.parent
}

// Iterates over the environment chain.
func (env rnEnv) lookup(ident ast.Ident) (ast.ID, bool) {
	if id, ok := env.current[ident]; ok {
		return id, true
	}
	return env.parent.lookup(ident)
}

func newRenamer(info *ast.Info) *renamer {
	return &renamer{
		Info: info,
		env:  newRnEnv(),
	}
}

func (r *renamer) renameExpr(expr ast.Expr) ast.Expr {
	switch expr := expr.(type) {
	case ast.Variable:
		if v, ok := r.env.lookup(expr.Ident); ok {
			return ast.NewVariable(v, expr.Pos())
		}

		err := UnbondVariableError{input: r.Input, variable: expr}
		panic(err)
	case ast.Label:
		return expr
	case ast.Literal:
		return expr
	case ast.Apply:
		newArgs := []ast.Node{}
		for _, arg := range expr.Args {
			newArgs = append(newArgs, r.renameExpr(arg))
		}

		return ast.NewApply(r.renameExpr(expr.Func), newArgs)
	case ast.Codata:
		newClauses := []ast.Clause{}
		for _, clause := range expr.Clauses {
			newClauses = append(newClauses, r.renameClause(clause))
		}

		return ast.NewCodata(newClauses, expr.Pos())
	case ast.Object:
		newFields := map[ast.Ident]ast.Expr{}
		for k, v := range expr.Fields {
			newFields[k] = r.renameExpr(v)
		}

		return ast.NewObject(newFields, expr.Pos())
	case ast.LambdaCase:
		newParameters := make([]ast.Ident, len(expr.Parameters))
		for i, param := range expr.Parameters {
			newParameters[i] = r.NewName(param)
		}
		r.push()
		for i, param := range expr.Parameters {
			r.bind(expr.Pos(), param, newParameters[i].(ast.ID))
		}
		newCases := []ast.Clause{}
		for _, c := range expr.Cases {
			newCases = append(newCases, r.renameClause(c))
		}
		r.pop()
		return ast.NewLambdaCase(newParameters, newCases)
	case ast.Lambda:
		newParameters := make([]ast.Ident, len(expr.Parameters))
		for i, param := range expr.Parameters {
			newParameters[i] = r.NewName(param)
		}
		r.push()
		for i, param := range expr.Parameters {
			r.bind(expr.Pos(), param, newParameters[i].(ast.ID))
		}
		newBody := r.renameExpr(expr.Body)
		r.pop()
		return ast.NewLambda(newParameters, newBody)
	case ast.Switch:
		newTarget := r.renameExpr(expr.Target)
		newCases := map[ast.Ident]ast.Expr{}
		for i, c := range expr.Cases {
			newCases[i] = r.renameExpr(c)
		}

		return ast.NewSwitch(newTarget, newCases)
	case ast.Select:
		newTarget := r.renameExpr(expr.Target)
		newBind := r.NewName(expr.Bind)
		r.push()
		r.bind(expr.Pos(), expr.Bind, newBind)
		newBody := r.renameExpr(expr.Body)
		r.pop()

		return ast.NewSelect(newTarget, expr.Index, newBind, newBody)
	default:
		panic(ast.NewNotExprError(r.Input, expr))
	}
}

func (r *renamer) renameClause(clause ast.Clause) ast.Clause {
	r.push()
	pattern := r.renamePattern(clause.Pattern)
	body := r.renameExpr(clause.Body)
	r.pop()

	return ast.NewClause(pattern, body)
}

// Return new environment and renamed pattern.
// Resulting environment does not contain any bindings for variables in the given environment,
// so caller has to merge it.
func (r *renamer) renamePattern(pattern ast.Pattern) ast.Pattern {
	switch pattern := pattern.(type) {
	case ast.Variable:
		newName := r.NewName(pattern.Ident)
		r.bind(pattern.Pos(), pattern.Ident, newName)

		return ast.NewVariable(newName, pattern.Pos())
	case ast.Label:
		return pattern
	case ast.Literal:
		return pattern
	case ast.Apply:
		newArgs := []ast.Node{}
		for _, arg := range pattern.Args {
			arg, ok := arg.(ast.Pattern)
			if !ok {
				err := ast.NewNotPatternError(r.Input, arg)
				panic(err)
			}
			newArg := r.renamePattern(arg)
			newArgs = append(newArgs, newArg)
		}
		fun, ok := pattern.Func.(ast.Pattern)
		if !ok {
			err := ast.NewNotPatternError(r.Input, pattern.Func)
			panic(err)
		}

		return ast.NewApply(r.renamePattern(fun), newArgs)
	case ast.This:
		return pattern
	default:
		panic(ast.NewNotPatternError(r.Input, pattern))
	}
}
