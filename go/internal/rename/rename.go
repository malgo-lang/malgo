package rename

import (
	"fmt"

	"github.com/takoeight0821/malgo/internal/ast"
)

// Check all variables are bound and replace them with RnID.
func Rename(input string, expr ast.Expr) ast.Expr {
	return newRenamer(input).renameExpr(expr)
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
	input string
	names map[string]int
	env   *rnEnv
}

// type rnEnv map[ast.Ident]RnID

type rnEnv struct {
	current map[ast.Ident]RnID
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
		current: map[ast.Ident]RnID{},
		parent:  nil,
	}
}

func (r *renamer) bind(pos int, ident ast.Ident, renamedIdent RnID) {
	if _, ok := r.env.current[ident]; ok {
		err := AlreadyBoundError{input: r.input, pos: pos, ident: ident}
		panic(err)
	}

	r.env.current[ident] = renamedIdent
}

func (r *renamer) push() {
	r.env = &rnEnv{
		current: map[ast.Ident]RnID{},
		parent:  r.env,
	}
}

func (r *renamer) pop() {
	r.env = r.env.parent
}

// Iterates over the environment chain.
func (env rnEnv) lookup(ident ast.Ident) (RnID, bool) {
	if id, ok := env.current[ident]; ok {
		return id, true
	}
	return env.parent.lookup(ident)
}

func newRenamer(input string) *renamer {
	return &renamer{
		input: input,
		names: map[string]int{},
		env:   newRnEnv(),
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

func (r *renamer) renameExpr(expr ast.Expr) ast.Expr {
	if !expr.IsExpr() {
		err := NotExprError{input: r.input, expr: expr}
		panic(err)
	}

	switch expr := expr.(type) {
	case ast.Variable:
		if v, ok := r.env.lookup(expr.Ident); ok {
			return ast.NewVariable(v, expr.Pos())
		}

		err := UnbondVariableError{input: r.input, variable: expr}
		panic(err)
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
	default:
		return expr
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
	if !pattern.IsPattern() {
		err := NotPatternError{input: r.input, pattern: pattern}
		panic(err)
	}

	switch pattern := pattern.(type) {
	case ast.Variable:
		newName := r.newName(pattern.Ident)
		r.bind(pattern.Pos(), pattern.Ident, newName)

		return ast.NewVariable(newName, pattern.Pos())
	case ast.Apply:
		newArgs := []ast.Node{}
		for _, arg := range pattern.Args {
			arg, ok := arg.(ast.Pattern)
			if !ok {
				err := NotPatternError{input: r.input, pattern: arg}
				panic(err)
			}
			newArg := r.renamePattern(arg)
			newArgs = append(newArgs, newArg)
		}
		fun, ok := pattern.Func.(ast.Pattern)
		if !ok {
			err := NotPatternError{input: r.input, pattern: pattern.Func}
			panic(err)
		}

		return ast.NewApply(r.renamePattern(fun), newArgs)
	default:
		return pattern
	}
}
