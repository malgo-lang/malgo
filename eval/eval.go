package eval

import (
	"errors"
	"fmt"

	"github.com/takoeight0821/malgo/ast"
	"github.com/takoeight0821/malgo/token"
	"github.com/takoeight0821/malgo/utils"
)

// Eval evaluates the given node and returns the result.
func (ev *Evaluator) Eval(node ast.Node) (Value, error) {
	switch node := node.(type) {
	case *ast.Var:
		return ev.evalVar(node)
	case *ast.Literal:
		return ev.evalLiteral(node)
	case *ast.Paren:
		return ev.evalParen(node)
	case *ast.Tuple:
		return ev.evalTuple(node)
	case *ast.Access:
		return ev.evalAccess(node)
	case *ast.Call:
		return ev.evalCall(node)
	case *ast.Prim:
		return ev.evalPrim(node)
	case *ast.Binary:
		return ev.evalBinary(node)
	case *ast.Assert:
		return ev.evalAssert(node)
	case *ast.Let:
		return Unit(), ev.evalLet(node)
	case *ast.Seq:
		var result Value
		for _, expr := range node.Exprs {
			var err error
			result, err = ev.Eval(expr)
			if err != nil {
				return nil, err
			}
		}

		return result, nil
	case *ast.Codata:
		panic("unreachable: codata must be desugared")
	case *ast.CodataClause:
		panic("unreachable: clause cannot appear outside of case")
	case *ast.Lambda:
		return ev.evalLambda(node), nil
	case *ast.Case:
		return ev.evalCase(node)
	case *ast.Object:
		return ev.evalObject(node), nil
	case *ast.Field:
		panic("unreachable: field cannot appear outside of object")
	case *ast.TypeDecl:
		return Unit(), ev.evalTypeDecl(node)
	case *ast.VarDecl:
		return Unit(), ev.evalVarDecl(node)
	case *ast.InfixDecl:
		return Unit(), nil
	case *ast.This:
		panic("unreachable: this cannot appear outside of pattern")
	}

	panic(fmt.Sprintf("unreachable: %v", node))
}

func (ev *Evaluator) evalVar(node *ast.Var) (Value, error) {
	name := tokenToName(node.Name)
	if v := ev.evEnv.get(name); v != nil {
		return v, nil
	}

	return nil, utils.PosError{Where: node.Base(), Err: UndefinedVariableError{Name: node.Name}}
}

func (ev *Evaluator) evalLiteral(node *ast.Literal) (Value, error) {
	//exhaustive:ignore
	switch node.Kind {
	case token.INTEGER:
		v, ok := node.Literal.(int)
		if !ok {
			return nil, utils.PosError{Where: node.Base(), Err: InvalidLiteralError{Kind: node.Kind}}
		}

		return Int(v), nil
	case token.STRING:
		v, ok := node.Literal.(string)
		if !ok {
			return nil, utils.PosError{Where: node.Base(), Err: InvalidLiteralError{Kind: node.Kind}}
		}

		return String(v), nil
	default:
		return nil, utils.PosError{Where: node.Base(), Err: InvalidLiteralError{Kind: node.Kind}}
	}
}

func (ev *Evaluator) evalParen(node *ast.Paren) (Value, error) {
	return ev.Eval(node.Expr)
}

func (ev *Evaluator) evalTuple(node *ast.Tuple) (Value, error) {
	var err error
	values := make([]Value, len(node.Exprs))
	for i, expr := range node.Exprs {
		values[i], err = ev.Eval(expr)
		if err != nil {
			return nil, err
		}
	}

	return Tuple(values), nil
}

func (ev *Evaluator) evalAccess(node *ast.Access) (Value, error) {
	receiver, err := ev.Eval(node.Receiver)
	if err != nil {
		return nil, err
	}

	switch receiver := receiver.(type) {
	case Object:
		if value, ok := receiver.Fields[node.Name.Lexeme]; ok {
			value, err = runThunk(value)
			if err != nil {
				return nil, err
			}
			receiver.Fields[node.Name.Lexeme] = value

			return value, nil
		}

		return nil, utils.PosError{Where: node.Base(), Err: UndefinedFieldError{Receiver: receiver, Name: node.Name.Lexeme}}
	default:
		return nil, utils.PosError{Where: node.Base(), Err: NotObjectError{Receiver: receiver}}
	}
}

func (ev *Evaluator) evalCall(node *ast.Call) (Value, error) {
	function, err := ev.Eval(node.Func)
	if err != nil {
		return nil, err
	}
	switch function := function.(type) {
	case Callable:
		args := make([]Value, len(node.Args))
		for i, arg := range node.Args {
			args[i], err = ev.Eval(arg)
			if err != nil {
				return nil, err
			}
		}
		v, err := function.Apply(node.Base(), args...)
		if err != nil {
			return nil, utils.PosError{Where: node.Base(), Err: err}
		}

		return v, nil
	default:
		return nil, utils.PosError{Where: node.Base(), Err: NotCallableError{Func: function}}
	}
}

func (ev *Evaluator) evalPrim(node *ast.Prim) (Value, error) {
	prim := fetchPrim(node.Name)
	if prim == nil {
		return nil, utils.PosError{Where: node.Base(), Err: UndefinedPrimError{Name: node.Name}}
	}

	args := make([]Value, len(node.Args))
	for i, arg := range node.Args {
		var err error
		args[i], err = ev.Eval(arg)
		if err != nil {
			return nil, err
		}
	}

	return prim(ev, args...)
}

func asInt(v Value) (Int, bool) {
	switch v := v.(type) {
	case Int:
		return v, true
	default:
		return 0, false
	}
}

func errorAt(base token.Token, err error) utils.PosError {
	return utils.PosError{Where: base, Err: err}
}

func fetchPrim(name token.Token) func(*Evaluator, ...Value) (Value, error) {
	return func(ev *Evaluator, args ...Value) (Value, error) {
		p := primitiveEvaluator{Evaluator: ev, where: name}

		return p.primitive(name.Lexeme)(args...)
	}
}

type ExitError struct {
	Code int
}

func (e ExitError) Error() string {
	return fmt.Sprintf("exit(%d)", e.Code)
}

func (ev *Evaluator) evalBinary(node *ast.Binary) (Value, error) {
	name := tokenToName(node.Op)
	if operator := ev.evEnv.get(name); operator != nil {
		switch operator := operator.(type) {
		case Callable:
			left, err := ev.Eval(node.Left)
			if err != nil {
				return nil, err
			}
			right, err := ev.Eval(node.Right)
			if err != nil {
				return nil, err
			}
			v, err := operator.Apply(node.Base(), left, right)
			if err != nil {
				return nil, utils.PosError{Where: node.Base(), Err: err}
			}

			return v, nil
		default:
			return nil, utils.PosError{Where: node.Base(), Err: NotCallableError{Func: operator}}
		}
	}

	return nil, utils.PosError{Where: node.Base(), Err: UndefinedVariableError{Name: node.Op}}
}

func (ev *Evaluator) evalAssert(node *ast.Assert) (Value, error) {
	return ev.Eval(node.Expr)
}

// evalLet evaluates the given let expression.
// let expression does not create a new scope.
// It just overrides the existing bindings or creates new bindings if not exists.
func (ev *Evaluator) evalLet(node *ast.Let) error {
	body, err := ev.Eval(node.Body)
	if err != nil {
		return err
	}
	if env, ok := body.match(node.Bind); ok {
		for name, v := range env {
			ev.evEnv.set(name, v)
		}

		return nil
	}

	return utils.PosError{
		Where: node.Base(),
		Err:   PatternMatchError{Patterns: []ast.Node{node.Bind}, Values: []Value{body}},
	}
}

func (ev *Evaluator) evalLambda(node *ast.Lambda) Function {
	params := make([]Name, len(node.Params))
	for i, param := range node.Params {
		params[i] = tokenToName(param)
	}

	return Function{
		Evaluator: *ev,
		Params:    params,
		Body:      node.Expr,
	}
}

// evalCase evaluates the given case expression.
// It first evaluates all scrutinees and then tries to match them with each clause.
// If a match is found, it evaluates the corresponding expressions and returns the result.
// If no match is found, it returns an error.
func (ev *Evaluator) evalCase(node *ast.Case) (Value, error) {
	scrs := make([]Value, len(node.Scrutinees))
	for i, scr := range node.Scrutinees {
		var err error
		scrs[i], err = ev.Eval(scr)
		if err != nil {
			return nil, err
		}
	}

	var err error
	for _, clause := range node.Clauses {
		if env, ok := matchClause(clause, scrs); ok {
			ev.evEnv = newEvEnv(ev.evEnv)

			for name, v := range env {
				ev.evEnv.set(name, v)
			}
			ret, err := ev.Eval(clause.Expr)
			if err != nil {
				return nil, err
			}

			ev.evEnv = ev.evEnv.parent

			return ret, nil
		}
		err = errors.Join(err, PatternMatchError{Patterns: clause.Patterns, Values: scrs})
	}

	return nil, utils.PosError{Where: node.Base(), Err: err}
}

// matchClause matches the given clause's patterns with the given scrutinees.
func matchClause(clause *ast.CaseClause, scrs []Value) (map[Name]Value, bool) {
	if len(clause.Patterns) != len(scrs) {
		return nil, false
	}
	env := make(map[Name]Value)
	for i, pattern := range clause.Patterns {
		m, ok := scrs[i].match(pattern)
		if !ok {
			return nil, false
		}
		for k, v := range m {
			env[k] = v
		}
	}

	return env, true
}

func (ev *Evaluator) evalObject(node *ast.Object) Object {
	fields := make(map[string]Value)
	for _, field := range node.Fields {
		fields[field.Name] = Thunk{Evaluator: *ev, Body: field.Expr}
	}

	return Object{Fields: fields}
}

func (ev *Evaluator) evalTypeDecl(node *ast.TypeDecl) error {
	for _, ctor := range node.Types {
		err := ev.defineConstructor(ctor)
		if err != nil {
			return err
		}
	}

	return nil
}

func (ev *Evaluator) defineConstructor(node ast.Node) error {
	switch node := node.(type) {
	case *ast.Var:
		ev.evEnv.set(tokenToName(node.Name), Data{Tag: tokenToName(node.Name), Elems: nil})

		return nil
	case *ast.Call:
		switch fn := node.Func.(type) {
		case *ast.Var:
			ev.evEnv.set(tokenToName(fn.Name), Constructor{Evaluator: *ev, Tag: tokenToName(fn.Name), Params: len(node.Args)})

			return nil
		case *ast.Prim:
			// For type checking
			// Ignore in evaluation
			return nil
		}
	case *ast.Prim:
		// For type checking
		// Ignore in evaluation
		return nil
	}

	return utils.PosError{Where: node.Base(), Err: NotConstructorError{Node: node}}
}

func (ev *Evaluator) evalVarDecl(node *ast.VarDecl) error {
	if node.Expr != nil {
		v, err := ev.Eval(node.Expr)
		if err != nil {
			return err
		}
		ev.evEnv.set(tokenToName(node.Name), v)
	}

	return nil
}
