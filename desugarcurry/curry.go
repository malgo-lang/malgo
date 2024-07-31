package desugarcurry

import (
	"fmt"

	"github.com/takoeight0821/malgo/ast"
	"github.com/takoeight0821/malgo/token"
)

// DesugarCurry is a pass that rewrite all function calls and lambda expressions to curried form.
// And also rewrite binary expressions to function calls.
type DesugarCurry struct{}

func (*DesugarCurry) Name() string {
	return "desugar.curry.Curry"
}

func (*DesugarCurry) Init([]ast.Node) error {
	return nil
}

func (d *DesugarCurry) Run(program []ast.Node) ([]ast.Node, error) {
	for i, n := range program {
		var err error
		program[i], err = d.desugar(n)
		if err != nil {
			return program, err
		}
	}

	return program, nil
}

func (d *DesugarCurry) desugar(node ast.Node) (ast.Node, error) {
	node, err := ast.Traverse(node, d.desugarEach)
	if err != nil {
		return node, fmt.Errorf("desugar: %w", err)
	}

	return node, nil
}

func (d *DesugarCurry) desugarEach(node ast.Node, err error) (ast.Node, error) {
	// early return if error occurred
	if err != nil {
		return node, err
	}

	switch node := node.(type) {
	case *ast.Call:
		if len(node.Args) <= 1 {
			return node, nil
		}

		fun := node.Func

		for _, arg := range node.Args {
			fun = &ast.Call{Func: fun, Args: []ast.Node{arg}}
		}

		return fun, nil
	case *ast.Lambda:
		if len(node.Params) <= 1 {
			return node, nil
		}

		expr := node.Expr
		for i := len(node.Params); i > 0; i-- {
			expr = &ast.Lambda{Params: []token.Token{node.Params[i-1]}, Expr: expr}
		}

		return expr, nil
	case *ast.Binary:
		return &ast.Call{
			Func: &ast.Call{
				Func: &ast.Var{Name: node.Op},
				Args: []ast.Node{node.Left},
			},
			Args: []ast.Node{node.Right},
		}, nil
	default:
		return node, nil
	}
}
