package driver

import (
	"errors"
	"fmt"

	"github.com/malgo-lang/malgo/ast"
	"github.com/malgo-lang/malgo/codata"
	"github.com/malgo-lang/malgo/desugarcurry"
	"github.com/malgo-lang/malgo/desugarwith"
	"github.com/malgo-lang/malgo/infix"
	"github.com/malgo-lang/malgo/nameresolve"
	"github.com/malgo-lang/malgo/parser"
	"github.com/malgo-lang/malgo/scanner"
)

type Pass interface {
	Name() string
	Init(program []ast.Node) error
	Run(program []ast.Node) ([]ast.Node, error)
}

type PassRunner struct {
	passes []Pass
}

func NewPassRunner() *PassRunner {
	return &PassRunner{passes: make([]Pass, 0)}
}

// AddPass adds a pass to the end of the pass list.
func (r *PassRunner) addPass(pass Pass) {
	r.passes = append(r.passes, pass)
}

func AddPassesUntil(runner *PassRunner, until Pass) {
	passes := []Pass{
		&desugarwith.DesugarWith{},
		&desugarcurry.DesugarCurry{},
		&codata.Flat{},
		infix.NewInfixResolver(),
		nameresolve.NewResolver(),
	}

	for _, pass := range passes {
		runner.addPass(pass)
		if pass.Name() == until.Name() {
			break
		}
	}
}

// Run executes passes in order.
// If an error occurs, it stops the execution and returns the current program.
func (r *PassRunner) Run(program []ast.Node) ([]ast.Node, error) {
	for _, pass := range r.passes {
		err := pass.Init(program)
		if err != nil {
			return program, fmt.Errorf("%s init: %w", pass.Name(), err)
		}
		program, err = pass.Run(program)
		if err != nil {
			return program, fmt.Errorf("%s run: %w", pass.Name(), err)
		}
	}

	return program, nil
}

// RunSource parses the source code and executes passes in order.
func (r *PassRunner) RunSource(filePath, source string) ([]ast.Node, error) {
	tokens := scanner.Scan(filePath, source)
	parser, err := parser.NewParser(tokens)
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}

	decls, err := parser.ParseDecl()
	if err != nil {
		expr, err2 := parser.ParseExpr()
		decls = []ast.Node{expr}

		if err2 != nil {
			return nil, fmt.Errorf("parse: %w", errors.Join(err, err2))
		}
	}

	return r.Run(decls)
}
