package driver

import (
	"fmt"

	"github.com/takoeight0821/malgo/ast"
	"github.com/takoeight0821/malgo/lexer"
	"github.com/takoeight0821/malgo/parser"
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
func (r *PassRunner) AddPass(pass Pass) {
	r.passes = append(r.passes, pass)
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
	tokens, err := lexer.Lex(filePath, source)
	if err != nil {
		return nil, fmt.Errorf("lex: %w", err)
	}

	decls, err := parser.NewParser(tokens).ParseDecl()
	if err != nil {
		return nil, fmt.Errorf("parse: %w", err)
	}

	return r.Run(decls)
}
