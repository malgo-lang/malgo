package desugarwith

import (
	"fmt"

	"github.com/takoeight0821/malgo/ast"
)

type DesugarWith struct{}

func (*DesugarWith) Name() string {
	return "desugar.with.With"
}

func (*DesugarWith) Init([]ast.Node) error {
	return nil
}

func (d *DesugarWith) Run(program []ast.Node) ([]ast.Node, error) {
	for i, n := range program {
		var err error
		program[i], err = d.desugar(n)
		if err != nil {
			return program, err
		}
	}

	return program, nil
}

func (d *DesugarWith) desugar(node ast.Node) (ast.Node, error) {
	node, err := ast.Traverse(node, d.desugarEach)
	if err != nil {
		return node, fmt.Errorf("desugar: %w", err)
	}

	return node, nil
}

func (d *DesugarWith) desugarEach(node ast.Node, err error) (ast.Node, error) {
	// early return if error occurred
	if err != nil {
		return node, err
	}

	if s, ok := node.(*ast.Seq); ok {
		seq, err := d.desugarSeq(s.Exprs)
		if err != nil {
			return node, err
		}

		return &ast.Seq{Exprs: seq}, nil
	}

	return node, nil
}

func (d *DesugarWith) desugarSeq(seq []ast.Node) ([]ast.Node, error) {
	if len(seq) == 0 {
		return seq, nil
	}

	first := seq[0]
	rest := seq[1:]
	switch first := first.(type) {
	case *ast.With:
		return d.desugarWith(first, rest)
	default:
		restDesugared, err := d.desugarSeq(rest)
		if err != nil {
			return seq, err
		}

		return append([]ast.Node{first}, restDesugared...), nil
	}
}

func (d *DesugarWith) desugarWith(with *ast.With, rest []ast.Node) ([]ast.Node, error) {
	restDesugared, err := d.desugarSeq(rest)
	if err != nil {
		return nil, err
	}

	cont := &ast.Codata{Clauses: [](*ast.CodataClause){&ast.CodataClause{
		Pattern: &ast.Call{Func: &ast.This{Token: with.Base()}, Args: with.Binds},
		Expr:    &ast.Seq{Exprs: restDesugared},
	}}}

	return []ast.Node{
		&ast.Call{Func: with.Body, Args: []ast.Node{cont}},
	}, nil
}
