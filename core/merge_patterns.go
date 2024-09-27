package core

import (
	"fmt"

	"github.com/malgo-lang/malgo/ast"
)

// MergePattern is a pass that merge multiple patterns into one pattern using tuple.
type MergePatterns struct{}

func (*MergePatterns) Name() string {
	return "core.MergePatterns"
}

func (*MergePatterns) Init([]ast.Node) error {
	return nil
}

func (m *MergePatterns) Run(program []ast.Node) ([]ast.Node, error) {
	for i, n := range program {
		var err error
		program[i], err = m.merge(n)
		if err != nil {
			return program, err
		}
	}

	return program, nil
}

func (m *MergePatterns) merge(node ast.Node) (ast.Node, error) {
	node, err := ast.Traverse(node, m.mergeEach)
	if err != nil {
		return node, fmt.Errorf("merge: %w", err)
	}

	return node, nil
}

func (m *MergePatterns) mergeEach(node ast.Node, err error) (ast.Node, error) {
	if err != nil {
		return node, err
	}

	if node, ok := node.(*ast.Binary); ok {
		return &ast.Call{
			Func: &ast.Var{Name: node.Op},
			Args: []ast.Node{node.Left, node.Right},
		}, nil
	}

	if node, ok := node.(*ast.Case); ok {
		scrutinee := &ast.Tuple{
			Where: node.Base(),
			Exprs: node.Scrutinees,
		}

		clauses := make([]*ast.CaseClause, len(node.Clauses))
		for i, clause := range node.Clauses {
			clauses[i] = &ast.CaseClause{
				Patterns: []ast.Node{&ast.Tuple{
					Where: clause.Base(),
					Exprs: clause.Patterns,
				}},
				Expr: clause.Expr,
			}
		}

		return &ast.Case{
			Scrutinees: []ast.Node{scrutinee},
			Clauses:    clauses,
		}, nil
	}

	return node, nil
}
