package infix

import (
	"fmt"
	"log"

	"github.com/takoeight0821/malgo/ast"
	"github.com/takoeight0821/malgo/token"
	"github.com/takoeight0821/malgo/utils"
)

// After parsing, every infix operator treated as left-associative and has the same precedence.
// In infix.go, we will fix this.

type Resolver struct {
	decls []*ast.InfixDecl
}

func NewInfixResolver() *Resolver {
	return &Resolver{decls: make([]*ast.InfixDecl, 0)}
}

func (r *Resolver) Name() string {
	return "infix.InfixResolver"
}

func (r *Resolver) Init(program []ast.Node) error {
	for _, node := range program {
		_, err := ast.Traverse(node, func(node ast.Node, _ error) (ast.Node, error) {
			switch node := node.(type) {
			case *ast.InfixDecl:
				r.add(node)

				return node, nil
			default:
				return node, nil
			}
		})
		if err != nil {
			return fmt.Errorf("infix: %w", err)
		}
	}

	return nil
}

func (r *Resolver) Run(program []ast.Node) ([]ast.Node, error) {
	for i, node := range program {
		var err error
		program[i], err = ast.Traverse(node, func(node ast.Node, _ error) (ast.Node, error) {
			switch n := node.(type) {
			case *ast.Binary:
				return r.mkBinary(n.Op, n.Left, n.Right), nil
			case *ast.Paren:
				return n.Expr, nil
			}

			return node, nil
		})
		if err != nil {
			return program, fmt.Errorf("infix: %w", err)
		}
	}

	return program, nil
}

func (r *Resolver) add(infix *ast.InfixDecl) {
	r.decls = append(r.decls, infix)
}

func (r Resolver) prec(op token.Token) int {
	for _, decl := range r.decls {
		if decl.Name.Lexeme == op.Lexeme {
			literal, ok := decl.Prec.Literal.(int)
			if !ok {
				log.Panicf("invalid precedence: %v", decl.Prec)
			}

			return literal
		}
	}

	return 0
}

func (r Resolver) assoc(op token.Token) token.Kind {
	for _, decl := range r.decls {
		if decl.Name.Lexeme == op.Lexeme {
			return decl.Assoc.Kind
		}
	}

	return token.INFIXL
}

func (r Resolver) mkBinary(operator token.Token, left, right ast.Node) ast.Node {
	switch left := left.(type) {
	case *ast.Binary:
		// (left.Left left.Op left.Right) op right
		if r.assocRight(left.Op, operator) {
			// left.Left left.Op (left.Right op right)
			newRight := r.mkBinary(operator, left.Right, right)

			return &ast.Binary{Left: left.Left, Op: left.Op, Right: newRight}
		}

		return &ast.Binary{Left: left, Op: operator, Right: right}
	default:
		return &ast.Binary{Left: left, Op: operator, Right: right}
	}
}

func (r Resolver) assocRight(op1, op2 token.Token) bool {
	prec1 := r.prec(op1)
	prec2 := r.prec(op2)
	if prec1 > prec2 {
		return false
	} else if prec1 < prec2 {
		return true
	}
	// same precedence
	if r.assoc(op1) != r.assoc(op2) {
		panic(utils.PosError{Where: op1, Err: NeedParenError{LeftOp: op1, RightOp: op2}})
	}
	if r.assoc(op1) == token.INFIXL {
		return false
	} else if r.assoc(op1) == token.INFIXR {
		return true
	}

	panic(utils.PosError{Where: op1, Err: NeedParenError{LeftOp: op1, RightOp: op2}})
}

type NeedParenError struct {
	LeftOp, RightOp token.Token
}

func (e NeedParenError) Error() string {
	return fmt.Sprintf("need parentheses around %v and %v", e.LeftOp, e.RightOp)
}
