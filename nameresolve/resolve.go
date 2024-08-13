// Package nameresolve resolves variable names and allocates unique numbers to them.
// It also checks if a variable is already defined in the same scope.
// All errors are accumulated and returned at the end of the process.
package nameresolve

import (
	"fmt"
	"log"

	"github.com/takoeight0821/malgo/ast"
	"github.com/takoeight0821/malgo/token"
	"github.com/takoeight0821/malgo/utils"
)

// Resolver resolves variable names and allocates unique numbers to them.
type Resolver struct {
	supply int  // Supply of unique numbers.
	env    *env // Current environment.
}

func NewResolver() *Resolver {
	return &Resolver{
		supply: 0,
		env:    newEnv(nil),
	}
}

// env is an linked list of name->id mappings.
type env struct {
	parent *env           // Parent environment.
	table  map[string]int // Variable name -> unique number.
}

func newEnv(parent *env) *env {
	return &env{
		parent: parent,
		table:  make(map[string]int),
	}
}

func (r *Resolver) Name() string {
	return "nameresolve.Resolver"
}

func (r *Resolver) Init(program []ast.Node) error {
	// Register top-level declarations.
	for _, node := range program {
		err := r.registerTopLevel(node)
		if err != nil {
			return err
		}
	}

	return nil
}

func (r *Resolver) Run(program []ast.Node) ([]ast.Node, error) {
	for i, node := range program {
		var err error
		program[i], err = r.solve(node)
		if err != nil {
			return program, err
		}
	}

	return program, nil
}

// define a variable in the current scope.
func (r *Resolver) define(name token.Token) {
	r.env.table[name.Lexeme] = r.supply
	r.supply++
}

type NotDefinedError struct {
	Name token.Token
}

func (e NotDefinedError) Error() string {
	return e.Name.String() + " is not defined"
}

func (e *env) lookup(name token.Token) (token.Token, error) {
	if uniq, ok := e.table[name.Lexeme]; ok {
		return token.Token{Kind: name.Kind, Lexeme: name.Lexeme, Location: name.Location, Literal: uniq}, nil
	}

	if e.parent != nil {
		return e.parent.lookup(name)
	}

	return name, utils.PosError{Where: name, Err: NotDefinedError{Name: name}}
}

// Define all top-level variables in the node.
func (r *Resolver) registerTopLevel(node ast.Node) error {
	if node, ok := node.(*ast.VarDecl); ok {
		if _, ok := r.env.table[node.Name.Lexeme]; ok {
			return utils.PosError{Where: node.Base(), Err: AlreadyDefinedError{Name: node.Name}}
		}
		r.define(node.Name)
	}

	return nil
}

func (r *Resolver) solveToken(t token.Token) (token.Token, error) {
	return r.env.lookup(t)
}

type NotClauseError struct {
	Node ast.Node
}

func (e NotClauseError) Error() string {
	return fmt.Sprintf("%v is not a clause", e.Node)
}

type NotFieldError struct {
	Node ast.Node
}

func (e NotFieldError) Error() string {
	return fmt.Sprintf("%v is not a field", e.Node)
}

// solve all variables in the node.
func (r *Resolver) solve(node ast.Node) (ast.Node, error) {
	switch node := node.(type) {
	case *ast.Var:
		var err error
		node.Name, err = r.env.lookup(node.Name)

		return node, err
	case *ast.Literal:
		return node, nil
	case *ast.Symbol:
		return node, nil
	case *ast.Paren:
		var err error
		node.Expr, err = r.solve(node.Expr)

		return node, err
	case *ast.Tuple:
		var err error
		for i, expr := range node.Exprs {
			node.Exprs[i], err = r.solve(expr)
			if err != nil {
				return node, err
			}
		}

		return node, nil
	case *ast.Access:
		var err error
		node.Receiver, err = r.solve(node.Receiver)

		return node, err
	case *ast.Call:
		var err error
		node.Func, err = r.solve(node.Func)
		if err != nil {
			return node, err
		}
		for i, arg := range node.Args {
			node.Args[i], err = r.solve(arg)
			if err != nil {
				return node, err
			}
		}

		return node, nil
	case *ast.Prim:
		var err error
		for i, arg := range node.Args {
			node.Args[i], err = r.solve(arg)
			if err != nil {
				return node, err
			}
		}

		return node, nil
	case *ast.Binary:
		var err error
		node.Op, err = r.env.lookup(node.Op)
		if err != nil {
			return node, err
		}
		node.Left, err = r.solve(node.Left)
		if err != nil {
			return node, err
		}
		node.Right, err = r.solve(node.Right)
		if err != nil {
			return node, err
		}

		return node, nil
	case *ast.Let:
		_, err := r.assign(node.Bind, overwrite)
		if err != nil {
			return node, err
		}
		node.Bind, err = r.solve(node.Bind)
		if err != nil {
			return node, err
		}
		node.Body, err = r.solve(node.Body)
		if err != nil {
			return node, err
		}

		return node, nil
	case *ast.Seq:
		for i, expr := range node.Exprs {
			var err error
			node.Exprs[i], err = r.solve(expr)
			if err != nil {
				return node, err
			}
		}

		return node, nil
	case *ast.Codata:
		log.Panicf("codata must be desugared before name resolution:\n%v", node)

		return node, nil
	case *ast.CodataClause:
		r.env = newEnv(r.env)
		defer func() { r.env = r.env.parent }()
		_, err := r.assign(node.Pattern, asPattern)
		if err != nil {
			return node, err
		}
		node.Pattern, err = r.solve(node.Pattern)
		if err != nil {
			return node, err
		}
		node.Expr, err = r.solve(node.Expr)
		if err != nil {
			return node, err
		}

		return node, nil
	case *ast.Lambda:
		r.env = newEnv(r.env)
		defer func() { r.env = r.env.parent }()
		for i, param := range node.Params {
			_, err := r.assignToken(param, asPattern)
			if err != nil {
				return node, err
			}
			node.Params[i], err = r.solveToken(param)
			if err != nil {
				return node, err
			}
		}
		var err error
		node.Expr, err = r.solve(node.Expr)
		if err != nil {
			return node, err
		}

		return node, nil
	case *ast.Case:
		for i, scr := range node.Scrutinees {
			var err error
			node.Scrutinees[i], err = r.solve(scr)
			if err != nil {
				return node, err
			}
		}
		for i, clause := range node.Clauses {
			clause, err := r.solve(clause)
			if err != nil {
				return node, err
			}
			theClause, ok := clause.(*ast.CaseClause)
			if !ok {
				return node, utils.PosError{Where: clause.Base(), Err: NotClauseError{Node: clause}}
			}
			node.Clauses[i] = theClause
		}

		return node, nil
	case *ast.CaseClause:
		r.env = newEnv(r.env)
		defer func() { r.env = r.env.parent }()
		for i, pattern := range node.Patterns {
			_, err := r.assign(pattern, asPattern)
			if err != nil {
				return node, err
			}
			node.Patterns[i], err = r.solve(pattern)
			if err != nil {
				return node, err
			}
		}
		var err error
		node.Expr, err = r.solve(node.Expr)
		if err != nil {
			return node, err
		}

		return node, nil
	case *ast.Object:
		for i, field := range node.Fields {
			field, err := r.solve(field)
			if err != nil {
				return node, err
			}
			theField, ok := field.(*ast.Field)
			if !ok {
				return node, utils.PosError{Where: field.Base(), Err: NotFieldError{Node: field}}
			}
			node.Fields[i] = theField
		}

		return node, nil
	case *ast.Field:
		r.env = newEnv(r.env)
		defer func() { r.env = r.env.parent }()
		var err error
		node.Expr, err = r.solve(node.Expr)
		if err != nil {
			return node, err
		}

		return node, nil
	case *ast.VarDecl:
		var err error
		node.Name, err = r.env.lookup(node.Name)
		if err != nil {
			return node, err
		}
		if node.Expr != nil {
			node.Expr, err = r.solve(node.Expr)
			if err != nil {
				return node, err
			}
		}

		return node, nil
	case *ast.InfixDecl:
		var err error
		node.Name, err = r.env.lookup(node.Name)
		if err != nil {
			return node, err
		}

		return node, nil
	case *ast.This:
		return node, nil
	default:
		log.Panicf("unexpected node: %v", node)

		return node, nil
	}
}

type mode func(*Resolver, ast.Node) ([]string, error)

type AlreadyDefinedError struct {
	Name token.Token
}

func (e AlreadyDefinedError) Error() string {
	return e.Name.String() + " is already defined"
}

// overwrite defines all variables in the node.
// If a variable is already defined in current scope, it is overwritten.
func overwrite(r *Resolver, node ast.Node) ([]string, error) {
	var defined []string
	_, err := ast.Traverse(node, func(node ast.Node, _ error) (ast.Node, error) {
		switch node := node.(type) {
		case *ast.Var:
			r.define(node.Name)
			defined = append(defined, node.Name.Lexeme)

			return node, nil
		default:
			return node, nil
		}
	})
	if err != nil {
		return nil, fmt.Errorf("overwrite: %w", err)
	}

	return defined, nil
}

type InvalidPatternError struct {
	Pattern ast.Node
}

func (e InvalidPatternError) Error() string {
	return fmt.Sprintf("invalid pattern %v", e.Pattern)
}

// Define variables in the node as pattern.
func asPattern(resolver *Resolver, pattern ast.Node) ([]string, error) {
	switch pattern := pattern.(type) {
	case *ast.Var:
		// If pattern is a constructor, ignore it.
		if utils.IsUpper(pattern.Name.Lexeme) {
			return nil, nil
		}
		if _, ok := resolver.env.table[pattern.Name.Lexeme]; ok {
			return nil, utils.PosError{Where: pattern.Base(), Err: AlreadyDefinedError{Name: pattern.Name}}
		}
		resolver.define(pattern.Name)

		return []string{pattern.Name.Lexeme}, nil
	case *ast.Literal:
		return nil, nil
	case *ast.Symbol:
		return nil, nil
	case *ast.Paren:
		return resolver.assign(pattern.Expr, asPattern)
	case *ast.Tuple:
		var defined []string
		for _, pat := range pattern.Exprs {
			newDefs, err := resolver.assign(pat, asPattern)
			if err != nil {
				return nil, err
			}
			defined = append(defined, newDefs...)
		}

		return defined, nil
	case *ast.Access:
		return resolver.assign(pattern.Receiver, asPattern)
	case *ast.Call:
		defined, err := resolver.assign(pattern.Func, asPattern)
		if err != nil {
			return nil, err
		}

		for _, arg := range pattern.Args {
			newDefs, err := resolver.assign(arg, asPattern)
			if err != nil {
				return nil, err
			}
			defined = append(defined, newDefs...)
		}

		return defined, nil
	default:
		return nil, utils.PosError{Where: pattern.Base(), Err: InvalidPatternError{Pattern: pattern}}
	}
}

type InvalidTypeError struct {
	Type ast.Node
}

func (e InvalidTypeError) Error() string {
	return fmt.Sprintf("invalid type %v", e.Type)
}

// assign defines variables in the node.
// The mode function determines which variables are defined.
// Returns a list of defined variables.
func (r *Resolver) assign(node ast.Node, mode mode) ([]string, error) {
	return mode(r, node)
}

func (r *Resolver) assignToken(t token.Token, mode mode) ([]string, error) {
	return r.assign(&ast.Var{Name: t}, mode)
}
