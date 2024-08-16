package codata

import (
	"fmt"
	"maps"
	"math"
	"slices"
	"strings"

	"github.com/takoeight0821/malgo/ast"
	"github.com/takoeight0821/malgo/token"
	"github.com/takoeight0821/malgo/utils"
)

// Flat converts [Codata] to [Object], [Case], and [Lambda].
type Flat struct {
	uniq       int
	scrutinees []token.Token
	guards     map[int][]ast.Node
}

func (f *Flat) genUniq(hint string) string {
	f.uniq++

	return fmt.Sprintf(":%s%d", hint, f.uniq)
}

func (*Flat) Name() string {
	return "codata.Flat"
}

func (*Flat) Init([]ast.Node) error {
	return nil
}

func (f *Flat) Run(program []ast.Node) ([]ast.Node, error) {
	for i, n := range program {
		var err error
		program[i], err = f.flat(n)
		if err != nil {
			return program, err
		}
	}

	return program, nil
}

func (f *Flat) flat(node ast.Node) (ast.Node, error) {
	node, err := ast.Traverse(node, f.flatEach)
	if err != nil {
		return node, fmt.Errorf("flat %v: %w", node, err)
	}

	return node, nil
}

// flatEach flattens [Codata] nodes.
// If error occurred, return the original node and the error. Because ast.Traverse needs it.
func (f *Flat) flatEach(node ast.Node, err error) (ast.Node, error) {
	// early return if error occurred
	if err != nil {
		return node, err
	}

	if c, ok := node.(*ast.Codata); ok {
		newNode, err := f.flatCodata(c)
		if err != nil {
			return node, err
		}

		return newNode, nil
	}

	return node, nil
}

func (f *Flat) flatCodata(codata *ast.Codata) (ast.Node, error) {
	f.uniq = 0
	f.scrutinees = make([]token.Token, 0)
	f.guards = make(map[int][]ast.Node)

	plists := make(map[int][]ast.Node)
	for i, clause := range codata.Clauses {
		plists[i] = makePatternList(clause.Pattern)
	}

	bodys := make(map[int]ast.Node)
	for i, clause := range codata.Clauses {
		bodys[i] = clause.Expr
	}

	return f.build(plists, bodys)
}

// makePatternList makes a sequence of patterns from a pattern.
// For example, if the pattern is `#.f(x, y)`, the sequence is `[#.f, #(x, y)]`.
func makePatternList(pattern ast.Node) []ast.Node {
	switch pattern := pattern.(type) {
	case *ast.This:
		return []ast.Node{}
	case *ast.Access:
		pl := makePatternList(pattern.Receiver)

		return append(pl, &ast.Access{Receiver: &ast.This{Token: pattern.Base()}, Name: pattern.Name})
	case *ast.Call:
		pl := makePatternList(pattern.Func)

		return append(pl, &ast.Call{Func: &ast.This{Token: pattern.Base()}, Args: pattern.Args})
	case *ast.Paren:
		return makePatternList(pattern.Expr)
	default:
		panic(fmt.Sprintf("unexpected pattern: %v", pattern))
	}
}

func (f *Flat) build(plists map[int][]ast.Node, bodys map[int]ast.Node) (ast.Node, error) {
	if len(plists) == 0 {
		panic(fmt.Sprintf("empty plists: %v", plists))
	}

	if anyEmpty(plists) {
		return f.buildCase(plists, bodys)
	}
	kind := kindOf(plists)
	switch kind {
	case Field:
		return f.buildObject(plists, bodys)
	case Function:
		return f.buildLambda(plists, bodys)
	case Mismatch:
		var where token.Token
		for _, ps := range plists {
			if len(ps) != 0 {
				where = ps[0].Base()

				break
			}
		}

		return nil, utils.PosError{Where: where, Err: MismatchError{Plists: plists}}
	default:
		panic(fmt.Sprintf("unexpected kind: %v", kind))
	}
}

func anyEmpty(plists map[int][]ast.Node) bool {
	for _, ps := range plists {
		if len(ps) == 0 {
			return true
		}
	}

	return false
}

type Kind int

const (
	Field Kind = iota
	Function
	Mismatch
)

func kindOf(plists map[int][]ast.Node) Kind {
	kind := Mismatch
	for _, ps := range plists {
		if len(ps) == 0 {
			return kind
		}

		switch ps[0].(type) {
		case *ast.Access:
			if kind == Mismatch {
				kind = Field
			}
			if kind != Field {
				return Mismatch
			}
		case *ast.Call:
			if kind == Mismatch {
				kind = Function
			}
			if kind != Function {
				return Mismatch
			}
		default:
			return Mismatch
		}
	}

	return kind
}

type MismatchError struct {
	Plists map[int][]ast.Node
}

func (e MismatchError) Error() string {
	var builder strings.Builder
	fmt.Fprintf(&builder, "mismatched patterns %v:\n", e.Plists)
	for _, ps := range e.Plists {
		fmt.Fprintf(&builder, "\t%v\n", ps[0])
	}

	return builder.String()
}

func (f *Flat) buildCase(plists map[int][]ast.Node, bodys map[int]ast.Node) (ast.Node, error) {
	if len(f.scrutinees) == 0 {
		// If there is no scrutinee, generate a body.
		// Use the topmost body.
		topmost := searchTopmost(plists)

		return bodys[topmost], nil
	}

	// restPlists is a map of patterns that are not empty.
	restPlists := make(map[int][]ast.Node)
	for i, ps := range plists {
		if len(ps) != 0 {
			restPlists[i] = ps
		}
	}

	// restBodys is a map of bodies corresponding to restPlists.
	restBodys := make(map[int]ast.Node)
	for i := range restPlists {
		restBodys[i] = bodys[i]
	}

	var restBody ast.Node
	if len(restPlists) != 0 {
		innerF := &Flat{
			uniq:       f.uniq,
			scrutinees: f.scrutinees,
			guards:     selectIndicies(slices.Collect(maps.Keys(restPlists)), f.guards),
		}
		var err error
		restBody, err = innerF.build(restPlists, restBodys)
		if err != nil {
			return nil, err
		}
	}

	clauses := make([]*ast.CaseClause, 0, len(plists)) // Pre-allocate clauses with the correct capacity
	for i, plist := range utils.Ordered(plists) {
		switch {
		case len(plist) == 0:
			clauses = append(clauses, &ast.CaseClause{
				Patterns: f.guards[i],
				Expr:     bodys[i],
			})
		case restBody != nil:
			clauses = append(clauses, &ast.CaseClause{
				Patterns: f.guards[i],
				Expr:     restBody,
			})
		default:
			panic("restBody must not be nil")
		}
	}

	scrutinees := make([]ast.Node, len(f.scrutinees))
	for i, s := range f.scrutinees {
		scrutinees[i] = &ast.Var{Name: s}
	}

	return &ast.Case{
		Scrutinees: scrutinees,
		Clauses:    clauses,
	}, nil
}

func searchTopmost(plists map[int][]ast.Node) int {
	topmost := math.MaxInt
	for i := range plists {
		if i < topmost {
			topmost = i
		}
	}

	return topmost
}

func (f *Flat) buildObject(plists map[int][]ast.Node, bodys map[int]ast.Node) (ast.Node, error) {
	fields, rest, err := popField(plists)
	if err != nil {
		return nil, err
	}

	objectFields := make([]*ast.Field, 0)
	for name, fieldIndicies := range utils.Ordered(fields) {
		innerF := &Flat{uniq: f.uniq, scrutinees: f.scrutinees, guards: selectIndicies(fieldIndicies, f.guards)}
		expr, err := innerF.build(selectIndicies(fieldIndicies, rest), bodys)
		if err != nil {
			return nil, err
		}

		objectFields = append(objectFields, &ast.Field{
			Name: name,
			Expr: expr,
		})
	}

	return &ast.Object{
		Fields: objectFields,
	}, nil
}

func selectIndicies(indices []int, original map[int][]ast.Node) map[int][]ast.Node {
	selected := make(map[int][]ast.Node)
	for _, i := range indices {
		selected[i] = original[i]
	}

	return selected
}

func popField(plists map[int][]ast.Node) (map[string][]int, map[int][]ast.Node, error) {
	fields := make(map[string][]int)
	rest := make(map[int][]ast.Node)
	for i, patterns := range plists {
		switch pattern := patterns[0].(type) {
		case *ast.Access:
			fields[pattern.Name.Lexeme] = append(fields[pattern.Name.Lexeme], i)
		default:
			return nil, nil, utils.PosError{Where: pattern.Base(), Err: UnexpectedPatternError{Pattern: pattern}}
		}
		rest[i] = patterns[1:]
	}

	return fields, rest, nil
}

func (f *Flat) buildLambda(plists map[int][]ast.Node, bodys map[int]ast.Node) (ast.Node, error) {
	guards, rest, err := popGuard(plists)
	if err != nil {
		return nil, err
	}

	arity := -1
	var where token.Token
	for _, patterns := range guards {
		if arity == -1 {
			arity = len(patterns)
		}
		if len(patterns) != arity {
			for _, p := range patterns {
				where = p.Base()

				break
			}

			return nil, utils.PosError{Where: where, Err: InvalidArityError{Guards: guards}}
		}
	}
	if arity == -1 {
		panic("arity must be positive")
	}

	scrutinees := make([]token.Token, arity)
	for i := range scrutinees {
		scrutinees[i] = token.Token{Kind: token.IDENT, Lexeme: f.genUniq("p"), Location: where.Location, Literal: nil}
	}

	for i, ps := range guards {
		//nolint:gocritic
		guards[i] = append(f.guards[i], ps...)
	}

	innerF := &Flat{uniq: f.uniq, scrutinees: append(f.scrutinees, scrutinees...), guards: guards}
	body, err := innerF.build(rest, bodys)
	if err != nil {
		return nil, err
	}

	return &ast.Lambda{
		Params: scrutinees,
		Expr:   body,
	}, nil
}

type InvalidArityError struct {
	Guards map[int][]ast.Node
}

func (e InvalidArityError) Error() string {
	return fmt.Sprintf("invalid arity: %v", e.Guards)
}

func popGuard(plists map[int][]ast.Node) (map[int][]ast.Node, map[int][]ast.Node, error) {
	guards := make(map[int][]ast.Node)
	rest := make(map[int][]ast.Node)
	for i, patterns := range plists {
		switch pattern := patterns[0].(type) {
		case *ast.Call:
			guards[i] = pattern.Args
		default:
			return nil, nil, utils.PosError{Where: pattern.Base(), Err: UnexpectedPatternError{Pattern: pattern}}
		}
		rest[i] = patterns[1:]
	}

	return guards, rest, nil
}

type UnexpectedPatternError struct {
	Pattern ast.Node
}

func (e UnexpectedPatternError) Error() string {
	return fmt.Sprintf("unexpected pattern: %v", e.Pattern)
}
