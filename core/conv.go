package core

import (
	"fmt"

	"github.com/malgo-lang/malgo/ast"
	"github.com/malgo-lang/malgo/token"
	"github.com/malgo-lang/malgo/utils"
)

type Converter struct {
	*UniqueGen
	toplevels map[string]int
	env       *env
}

type env struct {
	parent *env
	table  map[string]int
}

func NewConverter() *Converter {
	return &Converter{
		UniqueGen: NewGenerator(),
		toplevels: make(map[string]int, 0),
		env:       &env{nil, make(map[string]int)},
	}
}

func (c *Converter) assign(name string, unique int) {
	c.env.table[name] = unique
}

func (c *Converter) lookup(name string) (int, bool) {
	for env := c.env; env != nil; env = env.parent {
		if u, ok := env.table[name]; ok {
			return u, true
		}
	}

	return 0, false
}

func withUnique(token token.Token, unique int) token.Token {
	token.Literal = unique

	return token
}

func (c *Converter) Predefine(name token.Token) {
	c.toplevels[name.Lexeme] = c.Fresh()
}

func (c *Converter) ConvDef(decl *ast.VarDecl) (*Def, error) {
	uniq, ok := c.toplevels[decl.Name.Lexeme]
	if !ok {
		return nil, utils.PosError{Where: decl.Base(), Err: NotPredefinedError{decl.Name}}
	}
	name := withUnique(decl.Name, uniq)

	ret := c.FreshName("def", decl.Base().Location) // return definition
	body, err := c.ConvExpr(decl.Expr)
	if err != nil {
		return nil, err
	}

	return &Def{
		Name:    name,
		Returns: []token.Token{ret},
		Body: &Cut{
			Producer: body,
			Consumer: &Var{Name: ret},
		},
	}, nil
}

type NotPredefinedError struct {
	Name token.Token
}

func (e NotPredefinedError) Error() string {
	return e.Name.Lexeme + " is not pre-defined"
}

func (c *Converter) ConvExpr(expr ast.Node) (Producer, error) {
	switch expr := expr.(type) {
	case *ast.Var:
		if name, ok := c.lookup(expr.Name.Lexeme); ok {
			return &Var{Name: withUnique(expr.Name, name)}, nil
		}

		if uniq, ok := c.toplevels[expr.Name.Lexeme]; ok {
			ret := c.FreshName("top", expr.Base().Location) // return top-level variable

			return &Do{
				Name: ret,
				Body: &Invoke{
					Name:  withUnique(expr.Name, uniq),
					Conts: []Consumer{&Var{Name: ret}},
				},
			}, nil
		}

		return nil, utils.PosError{
			Where: expr.Name,
			Err:   NotDefinedError{Name: expr.Name},
		}
	case *ast.Literal:
		return &Literal{Token: expr.Token}, nil
	case *ast.Symbol:
		return &Symbol{Name: expr.Name}, nil
	case *ast.Tuple:
		return c.convTuple(expr)
	case *ast.Access:
		return c.convAccess(expr)
	case *ast.Call:
		fun, err := c.ConvExpr(expr.Func)
		if err != nil {
			return nil, err
		}

		return c.apply(expr.Base().Location, fun, expr.Args)
	case *ast.Prim:
		return c.convPrim(expr)
	case *ast.Seq:
		return c.convSeq(expr)
	case *ast.Lambda:
		return c.convLambda(expr)
	case *ast.Case:
		return c.convCase(expr)
	case *ast.Object:
		return c.convObject(expr)
	}

	return nil, unexpectedNodeError{expr, "expr"}
}

type unexpectedNodeError struct {
	node ast.Node
	when string
}

func (e unexpectedNodeError) Error() string {
	return fmt.Sprintf("unexpected node (%s): %v", e.when, e.node)
}

func (c *Converter) convTuple(expr *ast.Tuple) (Producer, error) {
	return c.apply(expr.Base().Location, &Symbol{
		Name: token.Token{
			Kind:     token.IDENT,
			Lexeme:   "tuple",
			Location: expr.Base().Location,
			Literal:  nil,
		},
	}, expr.Exprs)
}

func (c *Converter) convAccess(expr *ast.Access) (Producer, error) {
	ret := c.FreshName("proj", expr.Base().Location) // result of projection
	receiver, err := c.ConvExpr(expr.Receiver)
	if err != nil {
		return nil, err
	}

	return &Do{
		Name: ret,
		Body: &Cut{
			Producer: receiver,
			Consumer: &Destruct{
				Name: expr.Name.Lexeme,
				Args: []Producer{},
				Conts: []Consumer{
					&Var{Name: ret},
				},
			},
		},
	}, nil
}

func (c *Converter) convPrim(expr *ast.Prim) (Producer, error) {
	ret := c.FreshName("prim", expr.Base().Location)
	args := make([]Producer, len(expr.Args))
	for i, arg := range expr.Args {
		producer, err := c.ConvExpr(arg)
		if err != nil {
			return nil, err
		}

		args[i] = producer
	}

	return &Do{
		Name: ret,
		Body: &Prim{
			Name: expr.Name,
			Args: args,
			Cont: &Var{Name: ret},
		},
	}, nil
}

func (c *Converter) convSeq(expr *ast.Seq) (Producer, error) {
	head := expr.Exprs[0]
	tail := expr.Exprs[1:]

	if head, ok := head.(*ast.Let); ok {
		bind, err := c.ConvPattern(head.Bind)
		if err != nil {
			return nil, err
		}
		body, err := c.ConvExpr(head.Body)
		if err != nil {
			return nil, err
		}

		if len(tail) == 0 {
			return body, nil
		}

		ret := c.FreshName("let", head.Base().Location)

		tailProducer, err := c.convSeq(&ast.Seq{Exprs: tail})
		if err != nil {
			return nil, err
		}

		return &Do{
			Name: ret,
			Body: &Cut{
				Producer: body,
				Consumer: &Case{
					Clauses: []*Clause{
						{
							Pattern: bind,
							Body: &Cut{
								Producer: tailProducer,
								Consumer: &Var{Name: ret},
							},
						},
					},
				},
			},
		}, nil
	}

	producer, err := c.ConvExpr(head)
	if err != nil {
		return nil, err
	}

	if len(tail) == 0 {
		return producer, nil
	}

	ret := c.FreshName("seq", head.Base().Location)

	tailProducer, err := c.convSeq(&ast.Seq{Exprs: tail})
	if err != nil {
		return nil, err
	}

	hole := c.FreshName("_", head.Base().Location)

	return &Do{
		Name: ret,
		Body: &Cut{
			Producer: producer,
			Consumer: &Then{
				Name: hole,
				Body: &Cut{
					Producer: tailProducer,
					Consumer: &Var{Name: ret},
				},
			},
		},
	}, nil
}

func (c *Converter) convLambda(expr *ast.Lambda) (Producer, error) {
	cont := c.FreshName("lambda", expr.Base().Location) // return point of lambda

	c.env = &env{c.env, make(map[string]int)}
	params := make([]token.Token, len(expr.Params))
	for i, param := range expr.Params {
		unique := c.Fresh()
		params[i] = withUnique(param, unique)
		c.assign(param.Lexeme, unique)
	}

	body, err := c.ConvExpr(expr.Expr)
	if err != nil {
		return nil, err
	}

	c.env = c.env.parent

	return &Cocase{
		Methods: []*Method{
			{
				Name:   "ap",
				Params: params,
				Labels: []token.Token{cont},
				Body: &Cut{
					Producer: body,
					Consumer: &Var{Name: cont},
				},
			},
		},
	}, nil
}

func (c *Converter) convCase(expr *ast.Case) (Producer, error) {
	ret := c.FreshName("case", expr.Base().Location)

	if len(expr.Scrutinees) != 1 {
		panic("invalid number of scrutinees")
	}

	scrutinee, err := c.ConvExpr(expr.Scrutinees[0])
	if err != nil {
		return nil, err
	}

	clauses := make([]*Clause, len(expr.Clauses))
	for i, clause := range expr.Clauses {
		c.env = &env{c.env, make(map[string]int)}
		if len(clause.Patterns) != 1 {
			panic("invalid number of patterns")
		}

		pattern, err := c.ConvPattern(clause.Patterns[0])
		if err != nil {
			return nil, err
		}

		body, err := c.ConvExpr(clause.Expr)
		if err != nil {
			return nil, err
		}

		c.env = c.env.parent

		clauses[i] = &Clause{
			Pattern: pattern,
			Body: &Cut{
				Producer: body,
				Consumer: &Var{Name: ret},
			},
		}
	}

	return &Do{
		Name: ret,
		Body: &Cut{
			Producer: scrutinee,
			Consumer: &Case{
				Clauses: clauses,
			},
		},
	}, nil
}

func (c *Converter) convObject(expr *ast.Object) (Producer, error) {
	methods := make([]*Method, len(expr.Fields))
	for i, field := range expr.Fields {
		c.env = &env{c.env, make(map[string]int)}

		body, err := c.ConvExpr(field.Expr)
		if err != nil {
			return nil, err
		}

		c.env = c.env.parent

		ret := c.FreshName("object", expr.Base().Location) // return point of methods

		methods[i] = &Method{
			Name:   field.Name,
			Params: []token.Token{},
			Labels: []token.Token{ret},
			Body: &Cut{
				Producer: body,
				Consumer: &Var{Name: ret},
			},
		}
	}

	return &Cocase{
		Methods: methods,
	}, nil
}

func (c *Converter) ConvPattern(pattern ast.Node) (Pattern, error) {
	switch pattern := pattern.(type) {
	case *ast.Var:
		unique := c.Fresh()
		c.assign(pattern.Name.Lexeme, unique)

		return &Var{Name: withUnique(pattern.Name, unique)}, nil
	case *ast.Literal:
		return &Literal{Token: pattern.Token}, nil
	case *ast.Symbol:
		return &Symbol{Name: pattern.Name}, nil
	case *ast.Tuple:
		patterns := make([]Pattern, len(pattern.Exprs))
		for i, expr := range pattern.Exprs {
			p, err := c.ConvPattern(expr)
			if err != nil {
				return nil, err
			}

			patterns[i] = p
		}

		hole := c.FreshName("_", pattern.Base().Location)

		return &Extract{
			Target: &Symbol{
				Name: token.Token{
					Kind:     token.IDENT,
					Lexeme:   "tuple",
					Location: pattern.Base().Location,
					Literal:  nil,
				},
			},
			Name:  "ap",
			Args:  patterns,
			Conts: []Pattern{&Var{Name: hole}},
		}, nil
	case *ast.Access:
		receiver, err := c.ConvPattern(pattern.Receiver)
		if err != nil {
			return nil, err
		}

		hole := c.FreshName("_", pattern.Base().Location)

		return &Extract{
			Target: receiver,
			Name:   pattern.Name.Lexeme,
			Args:   []Pattern{},
			Conts:  []Pattern{&Var{Name: hole}},
		}, nil
	case *ast.Call:
		fun, err := c.ConvPattern(pattern.Func)
		if err != nil {
			return nil, err
		}

		args := make([]Pattern, len(pattern.Args))
		for i, arg := range pattern.Args {
			p, err := c.ConvPattern(arg)
			if err != nil {
				return nil, err
			}

			args[i] = p
		}

		hole := c.FreshName("_", pattern.Base().Location)

		return &Extract{
			Target: fun,
			Name:   "ap",
			Args:   args,
			Conts:  []Pattern{&Var{Name: hole}},
		}, nil
	}

	return nil, unexpectedNodeError{pattern, "pattern"}
}

func (c *Converter) apply(where token.Location, function Producer, args []ast.Node) (Producer, error) {
	ret := c.FreshName("ap", where)

	producers := make([]Producer, len(args))
	for i, arg := range args {
		producer, err := c.ConvExpr(arg)
		if err != nil {
			return nil, err
		}

		producers[i] = producer
	}

	// μa. < function | .ap(arg1, arg2, ...; a) >
	return &Do{
		Name: ret,
		Body: &Cut{
			Producer: function,
			Consumer: &Destruct{
				Name: "ap",
				Args: producers,
				Conts: []Consumer{
					&Var{Name: ret},
				},
			},
		},
	}, nil
}

type NotDefinedError struct {
	Name token.Token
}

func (e NotDefinedError) Error() string {
	return e.Name.Lexeme + " is not defined"
}
