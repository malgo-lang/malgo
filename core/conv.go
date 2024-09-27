package core

import (
	"github.com/malgo-lang/malgo/ast"
	"github.com/malgo-lang/malgo/token"
	"github.com/malgo-lang/malgo/utils"
)

type Converter struct {
	toplevels map[token.Token]*Def
	env       *env
	unique    int
}

type env struct {
	parent *env
	table  map[string]int
}

func NewConverter() *Converter {
	return &Converter{
		toplevels: make(map[token.Token]*Def),
		env:       &env{nil, make(map[string]int)},
		unique:    0,
	}
}

func (c *Converter) fresh() int {
	u := c.unique
	c.unique++

	return u
}

func (c *Converter) freshName(name token.Token) token.Token {
	return token.Token{
		Kind:     token.IDENT,
		Lexeme:   name.Lexeme,
		Location: name.Location,
		Literal:  c.fresh(),
	}
}

func (c *Converter) newName(lexeme string, base token.Location) token.Token {
	return token.Token{
		Kind:     token.IDENT,
		Lexeme:   lexeme,
		Location: base,
		Literal:  c.fresh(),
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

func (c *Converter) ConvDef(decl *ast.VarDecl) error {
	name := c.freshName(decl.Name)
	ret := c.newName("ret", decl.Base().Location)
	body, err := c.ConvExpr(decl.Expr)
	if err != nil {
		return err
	}

	c.toplevels[name] = &Def{
		Name:    name,
		Returns: []token.Token{ret},
		Body: &Cut{
			Producer: body,
			Consumer: &Var{Name: ret},
		},
	}

	return nil
}

func (c *Converter) ConvExpr(expr ast.Node) (Producer, error) {
	switch e := expr.(type) {
	case *ast.Var:
		if name, ok := c.lookup(e.Name.Lexeme); ok {
			return &Var{Name: withUnique(e.Name, name)}, nil
		}

		return nil, utils.PosError{
			Where: e.Name,
			Err:   NotDefinedError{Name: e.Name},
		}
	case *ast.Literal:
		return &Literal{Token: e.Token}, nil
	case *ast.Symbol:
		return &Symbol{Name: e.Name}, nil
	case *ast.Tuple:
		return c.construct(e.Base().Location, &Symbol{token.Token{
			Kind:     token.IDENT,
			Lexeme:   "tuple",
			Location: e.Base().Location,
			Literal:  nil,
		}}, e.Exprs)
	}

	panic("unreachable")
}

func (c *Converter) construct(where token.Location, constructor Producer, args []ast.Node) (Producer, error) {
	ret := c.newName("c", where)

	producers := make([]Producer, len(args))
	for i, arg := range args {
		producer, err := c.ConvExpr(arg)
		if err != nil {
			return nil, err
		}

		producers[i] = producer
	}

	// μc. < constructor | .ap(producer1, producer2, ...; c) >
	return &Do{
		Name: ret,
		Body: &Cut{
			Producer: constructor,
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
