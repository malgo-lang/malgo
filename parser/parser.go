package parser

import (
	"errors"
	"fmt"
	"os"

	"github.com/takoeight0821/malgo/ast"
	"github.com/takoeight0821/malgo/token"
	"github.com/takoeight0821/malgo/utils"
)

//go:generate go run ../tools/main.go -comment -in parser.go -out ../docs/syntax.ebnf

type Parser struct {
	tokens  []token.Token
	current int
}

func NewParser(tokens []token.Token) *Parser {
	return &Parser{tokens, 0}
}

func (p *Parser) ParseExpr() (ast.Node, error) {
	return p.expr()
}

func (p *Parser) ParseDecl() ([]ast.Node, error) {
	nodes := []ast.Node{}
	for !p.IsAtEnd() {
		node, err := p.decl()
		if err != nil {
			return nil, err
		}
		nodes = append(nodes, node)
	}

	return nodes, nil
}

// decl = dataDecl | typeDecl | varDecl | infixDecl ;
func (p *Parser) decl() (ast.Node, error) {
	if p.match(token.DATA) {
		return p.dataDecl()
	}
	if p.match(token.TYPE) {
		return p.typeDecl()
	}
	if p.match(token.DEF) {
		return p.varDecl()
	}

	return p.infixDecl()
}

func commaSeparated[T any](
	parser *Parser,
	startToken, endToken token.Kind,
	parseElement func(*Parser) (T, error),
) ([]T, error) {
	elements := []T{}
	if _, err := parser.consume(startToken); err != nil {
		return elements, err
	}

	if !parser.match(endToken) {
		element, err := parseElement(parser)
		if err != nil {
			return elements, err
		}
		elements = append(elements, element)
		for parser.match(token.COMMA) {
			parser.advance()
			if parser.match(endToken) {
				break
			}
			element, err := parseElement(parser)
			if err != nil {
				return elements, err
			}
			elements = append(elements, element)
		}
	}

	if _, err := parser.consume(endToken); err != nil {
		return elements, err
	}

	return elements, nil
}

// dataDecl = "data" IDENT (typeparams1)? "=" "{" constructor ("," constructor)* ","? "}" ;
// typeparams1 = "(" IDENT ("," IDENT)* ","? ")" ;
func (p *Parser) dataDecl() (*ast.TypeDecl, error) {
	if _, err := p.consume(token.DATA); err != nil {
		return nil, err
	}
	typename, err := p.consume(token.IDENT)
	if err != nil {
		return nil, err
	}

	var def ast.Node
	def = &ast.Var{Name: typename}
	if p.match(token.LEFTPAREN) {
		typeparams, err := commaSeparated(p, token.LEFTPAREN, token.RIGHTPAREN, func(p *Parser) (ast.Node, error) {
			name, err := p.consume(token.IDENT)
			if err != nil {
				return nil, err
			}

			return &ast.Var{Name: name}, nil
		})
		if err != nil {
			return nil, err
		}
		if len(typeparams) < 1 {
			return nil, unexpectedToken(p.previous(), "type parameter")
		}

		def = &ast.Call{Func: def, Args: typeparams}
	}

	if _, err := p.consume(token.EQUAL); err != nil {
		return nil, err
	}

	types, err := commaSeparated(p, token.LEFTBRACE, token.RIGHTBRACE, func(p *Parser) (ast.Node, error) {
		return p.constructor()
	})
	if err != nil {
		return nil, err
	}

	return &ast.TypeDecl{Def: def, Types: types}, nil
}

// typeDecl = "type" IDENT (typeparams1)? "=" type ;
func (p *Parser) typeDecl() (*ast.TypeDecl, error) {
	if _, err := p.consume(token.TYPE); err != nil {
		return nil, err
	}
	typename, err := p.consume(token.IDENT)
	if err != nil {
		return nil, err
	}

	var def ast.Node
	def = &ast.Var{Name: typename}
	if p.match(token.LEFTPAREN) {
		typeparams, err := commaSeparated(p, token.LEFTPAREN, token.RIGHTPAREN, func(p *Parser) (ast.Node, error) {
			name, err := p.consume(token.IDENT)
			if err != nil {
				return nil, err
			}

			return &ast.Var{Name: name}, nil
		})
		if err != nil {
			return nil, err
		}
		if len(typeparams) < 1 {
			return nil, unexpectedToken(p.previous(), "type parameter")
		}
		def = &ast.Call{Func: def, Args: typeparams}
	}

	if _, err := p.consume(token.EQUAL); err != nil {
		return nil, err
	}

	typ, err := p.typ()
	if err != nil {
		return nil, err
	}

	return &ast.TypeDecl{Def: def, Types: []ast.Node{typ}}, nil
}

// constructor = UPPER_IDENT typeparams ;
// typeparams = "(" (type ("," type)*)? ")" ;
func (p *Parser) constructor() (*ast.Call, error) {
	name, err := p.consume(token.IDENT)
	if err != nil {
		return nil, err
	}
	if !utils.IsUpper(name.Lexeme) {
		return nil, unexpectedToken(name, "identifier started with a upper-case character")
	}

	typeparams, err := commaSeparated(p, token.LEFTPAREN, token.RIGHTPAREN, func(p *Parser) (ast.Node, error) {
		return p.typ()
	})
	if err != nil {
		return nil, err
	}

	return &ast.Call{Func: &ast.Var{Name: name}, Args: typeparams}, nil
}

// varDecl = "def" IDENT "=" expr | "def" IDENT ":" type | "def" IDENT ":" type "=" expr ;
func (p *Parser) varDecl() (*ast.VarDecl, error) {
	if _, err := p.consume(token.DEF); err != nil {
		return nil, err
	}
	var name token.Token
	switch {
	case p.match(token.IDENT):
		name = p.advance()
	case p.match(token.OPERATOR):
		name = p.advance()
	default:
		return nil, unexpectedToken(p.peek(), "identifier", "operator")
	}
	var typ ast.Node
	var expr ast.Node
	var err error
	if p.match(token.COLON) {
		p.advance()
		typ, err = p.typ()
		if err != nil {
			return nil, err
		}
	}
	if p.match(token.EQUAL) {
		p.advance()
		expr, err = p.expr()
		if err != nil {
			return nil, err
		}
	}

	return &ast.VarDecl{Name: name, Type: typ, Expr: expr}, nil
}

// infixDecl = ("infix" | "infixl" | "infixr") INTEGER OPERATOR ;
func (p *Parser) infixDecl() (*ast.InfixDecl, error) {
	kind := p.advance()
	if kind.Kind != token.INFIX && kind.Kind != token.INFIXL && kind.Kind != token.INFIXR {
		return nil, unexpectedToken(p.peek(), "`infix`", "`infixl`", "`infixr`")
	}
	precedence, err := p.consume(token.INTEGER)
	if err != nil {
		return nil, err
	}
	name, err := p.consume(token.OPERATOR)
	if err != nil {
		return nil, err
	}

	return &ast.InfixDecl{Assoc: kind, Prec: precedence, Name: name}, nil
}

// expr = let | with | assert ;
func (p *Parser) expr() (ast.Node, error) {
	if p.IsAtEnd() {
		return nil, unexpectedToken(p.peek(), "expression")
	}
	if p.match(token.LET) {
		return p.let()
	}
	if p.match(token.WITH) {
		return p.with()
	}

	return p.assert()
}

// let = "let" pattern "=" assert ;
func (p *Parser) let() (*ast.Let, error) {
	p.advance()
	pattern, err := p.pattern()
	if err != nil {
		return nil, err
	}
	if _, err := p.consume(token.EQUAL); err != nil {
		return nil, err
	}
	expr, err := p.assert()
	if err != nil {
		return nil, err
	}

	return &ast.Let{Bind: pattern, Body: expr}, nil
}

// with = "with" withBind "<-" assert | "with" assert ;
// withBind = pattern ("," pattern)* "," ;
func (p *Parser) with() (*ast.With, error) {
	p.advance()

	patterns, err := try(p, func() ([]ast.Node, error) {
		var patterns []ast.Node
		pattern, err := p.pattern()
		if err != nil {
			return nil, err
		}
		patterns = append(patterns, pattern)

		for p.match(token.COMMA) {
			p.advance()
			if p.match(token.BACKARROW) {
				break
			}
			pattern, err := p.pattern()
			if err != nil {
				return nil, err
			}
			patterns = append(patterns, pattern)
		}

		if _, err := p.consume(token.BACKARROW); err != nil {
			return nil, err
		}

		return patterns, nil
	}, func() ([]ast.Node, error) {
		return []ast.Node{}, nil
	})
	if err != nil {
		return nil, err
	}

	expr, err := p.assert()
	if err != nil {
		return nil, err
	}

	if _, ok := expr.(*ast.Call); !ok {
		fmt.Fprintf(os.Stderr, "at %v: `%s`, warning: `with` expression should be a function call\n",
			expr.Base().Location, expr.Base().Lexeme)
	}

	return &ast.With{Binds: patterns, Body: expr}, nil
}

// atom = var | literal | paren | tuple | codata | PRIM "(" IDENT ("," expr)* ","? ")" ;
// var = IDENT ;
// literal = INTEGER | STRING ;
// paren = "(" ")" | "(" expr ")" ;
// tuple = "[" "]" | "[" expr ("," expr)* ","? "]" ;
// codata = "{" clause ("," clause)* ","? "}" ;
func (p *Parser) atom() (ast.Node, error) {
	//exhaustive:ignore
	switch tok := p.peek(); tok.Kind {
	case token.IDENT:
		p.advance()

		return &ast.Var{Name: tok}, nil
	case token.INTEGER, token.STRING:
		p.advance()

		return &ast.Literal{Token: tok}, nil
	case token.LEFTPAREN:
		p.advance()

		expr, err := p.expr()
		if err != nil {
			return nil, err
		}
		if _, err := p.consume(token.RIGHTPAREN); err != nil {
			return nil, err
		}

		return &ast.Paren{Expr: expr}, nil
	case token.LEFTBRACKET:
		exprs, err := commaSeparated(p, token.LEFTBRACKET, token.RIGHTBRACKET, func(p *Parser) (ast.Node, error) {
			return p.expr()
		})
		if err != nil {
			return nil, err
		}

		return &ast.Tuple{Exprs: exprs}, nil
	case token.LEFTBRACE:
		return p.codata()
	case token.PRIM:
		p.advance()
		args, err := commaSeparated(p, token.LEFTPAREN, token.RIGHTPAREN, func(p *Parser) (ast.Node, error) {
			return p.expr()
		})
		if err != nil {
			return nil, err
		}

		if args[0].Base().Kind != token.IDENT {
			return nil, unexpectedToken(args[0].Base(), token.IDENT.String())
		}
		name := args[0].Base()
		args = args[1:]

		return &ast.Prim{Name: name, Args: args}, nil
	default:
		return nil, unexpectedToken(
			tok,
			token.IDENT.String(),
			token.INTEGER.String(),
			token.STRING.String(),
			token.LEFTPAREN.String(),
			token.LEFTBRACKET.String(),
			token.LEFTBRACE.String(),
			token.PRIM.String(),
		)
	}
}

// assert = binary (":" type)* ;
func (p *Parser) assert() (ast.Node, error) {
	expr, err := p.binary()
	if err != nil {
		return nil, err
	}
	for p.match(token.COLON) {
		p.advance()
		typ, err := p.typ()
		if err != nil {
			return nil, err
		}
		expr = &ast.Assert{Expr: expr, Type: typ}
	}

	return expr, nil
}

// binary = method (operator method)* ;
func (p *Parser) binary() (ast.Node, error) {
	expr, err := p.method()
	if err != nil {
		return nil, err
	}
	for p.match(token.OPERATOR) {
		op := p.advance()
		right, err := p.method()
		if err != nil {
			return nil, err
		}
		expr = &ast.Binary{Left: expr, Op: op, Right: right}
	}

	return expr, nil
}

// method = atom (accessTail | callTail)* ;
func (p *Parser) method() (ast.Node, error) {
	expr, err := p.atom()
	if err != nil {
		return nil, err
	}
	for {
		switch {
		case p.match(token.DOT):
			expr, err = p.accessTail(expr)
			if err != nil {
				return nil, err
			}
		case p.match(token.LEFTPAREN):
			expr, err = p.callTail(expr)
			if err != nil {
				return nil, err
			}
		default:
			return expr, nil
		}
	}
}

// accessTail = "." IDENT callTail? ;
func (p *Parser) accessTail(receiver ast.Node) (ast.Node, error) {
	if _, err := p.consume(token.DOT); err != nil {
		return nil, err
	}
	name, err := p.consume(token.IDENT)
	if err != nil {
		return nil, err
	}
	expr := &ast.Access{Receiver: receiver, Name: name}

	if p.match(token.LEFTPAREN) {
		return p.callTail(expr)
	}

	return expr, nil
}

// callTail = "(" ")" | "(" expr ("," expr)* ","? ")" ;
func (p *Parser) callTail(fun ast.Node) (ast.Node, error) {
	args, err := commaSeparated(p, token.LEFTPAREN, token.RIGHTPAREN, func(p *Parser) (ast.Node, error) {
		return p.expr()
	})
	if err != nil {
		return nil, err
	}

	return &ast.Call{Func: fun, Args: args}, nil
}

// codata = "{" clause ("," clause)* ","? "}" ;
func (p *Parser) codata() (*ast.Codata, error) {
	p.advance()

	clause, err := p.clause()
	if err != nil {
		return nil, err
	}
	clauses := []*ast.CodataClause{clause}
	for p.match(token.COMMA) {
		p.advance()
		if p.match(token.RIGHTBRACE) {
			break
		}
		clause, err := p.clause()
		if err != nil {
			return nil, err
		}
		clauses = append(clauses, clause)
	}
	if _, err := p.consume(token.RIGHTBRACE); err != nil {
		return nil, err
	}

	return &ast.Codata{Clauses: clauses}, nil
}

// clause = clauseHead "->" clauseBody | clauseBody ;
// clauseHead = "(" ")" | "(" pattern ("," pattern)* ","? ")" | pattern ;
// clauseBody = expr (";" expr)* ";"? ;
func (p *Parser) clause() (*ast.CodataClause, error) {
	// try to parse `clauseHead "->"`
	pattern, err := try(p, func() (ast.Node, error) {
		pattern, err := p.clauseHead()
		if err != nil {
			return nil, err
		}

		if _, err := p.consume(token.ARROW); err != nil {
			return nil, err
		}

		return pattern, nil
	}, func() (ast.Node, error) {
		// if the parsing is failed, insert `#() ->` as pattern and go back to the original position.
		return &ast.Call{Func: &ast.This{Token: p.peek()}, Args: []ast.Node{}}, nil
	})
	if err != nil {
		return nil, err
	}

	expr, err := p.expr()
	if err != nil {
		return nil, err
	}
	exprs := []ast.Node{expr}
	for p.match(token.SEMICOLON) {
		p.advance()
		if p.match(token.RIGHTBRACE) {
			break
		}
		expr, err := p.expr()
		if err != nil {
			return nil, err
		}
		exprs = append(exprs, expr)
	}

	return &ast.CodataClause{Pattern: pattern, Expr: &ast.Seq{Exprs: exprs}}, nil
}

func (p *Parser) clauseHead() (ast.Node, error) {
	//nolint:exhaustive
	switch p.peek().Kind {
	case token.SHARP:
		return p.pattern()
	case token.LEFTPAREN:
		tok := p.peek()
		params, err := commaSeparated(p, token.LEFTPAREN, token.RIGHTPAREN, func(p *Parser) (ast.Node, error) {
			return p.pattern()
		})
		if err != nil {
			return nil, err
		}

		return &ast.Call{Func: &ast.This{Token: tok}, Args: params}, nil
	default:
		tok := p.peek()
		arg, err := p.pattern()
		if err != nil {
			return nil, err
		}

		return &ast.Call{Func: &ast.This{Token: tok}, Args: []ast.Node{arg}}, nil
	}
}

// pattern = methodPat ;
func (p *Parser) pattern() (ast.Node, error) {
	if p.IsAtEnd() {
		return nil, unexpectedToken(p.peek(), "pattern")
	}

	return p.methodPat()
}

// methodPat = atomPat (accessPatTail | callPatTail)* ;
func (p *Parser) methodPat() (ast.Node, error) {
	pat, err := p.atomPat()
	if err != nil {
		return nil, err
	}
	for {
		switch {
		case p.match(token.DOT):
			pat, err = p.accessPatTail(pat)
			if err != nil {
				return nil, err
			}
		case p.match(token.LEFTPAREN):
			pat, err = p.callPatTail(pat)
			if err != nil {
				return nil, err
			}
		default:
			return pat, nil
		}
	}
}

// accessPatTail = "." IDENT callPatTail? ;
func (p *Parser) accessPatTail(receiver ast.Node) (ast.Node, error) {
	if _, err := p.consume(token.DOT); err != nil {
		return nil, err
	}
	name, err := p.consume(token.IDENT)
	if err != nil {
		return nil, err
	}
	pat := &ast.Access{Receiver: receiver, Name: name}

	if p.match(token.LEFTPAREN) {
		return p.callPatTail(pat)
	}

	return pat, nil
}

// callPatTail = "(" ")" | "(" pattern ("," pattern)* ","? ")" ;
func (p *Parser) callPatTail(fun ast.Node) (ast.Node, error) {
	args, err := commaSeparated(p, token.LEFTPAREN, token.RIGHTPAREN, func(p *Parser) (ast.Node, error) {
		return p.pattern()
	})
	if err != nil {
		return nil, err
	}

	return &ast.Call{Func: fun, Args: args}, nil
}

// atomPat = IDENT | INTEGER | STRING | "(" pattern ")" | tuplePat ;
// tuplePat = "[" "]" | "[" pattern ("," pattern)* ","? "]" ;
func (p *Parser) atomPat() (ast.Node, error) {
	//exhaustive:ignore
	switch tok := p.peek(); tok.Kind {
	case token.SHARP:
		p.advance()

		return &ast.This{Token: tok}, nil
	case token.IDENT:
		p.advance()

		return &ast.Var{Name: tok}, nil
	case token.INTEGER, token.STRING:
		p.advance()

		return &ast.Literal{Token: tok}, nil
	case token.LEFTPAREN:
		p.advance()

		pat, err := p.pattern()
		if err != nil {
			return nil, err
		}
		if _, err := p.consume(token.RIGHTPAREN); err != nil {
			return nil, err
		}

		return &ast.Paren{Expr: pat}, nil
	case token.LEFTBRACKET:
		pats, err := commaSeparated(p, token.LEFTBRACKET, token.RIGHTBRACKET, func(p *Parser) (ast.Node, error) {
			return p.pattern()
		})
		if err != nil {
			return nil, err
		}

		return &ast.Tuple{Exprs: pats}, nil
	default:
		return nil, unexpectedToken(
			tok,
			token.SHARP.String(),
			token.IDENT.String(),
			token.INTEGER.String(),
			token.STRING.String(),
			token.LEFTPAREN.String(),
			token.LEFTBRACKET.String(),
		)
	}
}

// type = binopType ;
func (p *Parser) typ() (ast.Node, error) {
	if p.IsAtEnd() {
		return nil, unexpectedToken(p.peek(), "type")
	}

	return p.binopType()
}

// binopType = callType (operator callType)* ;
func (p *Parser) binopType() (ast.Node, error) {
	typ, err := p.callType()
	if err != nil {
		return nil, err
	}
	for p.match(token.OPERATOR) || p.match(token.ARROW) {
		op := p.advance()
		right, err := p.callType()
		if err != nil {
			return nil, err
		}
		typ = &ast.Binary{Left: typ, Op: op, Right: right}
	}

	return typ, nil
}

// callType = (PRIM "(" IDENT ("," type)* ","? ")" | atomType) ("(" ")" | "(" type ("," type)* ","? ")")* ;
func (p *Parser) callType() (ast.Node, error) {
	var typ ast.Node
	var err error
	if p.match(token.PRIM) {
		p.advance()
		args, err := commaSeparated(p, token.LEFTPAREN, token.RIGHTPAREN, func(p *Parser) (ast.Node, error) {
			return p.typ()
		})
		if err != nil {
			return nil, err
		}

		if args[0].Base().Kind != token.IDENT {
			return nil, unexpectedToken(args[0].Base(), token.IDENT.String())
		}

		name := args[0].Base()
		args = args[1:]

		typ = &ast.Prim{Name: name, Args: args}
	} else {
		typ, err = p.atomType()
		if err != nil {
			return nil, err
		}
	}

	for p.match(token.LEFTPAREN) {
		typ, err = p.callTypeTail(typ)
		if err != nil {
			return nil, err
		}
	}

	return typ, nil
}

func (p *Parser) callTypeTail(fun ast.Node) (*ast.Call, error) {
	args, err := commaSeparated(p, token.LEFTPAREN, token.RIGHTPAREN, func(p *Parser) (ast.Node, error) {
		return p.typ()
	})
	if err != nil {
		return nil, err
	}

	return &ast.Call{Func: fun, Args: args}, nil
}

// atomType = IDENT | "{" fieldType ("," fieldType)* ","? "}" | "(" type  ")" | tupleType ;
// tupleType = "[" "]" | "[" type ("," type)* ","? "]";
func (p *Parser) atomType() (ast.Node, error) {
	//exhaustive:ignore
	switch tok := p.peek(); tok.Kind {
	case token.IDENT:
		p.advance()

		return &ast.Var{Name: tok}, nil
	case token.LEFTBRACE:
		p.advance()
		fields, err := commaSeparated(p, token.LEFTBRACE, token.RIGHTBRACE, func(p *Parser) (*ast.Field, error) {
			return p.fieldType()
		})
		if err != nil {
			return nil, err
		}
		if len(fields) < 1 {
			return nil, unexpectedToken(p.previous(), "field")
		}

		return &ast.Object{Fields: fields}, nil
	case token.LEFTBRACKET:
		p.advance()
		types, err := commaSeparated(p, token.LEFTBRACKET, token.RIGHTBRACKET, func(p *Parser) (ast.Node, error) {
			return p.typ()
		})
		if err != nil {
			return nil, err
		}

		return &ast.Tuple{Exprs: types}, nil
	case token.LEFTPAREN:
		typ, err := p.typ()
		if err != nil {
			return nil, err
		}
		if _, err := p.consume(token.RIGHTPAREN); err != nil {
			return nil, err
		}

		return &ast.Paren{Expr: typ}, nil
	default:
		return nil, unexpectedToken(tok, "identifier", "`{`", "`(`")
	}
}

// fieldType = IDENT ":" type ;
func (p *Parser) fieldType() (*ast.Field, error) {
	name, err := p.consume(token.IDENT)
	if err != nil {
		return nil, err
	}

	if _, err := p.consume(token.COLON); err != nil {
		return nil, err
	}

	typ, err := p.typ()
	if err != nil {
		return nil, err
	}

	return &ast.Field{Name: name.Lexeme, Expr: typ}, nil
}

// peek returns the current token in the token stream without consuming it.
func (p Parser) peek() token.Token {
	return p.tokens[p.current]
}

// advance moves the parser to the next token in the token stream.
// It returns the current token before advancing.
func (p *Parser) advance() token.Token {
	if !p.IsAtEnd() {
		p.current++
	}

	return p.previous()
}

// previous returns the previous token in the token stream.
func (p Parser) previous() token.Token {
	return p.tokens[p.current-1]
}

// IsAtEnd checks if the parser has reached the end of the input.
func (p Parser) IsAtEnd() bool {
	return p.peek().Kind == token.EOF
}

// match checks if the current token in the input stream has the specified kind.
// It returns true if the current token matches the specified kind, false otherwise.
func (p Parser) match(kind token.Kind) bool {
	if p.IsAtEnd() {
		return false
	}

	return p.peek().Kind == kind
}

func (p *Parser) consume(kind token.Kind) (token.Token, error) {
	if p.match(kind) {
		return p.advance(), nil
	}

	return p.peek(), unexpectedToken(p.peek(), kind.String())
}

type UnexpectedTokenError struct {
	Expected []string
}

func (e UnexpectedTokenError) Error() string {
	var msg string
	if len(e.Expected) >= 1 {
		msg = e.Expected[0]
	}

	for _, ex := range e.Expected[1:] {
		msg = msg + ", " + ex
	}

	return "unexpected token: expected " + msg
}

func unexpectedToken(t token.Token, expected ...string) error {
	return utils.PosError{Where: t, Err: UnexpectedTokenError{Expected: expected}}
}

func try[T any](p *Parser, action func() (T, error), handler func() (T, error)) (T, error) {
	savedCurrent := p.current

	node, err := action()
	if err != nil {
		p.current = savedCurrent

		node, rerr := handler()
		if rerr != nil {
			return node, errors.Join(err, rerr)
		}

		return node, nil
	}

	return node, nil
}
