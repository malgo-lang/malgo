package parser

import (
	"errors"
	"fmt"
	"os"

	"github.com/takoeight0821/malgo/ast"
	"github.com/takoeight0821/malgo/scanner"
	"github.com/takoeight0821/malgo/token"
	"github.com/takoeight0821/malgo/utils"
)

//go:generate go run ../tools/main.go -comment -in parser.go -out ../docs/syntax.ebnf

type Parser struct {
	lex     *scanner.Scanner
	current token.Token
	prev    token.Token
}

func NewParser(lex *scanner.Scanner) (*Parser, error) {
	current, err := lex.Next()

	return &Parser{lex, current, token.Token{}}, err
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

// decl = varDecl | infixDecl | expr ;
func (p *Parser) decl() (ast.Node, error) {
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

// varDecl = "def" IDENT "=" expr ;
func (p *Parser) varDecl() (*ast.VarDecl, error) {
	if _, err := p.consume(token.DEF); err != nil {
		return nil, err
	}
	var name token.Token
	var err error
	switch {
	case p.match(token.IDENT):
		name, err = p.advance()
		if err != nil {
			return nil, err
		}
	case p.match(token.OPERATOR):
		name, err = p.advance()
		if err != nil {
			return nil, err
		}
	default:
		return nil, unexpectedToken(p.peek(), token.IDENT, token.OPERATOR)
	}
	_, err = p.consume(token.EQUAL)
	if err != nil {
		return nil, err
	}

	expr, err := p.expr()
	if err != nil {
		return nil, err
	}

	return &ast.VarDecl{Name: name, Expr: expr}, nil
}

// infixDecl = ("infix" | "infixl" | "infixr") INTEGER OPERATOR ;
func (p *Parser) infixDecl() (*ast.InfixDecl, error) {
	kind, err := p.advance()
	if err != nil {
		return nil, err
	}
	if kind.Kind != token.INFIX && kind.Kind != token.INFIXL && kind.Kind != token.INFIXR {
		return nil, unexpectedToken(kind, token.INFIX, token.INFIXL, token.INFIXR)
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

// expr = let | with | binary ;
func (p *Parser) expr() (ast.Node, error) {
	if p.IsAtEnd() {
		return nil, unexpectedEOF()
	}
	if p.match(token.LET) {
		return p.let()
	}
	if p.match(token.WITH) {
		return p.with()
	}

	return p.binary()
}

// let = "let" pattern "=" binary ;
func (p *Parser) let() (*ast.Let, error) {
	p.advance()
	pattern, err := p.pattern()
	if err != nil {
		return nil, err
	}
	if _, err := p.consume(token.EQUAL); err != nil {
		return nil, err
	}
	expr, err := p.binary()
	if err != nil {
		return nil, err
	}

	return &ast.Let{Bind: pattern, Body: expr}, nil
}

// with = "with" withBind "<-" binary | "with" binary ;
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
	}, func(_ error) ([]ast.Node, error) {
		return []ast.Node{}, nil
	})
	if err != nil {
		return nil, err
	}

	expr, err := p.binary()
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
// symbol = SYMBOL ;
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
	case token.SYMBOL:
		p.advance()

		return &ast.Symbol{Name: tok}, nil
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
			return nil, unexpectedToken(args[0].Base(), token.IDENT)
		}
		name := args[0].Base()
		args = args[1:]

		return &ast.Prim{Name: name, Args: args}, nil
	default:
		return nil, unexpectedToken(
			tok,
			token.IDENT,
			token.INTEGER,
			token.STRING,
			token.LEFTPAREN,
			token.LEFTBRACKET,
			token.LEFTBRACE,
			token.PRIM,
		)
	}
}

// binary = method (operator method)* ;
func (p *Parser) binary() (ast.Node, error) {
	expr, err := p.method()
	if err != nil {
		return nil, err
	}
	for p.match(token.OPERATOR) {
		op, err := p.advance()
		if err != nil {
			return nil, err
		}
		right, err := p.method()
		if err != nil {
			return nil, err
		}
		expr = &ast.Binary{Left: expr, Op: op, Right: right}
	}

	return expr, nil
}

// method = atom (accessTail | callTail | blockCallTail)* ;
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
		case p.match(token.LEFTBRACE):
			expr, err = p.blockCallTail(expr)
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

// blockCallTail = codata ;
func (p *Parser) blockCallTail(fun ast.Node) (ast.Node, error) {
	arg, err := p.codata()
	if err != nil {
		return nil, err
	}

	return &ast.Call{Func: fun, Args: []ast.Node{arg}}, nil
}

// codata = "{" clause ("," clause)* ","? "}" ;
func (p *Parser) codata() (*ast.Codata, error) {
	p.advance()

	clause, isOnlyBody, err := p.clause()
	if err != nil {
		return nil, err
	}
	if isOnlyBody {
		if _, err := p.consume(token.RIGHTBRACE); err != nil {
			return nil, fmt.Errorf("%w\nhint: add parentheses around the parameter list", err)
		}

		return &ast.Codata{Clauses: []*ast.CodataClause{clause}}, nil
	}

	clauses := []*ast.CodataClause{clause}
	for p.match(token.COMMA) {
		p.advance()
		if p.match(token.RIGHTBRACE) {
			break
		}
		clause, isOnlyBody, err := p.clause()
		if err != nil {
			return nil, err
		}
		if isOnlyBody {
			return nil, unexpectedOnlyBodyClause(clause.Base())
		}
		clauses = append(clauses, clause)
	}
	if _, err := p.consume(token.RIGHTBRACE); err != nil {
		return nil, err
	}

	return &ast.Codata{Clauses: clauses}, nil
}

type UnexpectedOnlyBodyClauseError struct{}

func (e UnexpectedOnlyBodyClauseError) Error() string {
	return "unexpected only body clause"
}

func unexpectedOnlyBodyClause(token token.Token) error {
	return utils.PosError{Where: token, Err: UnexpectedOnlyBodyClauseError{}}
}

// clause = clauseHead "->" clauseBody | clauseBody ;
// clauseHead = "(" ")" | "(" pattern ("," pattern)* ","? ")" | pattern ;
// clauseBody = expr (";" expr)* ";"? ;
func (p *Parser) clause() (*ast.CodataClause, bool, error) {
	var isOnlyBody bool
	// try to parse `clauseHead "->"`
	pattern, perr := try(p, func() (ast.Node, error) {
		pattern, err := p.clauseHead()
		if err != nil {
			return nil, err
		}

		if _, err := p.consume(token.ARROW); err != nil {
			return nil, err
		}

		isOnlyBody = false

		return pattern, nil
	}, func(err error) (ast.Node, error) {
		// if the parsing is failed, insert `#() ->` as pattern and go back to the original position.
		isOnlyBody = true

		return &ast.Call{Func: &ast.This{Token: p.peek()}, Args: []ast.Node{}}, err
	})

	expr, err := p.expr()
	if err != nil {
		return nil, isOnlyBody, errors.Join(perr, err)
	}
	exprs := []ast.Node{expr}
	for p.match(token.SEMICOLON) {
		p.advance()
		if p.match(token.RIGHTBRACE) {
			break
		}
		expr, err := p.expr()
		if err != nil {
			return nil, isOnlyBody, err
		}
		exprs = append(exprs, expr)
	}

	return &ast.CodataClause{Pattern: pattern, Expr: &ast.Seq{Exprs: exprs}}, isOnlyBody, nil
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
		return nil, unexpectedEOF()
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
	case token.SYMBOL:
		p.advance()

		return &ast.Symbol{Name: tok}, nil
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
			token.SHARP,
			token.IDENT,
			token.INTEGER,
			token.STRING,
			token.LEFTPAREN,
			token.LEFTBRACKET,
		)
	}
}

// peek returns the current token in the token stream without consuming it.
func (p Parser) peek() token.Token {
	return p.current
}

// advance moves the parser to the next token in the token stream.
// It returns the current token before advancing.
func (p *Parser) advance() (token.Token, error) {
	var err error = nil

	if !p.IsAtEnd() {
		p.prev = p.current
		p.current, err = p.lex.Next()
	}

	return p.previous(), err
}

// previous returns the previous token in the token stream.
func (p Parser) previous() token.Token {
	return p.prev
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
		return p.advance()
	}

	return p.peek(), unexpectedToken(p.peek(), kind)
}

type UnexpectedTokenError struct {
	Expected []token.Kind
	Actual   token.Token
}

func (e UnexpectedTokenError) Error() string {
	var msg string
	if len(e.Expected) >= 1 {
		msg = e.Expected[0].String()
	}

	for _, ex := range e.Expected[1:] {
		msg = msg + ", " + ex.String()
	}

	return "unexpected token: expected " + msg + ", got " + e.Actual.String()
}

func unexpectedToken(t token.Token, expected ...token.Kind) error {
	return utils.PosError{Where: t, Err: UnexpectedTokenError{Expected: expected, Actual: t}}
}

type UnexpectedEOFError struct{}

func (e UnexpectedEOFError) Error() string {
	return "unexpected EOF"
}

func unexpectedEOF() error {
	return UnexpectedEOFError{}
}

func try[T any](p *Parser, action func() (T, error), handler func(error) (T, error)) (T, error) {
	savedLex := *p.lex
	savedCurrent := p.current
	savedPrev := p.prev

	node, err := action()
	if err != nil {
		p.lex = &savedLex
		p.current = savedCurrent
		p.prev = savedPrev

		node, rerr := handler(err)
		if rerr != nil {
			return node, rerr
		}

		return node, nil
	}

	return node, nil
}
