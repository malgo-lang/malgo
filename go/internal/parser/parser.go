package parser

import (
	"fmt"
	"strconv"

	"github.com/takoeight0821/malgo/internal/ast"
)

type Lexer struct {
	input  string
	cursor int
}

func NewLexer(input string) *Lexer {
	return &Lexer{
		input:  input,
		cursor: 0,
	}
}

type TokenKind int

const (
	EOF TokenKind = iota
	IDENT
	NUMBER
	LPAREN   // (
	RPAREN   // )
	LBRACE   // {
	RBRACE   // }
	LBRACKET // [
	RBRACKET // ]
	ARROW    // ->
	COMMA    // ,
	SHARP    // #
)

func (k TokenKind) String() string {
	symbolMap := map[TokenKind]string{
		EOF:      "EOF",
		IDENT:    "IDENT",
		NUMBER:   "NUMBER",
		LPAREN:   "(",
		RPAREN:   ")",
		LBRACE:   "{",
		RBRACE:   "}",
		LBRACKET: "[",
		RBRACKET: "]",
		ARROW:    "->",
		COMMA:    ",",
		SHARP:    "#",
	}

	return symbolMap[k]
}

type Token struct {
	kind  TokenKind
	value string
	pos   int
}

func (l *Lexer) NewToken() Token {
	if l.cursor >= len(l.input) {
		token := Token{kind: EOF, value: "", pos: l.cursor}
		return token
	}

	switch l.input[l.cursor] {
	// skip whitespace
	case ' ', '\t', '\n':
		l.cursor++
		return l.NewToken()
	case '(':
		token := Token{kind: LPAREN, value: "(", pos: l.cursor}
		l.cursor++
		return token
	case ')':
		token := Token{kind: RPAREN, value: ")", pos: l.cursor}
		l.cursor++
		return token
	case '{':
		token := Token{kind: LBRACE, value: "{", pos: l.cursor}
		l.cursor++
		return token
	case '}':
		token := Token{kind: RBRACE, value: "}", pos: l.cursor}
		l.cursor++
		return token
	case '[':
		token := Token{kind: LBRACKET, value: "[", pos: l.cursor}
		l.cursor++
		return token
	case ']':
		token := Token{kind: RBRACKET, value: "]", pos: l.cursor}
		l.cursor++
		return token
	case ',':
		token := Token{kind: COMMA, value: ",", pos: l.cursor}
		l.cursor++
		return token
	case '#':
		token := Token{kind: SHARP, value: "#", pos: l.cursor}
		l.cursor++
		return token
	case '-':
		if l.cursor+1 < len(l.input) && l.input[l.cursor+1] == '>' {
			token := Token{kind: ARROW, value: "->", pos: l.cursor}
			l.cursor += 2
			return token
		}
		fallthrough
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		token := Token{kind: NUMBER, value: "", pos: l.cursor}
		for l.cursor < len(l.input) && l.input[l.cursor] >= '0' && l.input[l.cursor] <= '9' {
			token.value += string(l.input[l.cursor])
			l.cursor++
		}
		return token
	default:
		if isIdentStart(l.input[l.cursor]) {
			token := Token{kind: IDENT, value: "", pos: l.cursor}
			for l.cursor < len(l.input) && isIdentPart(l.input[l.cursor]) {
				token.value += string(l.input[l.cursor])
				l.cursor++
			}
			return token
		}
		panic("unknown character %c" + string(l.input[l.cursor]))
	}
}

func isIdentStart(c byte) bool {
	return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_'
}

func isIdentPart(c byte) bool {
	return isIdentStart(c) || ('0' <= c && c <= '9')
}

type Parser struct {
	lexer *Lexer
	token Token
}

func NewParser(input string) *Parser {
	lexer := NewLexer(input)
	token := lexer.NewToken()
	return &Parser{
		lexer: lexer,
		token: token,
	}
}

func (p *Parser) nextToken() {
	p.token = p.lexer.NewToken()
}

type ExpectTokenError struct {
	Expected TokenKind
	Actual   TokenKind
	Pos      int
}

func (e ExpectTokenError) Error() string {
	return fmt.Sprintf("at %d: expected %v, but got %v", e.Pos, e.Expected, e.Actual)
}

func (p Parser) expect(kind TokenKind) error {
	// TODO: show line number and column number
	return ExpectTokenError{Expected: kind, Actual: p.token.kind, Pos: p.token.pos}
}

type ExpectAtomError struct {
	Actual TokenKind
	Pos    int
}

func (e ExpectAtomError) Error() string {
	return fmt.Sprintf("at %d: expected atom, but got %v", e.Pos, e.Actual)
}

func (p Parser) expectAtom() error {
	return ExpectAtomError{Actual: p.token.kind, Pos: p.token.pos}
}

type ExpectPatternError struct {
	Expr ast.Node
	Pos  int
}

func (e ExpectPatternError) Error() string {
	return fmt.Sprintf("at %d: expected pattern, but got %s", e.Pos, e.Expr)
}

func (p Parser) expectPattern(expr ast.Node) error {
	return ExpectPatternError{Expr: expr, Pos: p.token.pos}
}

type InvalidLiteralError struct {
	Value string
	Pos   int
}

func (e InvalidLiteralError) Error() string {
	return fmt.Sprintf("at %d: invalid literal %s", e.Pos, e.Value)
}

func (p Parser) invalidLiteral() error {
	return InvalidLiteralError{Value: p.token.value, Pos: p.token.pos}
}

// program -> expr
func (p *Parser) Parse() ast.Node {
	return p.parseExpr()
}

// expr -> apply
func (p *Parser) parseExpr() ast.Expr {
	return p.parseApply()
}

// apply -> atom atom*
func (p *Parser) parseApply() ast.Expr {
	fun, err := p.parseAtom()
	if err != nil {
		panic(err)
	}

	// Save the cursor for backtracking
	cursor := p.lexer.cursor

	args := []ast.Node{}
	for {
		arg, err := p.parseAtom()
		if err != nil {
			p.lexer.cursor = cursor
			break
		}
		// Update the cursor for backtracking
		cursor = p.lexer.cursor
		args = append(args, arg)
	}

	if len(args) == 0 {
		return fun
	}
	return ast.NewApply(fun, args)
}

// atom -> ident | number | "(" expr ")" | "{" codata "}"
func (p *Parser) parseAtom() (ast.Expr, error) {
	switch p.token.kind {
	case IDENT:
		expr := ast.NewVariable(ast.String(p.token.value), p.token.pos)
		p.nextToken()
		return expr, nil
	case NUMBER:
		value, err := strconv.Atoi(p.token.value)
		if err != nil {
			return nil, p.invalidLiteral()
		}
		expr := ast.NewLiteral(value, p.token.pos)
		p.nextToken()
		return expr, nil
	case SHARP:
		expr := ast.NewThis(p.token.pos)
		p.nextToken()
		return expr, nil
	case LPAREN:
		p.nextToken()
		expr := p.parseExpr()
		if p.token.kind != RPAREN {
			return nil, p.expect(RPAREN)
		}
		p.nextToken()
		return expr, nil
	case LBRACE:
		p.nextToken()
		expr := p.parseCodata()
		if p.token.kind != RBRACE {
			return nil, p.expect(RBRACE)
		}
		p.nextToken()
		return expr, nil
	case EOF, RPAREN, RBRACE, LBRACKET, RBRACKET, ARROW, COMMA:
		fallthrough
	default:
		return nil, p.expectAtom()
	}
}

// codata -> clause ("," clause)* ","?
func (p *Parser) parseCodata() ast.Expr {
	pos := p.token.pos
	clauses := []ast.Clause{}
	for {
		clause := p.parseClause()
		clauses = append(clauses, clause)
		if p.token.kind != COMMA {
			break
		}
		p.nextToken()
	}
	// skip optional comma
	if p.token.kind == COMMA {
		p.nextToken()
	}
	return ast.NewCodata(clauses, pos)
}

// clause -> pattern "->" expr
func (p *Parser) parseClause() ast.Clause {
	pattern := p.parsePattern()
	if p.token.kind != ARROW {
		panic(p.expect(ARROW))
	}
	p.nextToken()
	body := p.parseExpr()
	return ast.NewClause(pattern, body)
}

// pattern -> expr
func (p *Parser) parsePattern() ast.Pattern {
	expr := p.parseExpr()
	pattern, ok := expr.(ast.Pattern)
	if ok {
		return pattern
	}
	panic(p.expectPattern(expr))
}
