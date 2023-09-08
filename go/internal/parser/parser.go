package parser

import (
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
		} else {
			panic("unknown character %c" + string(l.input[l.cursor]))
		}
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

func (p *Parser) Parse() ast.Node {
	return p.parseExpr()
}

func (p *Parser) parseExpr() ast.Expr {
	switch p.token.kind {
	case IDENT:
		return p.parseVariable()
	case NUMBER:
		return p.parseLiteral()
	default:
		panic("unexpected token " + p.token.value)
	}
}

func (p *Parser) parseVariable() ast.Variable {
	variable := ast.NewVariable(p.token.value, p.token.pos)
	p.nextToken()
	return variable
}

func (p *Parser) parseLiteral() ast.Literal {
	value, err := strconv.Atoi(p.token.value)
	if err != nil {
		panic(err)
	}
	literal := ast.NewLiteral(value, p.token.pos)
	p.nextToken()
	return literal
}
