package parser

import (
	"fmt"
	"os"
	"strconv"
	"strings"

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

type CompileError interface {
	Input() string
	Pos() int
}

func Line(err CompileError) string {
	msg := ""

	line := 1
	column := 1
	start := 0

	for i := 0; i < err.Pos(); i++ {
		if err.Input()[i] == '\n' {
			start = i + 1
			line++
			column = 1
		} else {
			column++
		}
	}

	msg += fmt.Sprintf("error at %d:%d\n\n", line, column)

	startFrom := err.Input()[start:]
	cutNewline := strings.Split(startFrom, "\n")[0]
	msg += fmt.Sprintf("%s\n", cutNewline)

	for i := 0; i < column-1; i++ {
		if startFrom[i] == '\t' {
			msg += "\t"
		} else {
			msg += " "
		}
	}

	msg += "^\n"

	return msg
}

func PrintLine(err CompileError) {
	fmt.Fprint(os.Stderr, Line(err))
}

func Error(err CompileError) {
	PrintLine(err)
	panic(err)
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

type UnknownCharError struct {
	input     string
	cursor    int
	character byte
}

func (e UnknownCharError) Input() string {
	return e.input
}

func (e UnknownCharError) Pos() int {
	return e.cursor
}

func (e UnknownCharError) Error() string {
	return fmt.Sprintf("unknown character %c", e.character)
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
		Error(UnknownCharError{input: l.input, cursor: l.cursor, character: l.input[l.cursor]})
	}
	return Token{}
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
	input    string
	cursor   int
	Expected TokenKind
	Actual   TokenKind
}

func (e ExpectTokenError) Input() string {
	return e.input
}

func (e ExpectTokenError) Pos() int {
	return e.cursor
}

func (e ExpectTokenError) Error() string {
	return fmt.Sprintf("expected %v, but got %v", e.Expected, e.Actual)
}

func (p Parser) expect(kind TokenKind) CompileError {
	return ExpectTokenError{input: p.lexer.input, cursor: p.token.pos, Expected: kind, Actual: p.token.kind}
}

type ExpectAtomError struct {
	input  string
	cursor int
	Actual TokenKind
}

func (e ExpectAtomError) Input() string {
	return e.input
}

func (e ExpectAtomError) Pos() int {
	return e.cursor
}

func (e ExpectAtomError) Error() string {
	return fmt.Sprintf("expected atom, but got %v", e.Actual)
}

func (p Parser) expectAtom() CompileError {
	return ExpectAtomError{input: p.lexer.input, cursor: p.token.pos, Actual: p.token.kind}
}

type ExpectPatternError struct {
	input  string
	cursor int
	Expr   ast.Node
}

func (e ExpectPatternError) Input() string {
	return e.input
}

func (e ExpectPatternError) Pos() int {
	return e.cursor
}

func (e ExpectPatternError) Error() string {
	return fmt.Sprintf("expected pattern, but got %s", e.Expr)
}

func (p Parser) expectPattern(expr ast.Node) CompileError {
	return ExpectPatternError{input: p.lexer.input, cursor: p.token.pos, Expr: expr}
}

type InvalidLiteralError struct {
	input  string
	cursor int
	Value  string
}

func (e InvalidLiteralError) Input() string {
	return e.input
}

func (e InvalidLiteralError) Pos() int {
	return e.cursor
}

func (e InvalidLiteralError) Error() string {
	return fmt.Sprintf("invalid literal %s", e.Value)
}

func (p Parser) invalidLiteral() CompileError {
	return InvalidLiteralError{input: p.lexer.input, cursor: p.token.pos, Value: p.token.value}
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
		Error(err)
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
func (p *Parser) parseAtom() (ast.Expr, CompileError) {
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
	case EOF, RPAREN, RBRACE, LBRACKET, RBRACKET, ARROW, COMMA: // fallthrough
	}
	return nil, p.expectAtom()
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
		Error(p.expect(ARROW))
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
	Error(p.expectPattern(expr))
	return nil
}
