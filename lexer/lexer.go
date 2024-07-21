package lexer

import (
	"errors"
	"fmt"
	"strconv"
	"unicode"
	"unicode/utf8"

	"github.com/takoeight0821/malgo/token"
	"github.com/takoeight0821/malgo/utils"
)

func Lex(filePath, source string) ([]token.Token, error) {
	lexer := lexer{
		source:  source,
		tokens:  []token.Token{},
		start:   0,
		current: 0,

		filePath: filePath,
		line:     1,
		column:   1,
	}

	var err error

	for !lexer.isAtEnd() {
		err = errors.Join(err, lexer.scanToken())
	}

	lexer.tokens = append(lexer.tokens, token.Token{Kind: token.EOF, Lexeme: "", Location: lexer.location(), Literal: nil})

	return lexer.tokens, err
}

type lexer struct {
	source string
	tokens []token.Token

	start   int // start of current lexeme
	current int // current position in source

	filePath string // current file path
	line     int    // current line number
	column   int    // current column number
}

func (l lexer) location() token.Location {
	return token.Location{FilePath: l.filePath, Line: l.line, Column: l.column}
}

func (l lexer) isAtEnd() bool {
	return l.current >= len(l.source)
}

func (l lexer) peek() rune {
	if l.isAtEnd() {
		return '\x00'
	}
	runeValue, _ := utf8.DecodeRuneInString(l.source[l.current:])

	return runeValue
}

func (l *lexer) advance() rune {
	runeValue, width := utf8.DecodeRuneInString(l.source[l.current:])
	l.current += width
	l.column++

	return runeValue
}

func (l *lexer) addToken(loc token.Location, kind token.Kind, literal any) {
	text := l.source[l.start:l.current]
	l.tokens = append(l.tokens, token.Token{Kind: kind, Lexeme: text, Location: loc, Literal: literal})
}

type UnexpectedCharacterError struct {
	Line int
	Char rune
}

func (e UnexpectedCharacterError) Error() string {
	return fmt.Sprintf("unexpected character: %c at line %d", e.Char, e.Line)
}

func (l *lexer) scanToken() error {
	l.start = l.current
	loc := l.location()
	char := l.advance()
	switch char {
	case ' ', '\r', '\t':
		// ignore whitespace
		return nil
	case '\n':
		l.line++
		l.column = 1

		return nil
	case '/':
		return l.skipComment()
	case '"':
		return l.string(loc)
	default:
		if char == ':' && isAlpha(l.peek()) {
			return l.symbol(loc)
		}

		if k, ok := getReservedSymbol(char); ok {
			l.addToken(loc, k, nil)

			return nil
		}
		if isDigit(char) {
			return l.integer(loc)
		}
		if isAlpha(char) {
			return l.identifier(loc)
		}
		if isSymbol(char) {
			return l.operator(loc)
		}
	}

	return UnexpectedCharacterError{Line: l.line, Char: char}
}

func (l *lexer) skipComment() error {
	if l.peek() == '/' {
		l.advance()

		return l.skipLineComment()
	}
	if l.peek() == '*' {
		l.advance()

		return l.skipBlockComment()
	}

	// If `/` is not followed by `/` or `*`, it is an operator.
	l.addToken(l.location(), token.OPERATOR, nil)

	return nil
}

func (l *lexer) skipLineComment() error {
	for l.peek() != '\n' && !l.isAtEnd() {
		l.advance()
	}

	return nil
}

func (l *lexer) skipBlockComment() error {
	for !l.isAtEnd() {
		char := l.advance()

		if char == '*' && l.peek() == '/' {
			l.advance()

			return nil
		}
		if char == '\n' {
			l.line++
			l.column = 1
		}
	}

	return UnterminatedBlockCommentError{Line: l.line}
}

type UnterminatedBlockCommentError struct {
	Line int
}

func (e UnterminatedBlockCommentError) Error() string {
	return fmt.Sprintf("unterminated block comment at line %d", e.Line)
}

func (l *lexer) string(loc token.Location) error {
	for l.peek() != '"' && !l.isAtEnd() {
		if l.peek() == '\n' {
			l.line++
		}
		if l.peek() == '\\' {
			l.advance()
			if l.isAtEnd() {
				return UnterminatedStringError{Line: l.line}
			}
		}
		l.advance()
	}

	if l.isAtEnd() {
		return UnterminatedStringError{Line: l.line}
	}

	r := l.advance()

	if r != '"' {
		return UnterminatedStringError{Line: l.line}
	}

	value := l.source[l.start+1 : l.current-1]
	l.addToken(loc, token.STRING, value)

	return nil
}

type UnterminatedStringError struct {
	Line int
}

func (e UnterminatedStringError) Error() string {
	return fmt.Sprintf("unterminated string at line %d", e.Line)
}

func isDigit(c rune) bool {
	return c >= '0' && c <= '9'
}

func (l *lexer) integer(loc token.Location) error {
	for isDigit(l.peek()) {
		l.advance()
	}

	value, err := strconv.Atoi(l.source[l.start:l.current])
	if err != nil {
		return fmt.Errorf("invalid integer: %w", err)
	}
	l.addToken(loc, token.INTEGER, value)

	return nil
}

func isAlpha(c rune) bool {
	return unicode.IsLetter(c) || c == '_'
}

// identifier parses a keyword, identifier, or symbol.
// If the identifier is a keyword, it is tokenized as the keyword.
// If the identifier starts with an uppercase letter, it is tokenized as a symbol.
// Otherwise, it is tokenized as an identifier.
func (l *lexer) identifier(loc token.Location) error {
	for isAlpha(l.peek()) || isDigit(l.peek()) {
		l.advance()
	}

	value := l.source[l.start:l.current]

	if k, ok := getKeyword(value); ok {
		l.addToken(loc, k, nil)
	} else if utils.IsUpper(value) {
		l.addToken(loc, token.SYMBOL, nil)
	} else {
		l.addToken(loc, token.IDENT, nil)
	}

	return nil
}

// symbol parses a identifier as symbol starting with a colon.
func (l *lexer) symbol(loc token.Location) error {
	for isAlpha(l.peek()) || isDigit(l.peek()) {
		l.advance()
	}

	l.addToken(loc, token.SYMBOL, nil)

	return nil
}

func getKeyword(str string) (token.Kind, bool) {
	keywords := map[string]token.Kind{
		"->":     token.ARROW,
		"<-":     token.BACKARROW,
		"|":      token.BAR,
		"=":      token.EQUAL,
		"case":   token.CASE,
		"data":   token.DATA,
		"def":    token.DEF,
		"fn":     token.FN,
		"infix":  token.INFIX,
		"infixl": token.INFIXL,
		"infixr": token.INFIXR,
		"let":    token.LET,
		"prim":   token.PRIM,
		"type":   token.TYPE,
		"with":   token.WITH,
	}

	if k, ok := keywords[str]; ok {
		return k, true
	}

	return token.IDENT, false
}

func isSymbol(c rune) bool {
	_, isReserved := getReservedSymbol(c)

	return c != '_' && !isReserved && (unicode.IsSymbol(c) || unicode.IsPunct(c))
}

func getReservedSymbol(char rune) (token.Kind, bool) {
	// These characters are reserved symbols, but they are not included in operator.
	reservedSymbols := map[rune]token.Kind{
		'(': token.LEFTPAREN,
		')': token.RIGHTPAREN,
		'{': token.LEFTBRACE,
		'}': token.RIGHTBRACE,
		'[': token.LEFTBRACKET,
		']': token.RIGHTBRACKET,
		':': token.COLON,
		',': token.COMMA,
		'.': token.DOT,
		';': token.SEMICOLON,
		'#': token.SHARP,
	}
	if k, ok := reservedSymbols[char]; ok {
		return k, true
	}

	return token.OPERATOR, false
}

func (l *lexer) operator(loc token.Location) error {
	for isSymbol(l.peek()) {
		l.advance()
	}

	value := l.source[l.start:l.current]
	if k, ok := getKeyword(value); ok {
		l.addToken(loc, k, nil)
	} else {
		l.addToken(loc, token.OPERATOR, nil)
	}

	return nil
}
