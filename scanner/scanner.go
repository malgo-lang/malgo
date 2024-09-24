package scanner

import (
	"iter"
	"strconv"
	"unicode"
	"unicode/utf8"

	"github.com/malgo-lang/malgo/token"
	"github.com/malgo-lang/malgo/utils"
)

func Scan(name, input string) iter.Seq2[token.Token, error] {
	return func(yield func(token.Token, error) bool) {
		scanner := newScanner(name, input, yield)
		isContinue := true
		for lex := lexCode; lex != nil; lex, isContinue = lex(scanner) {
			if !isContinue {
				break
			}
		}
	}
}

type scanner struct {
	name    string // name of the source file
	input   string // source code
	start   int    // start of current lexeme
	current int    // current position in source

	line   int // current line number
	column int // current column number
	yield  func(token.Token, error) bool

	state stateFn
}

func newScanner(name, input string, yield func(token.Token, error) bool) *scanner {
	scanner := &scanner{
		name:    name,
		input:   input,
		start:   0,
		current: 0,

		line:   1,
		column: 1,
		yield:  yield,

		state: lexCode,
	}

	return scanner
}

// stateFn represents the state of the scanner as a function.
type stateFn func(*scanner) (stateFn, bool)

// emit creates a new token and sends it to the channel.
// It returns false if scanner.yield returns false (break the loop).
func (scanner *scanner) emit(loc token.Location, kind token.Kind, literal any) bool {
	if kind == token.ERROR {
		if err, ok := literal.(error); ok {
			return scanner.yield(
				//exhaustruct:ignore
				token.Token{},
				err)
		}

		panic("literal must be an error")
	}

	if !scanner.yield(token.Token{
		Kind:     kind,
		Lexeme:   scanner.input[scanner.start:scanner.current],
		Location: loc,
		Literal:  literal,
	}, nil) {
		return false
	}

	scanner.start = scanner.current

	return true
}

// ignore skips the current lexeme.
func (scanner *scanner) ignore() {
	scanner.start = scanner.current
}

// location returns the current location.
func (scanner *scanner) location() token.Location {
	return token.Location{
		FilePath: scanner.name,
		Line:     scanner.line,
		Column:   scanner.column,
	}
}

func (scanner *scanner) isAtEnd() bool {
	return scanner.current >= len(scanner.input)
}

func (scanner *scanner) peek() rune {
	if scanner.isAtEnd() {
		return '\x00'
	}

	runeValue, _ := utf8.DecodeRuneInString(scanner.input[scanner.current:])

	return runeValue
}

func (scanner *scanner) advance() rune {
	runeValue, width := utf8.DecodeRuneInString(scanner.input[scanner.current:])

	scanner.current += width
	scanner.column++

	if runeValue == '\n' {
		scanner.line++
		scanner.column = 1
	}

	return runeValue
}

// lexCode scans the source code.
func lexCode(scanner *scanner) (stateFn, bool) {
	// When current character is ':', it needs to check if it is a symbol or not by looking at the next character.
	// So we also need to keep current location for the current character.
	loc := scanner.location()
	char := scanner.peek()

	switch char {
	case ' ', '\t', '\r', '\n':
		scanner.advance()
		scanner.ignore()

		return lexCode, true
	case '/':
		return lexComment, true
	case '"':
		return lexString, true
	default:
		if char == ':' {
			scanner.advance()

			if isAlpha(scanner.peek()) {
				return lexSymbol(loc), true
			}
		}
		if k, ok := getReservedSymbol(char); ok {
			scanner.advance()

			return lexCode, scanner.emit(loc, k, nil)
		}

		if isDigit(char) {
			return lexNumber, true
		}
		if isAlpha(char) {
			return lexIdentifier, true
		}
		if isSymbol(char) {
			return lexOperator, true
		}
	}

	if !scanner.isAtEnd() {
		return nil, scanner.emit(loc, token.ERROR, UnexpectedCharacterError{Location: loc, Char: char})
	}

	return nil, scanner.emit(loc, token.EOF, nil)
}

// lexComment scans the comment and ignores it.
// If it isn't a comment, it emits the operator token.
func lexComment(scanner *scanner) (stateFn, bool) {
	// keep the location of '/'
	loc := scanner.location()
	scanner.advance()

	if scanner.peek() == '/' {
		for scanner.peek() != '\n' && !scanner.isAtEnd() {
			scanner.advance()
		}
		scanner.ignore()

		return lexCode, true
	}

	if scanner.peek() == '*' {
		scanner.advance()

		for !scanner.isAtEnd() {
			char := scanner.advance()

			if char == '*' && scanner.peek() == '/' {
				scanner.advance()
				scanner.ignore()

				return lexCode, true
			}
		}
	}

	return lexCode, scanner.emit(loc, token.OPERATOR, nil)
}

// lexString scans the string literal.
func lexString(scanner *scanner) (stateFn, bool) {
	loc := scanner.location()
	scanner.advance()

	for scanner.peek() != '"' && !scanner.isAtEnd() {
		if scanner.peek() == '\\' {
			scanner.advance()
			if scanner.isAtEnd() {
				return nil, scanner.emit(loc, token.ERROR, UnterminatedStringError{Location: loc})
			}
		}
		scanner.advance()
	}

	if scanner.isAtEnd() {
		return nil, scanner.emit(loc, token.ERROR, UnterminatedStringError{Location: loc})
	}

	if scanner.advance() != '"' {
		return nil, scanner.emit(loc, token.ERROR, UnterminatedStringError{Location: loc})
	}

	value := scanner.input[scanner.start+1 : scanner.current-1]

	return lexCode, scanner.emit(loc, token.STRING, value)
}

// lexSymbol scans the symbol.
// The location is given as an argument because the first character of the symbol ':' is already consumed.
func lexSymbol(loc token.Location) func(scanner *scanner) (stateFn, bool) {
	return func(scanner *scanner) (stateFn, bool) {
		scanner.start = scanner.current
		for isAlpha(scanner.peek()) || isDigit(scanner.peek()) {
			scanner.advance()
		}

		return lexCode, scanner.emit(loc, token.SYMBOL, nil)
	}
}

// lexNumber scans the number literal.
func lexNumber(scanner *scanner) (stateFn, bool) {
	loc := scanner.location()
	for isDigit(scanner.peek()) {
		scanner.advance()
	}

	value, err := strconv.Atoi(scanner.input[scanner.start:scanner.current])
	if err != nil {
		return nil, scanner.emit(loc, token.ERROR, err)
	}

	return lexCode, scanner.emit(loc, token.INTEGER, value)
}

// lexIdentifier scans the identifier.
// If the identifier is a keyword, it emits the keyword token.
// If the identifier is an upper case, it emits the SYMBOL token.
// Otherwise, it emits the IDENT token.
func lexIdentifier(scanner *scanner) (stateFn, bool) {
	loc := scanner.location()
	for isAlpha(scanner.peek()) || isDigit(scanner.peek()) {
		scanner.advance()
	}

	value := scanner.input[scanner.start:scanner.current]

	if kind, ok := getKeyword(value); ok {
		return lexCode, scanner.emit(loc, kind, nil)
	} else if utils.IsUpper(value) {
		return lexCode, scanner.emit(loc, token.SYMBOL, nil)
	}

	return lexCode, scanner.emit(loc, token.IDENT, nil)
}

// lexOperator scans the operator.
// If the operator is a keyword, it emits the keyword token.
// Otherwise, it emits the OPERATOR token.
func lexOperator(scanner *scanner) (stateFn, bool) {
	loc := scanner.location()
	for isSymbol(scanner.peek()) {
		scanner.advance()
	}

	value := scanner.input[scanner.start:scanner.current]
	if kind, ok := getKeyword(value); ok {
		return lexCode, scanner.emit(loc, kind, nil)
	}

	return lexCode, scanner.emit(loc, token.OPERATOR, nil)
}

// getKeyword returns the keyword token kind.
func getKeyword(value string) (token.Kind, bool) {
	switch value {
	case "->":
		return token.ARROW, true
	case "<-":
		return token.BACKARROW, true
	case "|":
		return token.BAR, true
	case "=":
		return token.EQUAL, true
	case "case":
		return token.CASE, true
	case "data":
		return token.DATA, true
	case "def":
		return token.DEF, true
	case "fn":
		return token.FN, true
	case "infix":
		return token.INFIX, true
	case "infixl":
		return token.INFIXL, true
	case "infixr":
		return token.INFIXR, true
	case "let":
		return token.LET, true
	case "prim":
		return token.PRIM, true
	case "type":
		return token.TYPE, true
	case "with":
		return token.WITH, true
	default:
		return token.IDENT, false
	}
}

// getReservedSymbol returns the reserved symbol token kind.
func getReservedSymbol(char rune) (token.Kind, bool) {
	// These characters are reserved symbols.
	// They are not allowed to be used as operators.
	switch char {
	case '(':
		return token.LEFTPAREN, true
	case ')':
		return token.RIGHTPAREN, true
	case '{':
		return token.LEFTBRACE, true
	case '}':
		return token.RIGHTBRACE, true
	case '[':
		return token.LEFTBRACKET, true
	case ']':
		return token.RIGHTBRACKET, true
	case ':':
		return token.COLON, true
	case ',':
		return token.COMMA, true
	case '.':
		return token.DOT, true
	case ';':
		return token.SEMICOLON, true
	case '#':
		return token.SHARP, true
	default:
		return token.OPERATOR, false
	}
}

func isAlpha(char rune) bool {
	return unicode.IsLetter(char) || char == '_'
}

func isDigit(char rune) bool {
	return char >= '0' && char <= '9'
}

func isSymbol(char rune) bool {
	_, isReserved := getReservedSymbol(char)

	return char != '_' && !isReserved && (unicode.IsSymbol(char) || unicode.IsPunct(char))
}

type UnterminatedStringError struct {
	Location token.Location
}

func (e UnterminatedStringError) Error() string {
	return "unterminated string at " + e.Location.String()
}

type UnexpectedCharacterError struct {
	Location token.Location
	Char     rune
}

func (e UnexpectedCharacterError) Error() string {
	return "unexpected character: " + string(e.Char) + " at " + e.Location.String()
}
