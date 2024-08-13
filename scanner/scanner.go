package scanner

import (
	"strconv"
	"unicode"
	"unicode/utf8"

	"github.com/takoeight0821/malgo/token"
	"github.com/takoeight0821/malgo/utils"
)

type Scanner struct {
	name    string // name of the source file
	input   string // source code
	start   int    // start of current lexeme
	current int    // current position in source

	line   int // current line number
	column int // current column number
	tokens chan token.Token

	state stateFn
}

func NewScanner(name, input string) *Scanner {
	scanner := &Scanner{
		name:    name,
		input:   input,
		start:   0,
		current: 0,

		line:   1,
		column: 1,
		tokens: make(chan token.Token, 1),

		state: lexCode,
	}

	return scanner
}

func (scanner *Scanner) Next() (token.Token, error) {
	for {
		select {
		// If there is a token in the channel, return it.
		case tok := <-scanner.tokens:
			// If the token is an error, return it as an error.
			if tok.Kind == token.ERROR {
				return tok, tok.Literal.(error)
			}

			return tok, nil
		// If the channel is empty, call the state function to get the next token.
		default:
			scanner.state = scanner.state(scanner)
		}
	}
}

// stateFn represents the state of the scanner as a function.
type stateFn func(*Scanner) stateFn

// emit creates a new token and sends it to the channel.
func (scanner *Scanner) emit(loc token.Location, kind token.Kind, literal any) {
	scanner.tokens <- token.Token{
		Kind:     kind,
		Lexeme:   scanner.input[scanner.start:scanner.current],
		Location: loc,
		Literal:  literal,
	}

	scanner.start = scanner.current
}

// ignore skips the current lexeme.
func (scanner *Scanner) ignore() {
	scanner.start = scanner.current
}

// location returns the current location.
func (scanner *Scanner) location() token.Location {
	return token.Location{
		FilePath: scanner.name,
		Line:     scanner.line,
		Column:   scanner.column,
	}
}

func (scanner *Scanner) isAtEnd() bool {
	return scanner.current >= len(scanner.input)
}

func (scanner *Scanner) peek() rune {
	if scanner.isAtEnd() {
		return '\x00'
	}

	runeValue, _ := utf8.DecodeRuneInString(scanner.input[scanner.current:])

	return runeValue
}

func (scanner *Scanner) advance() rune {
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
func lexCode(scanner *Scanner) stateFn {
	// When current character is ':', it needs to check if it is a symbol or not by looking at the next character.
	// So we also need to keep current location for the current character.
	loc := scanner.location()
	char := scanner.peek()

	switch char {
	case ' ', '\t', '\r', '\n':
		scanner.advance()
		scanner.ignore()

		return lexCode
	case '/':
		return lexComment
	case '"':
		return lexString
	default:
		if char == ':' {
			scanner.advance()

			if isAlpha(scanner.peek()) {
				return lexSymbol(loc)
			}
		}
		if k, ok := getReservedSymbol(char); ok {
			scanner.advance()
			scanner.emit(loc, k, nil)

			return lexCode
		}

		if isDigit(char) {
			return lexNumber
		}
		if isAlpha(char) {
			return lexIdentifier
		}
		if isSymbol(char) {
			return lexOperator
		}
	}

	if !scanner.isAtEnd() {
		scanner.emit(loc, token.ERROR, UnexpectedCharacterError{Location: loc, Char: char})

		return nil
	}

	scanner.emit(loc, token.EOF, nil)

	return nil
}

// lexComment scans the comment and ignores it.
func lexComment(scanner *Scanner) stateFn {
	// keep the location of '/'
	loc := scanner.location()
	scanner.advance()

	if scanner.peek() == '/' {
		for scanner.peek() != '\n' && !scanner.isAtEnd() {
			scanner.advance()
		}
		scanner.ignore()

		return lexCode
	}

	if scanner.peek() == '*' {
		scanner.advance()

		for !scanner.isAtEnd() {
			char := scanner.advance()

			if char == '*' && scanner.peek() == '/' {
				scanner.advance()
				scanner.ignore()

				return lexCode
			}
		}
	}

	scanner.emit(loc, token.OPERATOR, nil)

	return lexCode
}

// lexString scans the string literal.
func lexString(scanner *Scanner) stateFn {
	loc := scanner.location()
	scanner.advance()

	for scanner.peek() != '"' && !scanner.isAtEnd() {
		if scanner.peek() == '\\' {
			scanner.advance()
			if scanner.isAtEnd() {
				scanner.emit(loc, token.ERROR, UnterminatedStringError{Location: loc})

				return nil
			}
		}
		scanner.advance()
	}

	if scanner.isAtEnd() {
		scanner.emit(loc, token.ERROR, UnterminatedStringError{Location: loc})

		return nil
	}

	if scanner.advance() != '"' {
		scanner.emit(loc, token.ERROR, UnterminatedStringError{Location: loc})

		return nil
	}

	value := scanner.input[scanner.start+1 : scanner.current-1]
	scanner.emit(loc, token.STRING, value)

	return lexCode
}

// lexSymbol scans the symbol.
// The location is given as an argument because the first character of the symbol ':' is already consumed.
func lexSymbol(loc token.Location) func(scanner *Scanner) stateFn {
	return func(scanner *Scanner) stateFn {
		for isAlpha(scanner.peek()) || isDigit(scanner.peek()) {
			scanner.advance()
		}

		scanner.emit(loc, token.SYMBOL, nil)

		return lexCode
	}
}

// lexNumber scans the number literal.
func lexNumber(scanner *Scanner) stateFn {
	loc := scanner.location()
	for isDigit(scanner.peek()) {
		scanner.advance()
	}

	value, err := strconv.Atoi(scanner.input[scanner.start:scanner.current])
	if err != nil {
		scanner.emit(loc, token.ERROR, err)

		return nil
	}
	scanner.emit(loc, token.INTEGER, value)

	return lexCode
}

// lexIdentifier scans the identifier.
// If the identifier is a keyword, it emits the keyword token.
// If the identifier is an upper case, it emits the SYMBOL token.
// Otherwise, it emits the IDENT token.
func lexIdentifier(scanner *Scanner) stateFn {
	loc := scanner.location()
	for isAlpha(scanner.peek()) || isDigit(scanner.peek()) {
		scanner.advance()
	}

	value := scanner.input[scanner.start:scanner.current]

	if kind, ok := getKeyword(value); ok {
		scanner.emit(loc, kind, nil)
	} else if utils.IsUpper(value) {
		scanner.emit(loc, token.SYMBOL, nil)
	} else {
		scanner.emit(loc, token.IDENT, nil)
	}

	return lexCode
}

// lexOperator scans the operator.
// If the operator is a keyword, it emits the keyword token.
// Otherwise, it emits the OPERATOR token.
func lexOperator(scanner *Scanner) stateFn {
	loc := scanner.location()
	for isSymbol(scanner.peek()) {
		scanner.advance()
	}

	value := scanner.input[scanner.start:scanner.current]
	if kind, ok := getKeyword(value); ok {
		scanner.emit(loc, kind, nil)
	} else {
		scanner.emit(loc, token.OPERATOR, nil)
	}

	return lexCode
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
