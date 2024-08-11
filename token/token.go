package token

import "fmt"

//go:generate go run golang.org/x/tools/cmd/stringer@v0.13.0 -type=Kind
type Kind int

const (
	ERROR Kind = iota
	EOF

	// Single-character tokens.
	LEFTPAREN
	RIGHTPAREN
	LEFTBRACE
	RIGHTBRACE
	LEFTBRACKET
	RIGHTBRACKET
	COLON
	COMMA
	DOT
	SEMICOLON
	SHARP

	// Literals and identifiers.
	IDENT
	OPERATOR
	INTEGER
	STRING
	SYMBOL

	// Keywords.
	ARROW
	BACKARROW
	BAR
	CASE
	DATA
	DEF
	EQUAL
	FN
	INFIX
	INFIXL
	INFIXR
	LET
	PRIM
	TYPE
	WITH
)

type Token struct {
	Kind     Kind
	Lexeme   string
	Location Location
	Literal  any
}

func (t Token) String() string {
	if (t.Kind == IDENT || t.Kind == OPERATOR) && t.Literal != nil {
		return fmt.Sprintf("%s.%#v", t.Lexeme, t.Literal)
	}

	return t.Lexeme
}

func (t Token) Base() Token {
	return t
}

type Location struct {
	FilePath string
	Line     int
	Column   int
}

func (l Location) String() string {
	// if FilePath starts with "./" or "../", remove them.
	filePath := l.FilePath
	if len(filePath) >= 2 && filePath[0] == '.' && filePath[1] == '/' {
		filePath = filePath[2:]
	}
	if len(filePath) >= 3 && filePath[0] == '.' && filePath[1] == '.' && filePath[2] == '/' {
		filePath = filePath[3:]
	}

	return fmt.Sprintf("%s:%d:%d", filePath, l.Line, l.Column)
}
