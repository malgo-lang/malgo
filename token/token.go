package token

import "fmt"

//go:generate go run golang.org/x/tools/cmd/stringer@v0.13.0 -type=Kind
type Kind int

const (
	EOF Kind = iota

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

	// Keywords.
	ARROW
	BACKARROW
	BAR
	CASE
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
	return fmt.Sprintf("%s:%d:%d", l.FilePath, l.Line, l.Column)
}
