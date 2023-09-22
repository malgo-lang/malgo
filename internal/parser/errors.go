package parser

import (
	"fmt"

	"github.com/malgo-lang/malgo/internal/ast"
)

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
	return ast.Line(e) + fmt.Sprintf("unknown character %c", e.character)
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
	return ast.Line(e) + fmt.Sprintf("expected %v, but got %v", e.Expected, e.Actual)
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
	return ast.Line(e) + fmt.Sprintf("expected atom, but got %v", e.Actual)
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
	return ast.Line(e) + fmt.Sprintf("expected pattern, but got %s", e.Expr)
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
	return ast.Line(e) + fmt.Sprintf("invalid literal %s", e.Value)
}
