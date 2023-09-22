package ast

import (
	"fmt"
	"os"
	"strings"
)

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

type NotExprError struct {
	input string
	expr  Node
}

func NewNotExprError(input string, expr Node) NotExprError {
	return NotExprError{
		input: input,
		expr:  expr,
	}
}

func (e NotExprError) Input() string {
	return e.input
}

func (e NotExprError) Pos() int {
	return e.expr.Pos()
}

func (e NotExprError) Error() string {
	return Line(e) + fmt.Sprintf("%v is not an expression", e.expr)
}

type NotPatternError struct {
	input   string
	pattern Node
}

func NewNotPatternError(input string, pattern Node) NotPatternError {
	return NotPatternError{
		input:   input,
		pattern: pattern,
	}
}

func (e NotPatternError) Input() string {
	return e.input
}

func (e NotPatternError) Pos() int {
	return e.pattern.Pos()
}

func (e NotPatternError) Error() string {
	return Line(e) + fmt.Sprintf("%v is not a pattern", e.pattern)
}
