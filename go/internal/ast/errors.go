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
