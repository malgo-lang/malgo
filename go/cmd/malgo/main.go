package main

import (
	"fmt"

	"github.com/takoeight0821/malgo/internal/parser"
)

func main() {
	parser := parser.NewParser("foo")
	expr := parser.Parse()
	fmt.Printf("%#v\n", expr)
}
