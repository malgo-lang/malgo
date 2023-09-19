package main

import (
	"github.com/takoeight0821/malgo/tools/interfaceexhaustive"
	"golang.org/x/tools/go/analysis/unitchecker"
)

func main() { unitchecker.Main(interfaceexhaustive.Analyzer) }
