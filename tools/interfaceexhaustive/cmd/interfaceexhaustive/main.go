package main

import (
	"github.com/malgo-lang/malgo/tools/interfaceexhaustive"
	"golang.org/x/tools/go/analysis/unitchecker"
)

func main() { unitchecker.Main(interfaceexhaustive.Analyzer) }
