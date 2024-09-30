package core_test

import (
	"os"
	"strings"
	"testing"

	"github.com/malgo-lang/malgo/ast"
	"github.com/malgo-lang/malgo/codata"
	"github.com/malgo-lang/malgo/core"
	"github.com/malgo-lang/malgo/desugarwith"
	"github.com/malgo-lang/malgo/driver"
	"github.com/malgo-lang/malgo/infix"
	"github.com/malgo-lang/malgo/utils"
	"github.com/sebdah/goldie/v2"
)

func TestConv(t *testing.T) {
	t.Parallel()

	testfiles, err := utils.FindSourceFiles("../testdata")
	if err != nil {
		t.Errorf("failed to find test files: %v", err)

		return
	}

	for _, testfile := range testfiles {
		t.Logf("testing %s", testfile)

		source, err := os.ReadFile(testfile)
		if err != nil {
			t.Errorf("failed to read %s: %v", testfile, err)

			return
		}

		runner := driver.NewPassRunner()
		runner.AddPass(&desugarwith.DesugarWith{})
		runner.AddPass(&codata.Flat{})
		runner.AddPass(infix.NewInfixResolver())
		runner.AddPass(&core.MergePatterns{})

		nodes, err := runner.RunSource(testfile, string(source))
		if err != nil {
			t.Errorf("%s returned error: %v", testfile, err)

			return
		}

		converter := core.NewConverter()
		defs := make([]*core.Def, 0, len(nodes))

		for _, node := range nodes {
			if n, ok := node.(*ast.VarDecl); ok {
				converter.Predefine(n.Name)
			}
		}

		for _, node := range nodes {
			if n, ok := node.(*ast.VarDecl); ok {
				def, err := converter.ConvDef(n)
				if err != nil {
					t.Errorf("%s returned error: %v", testfile, err)

					return
				}

				defs = append(defs, def)
			} else {
				t.Logf("skipping %T", node)
			}
		}

		var builder strings.Builder

		for _, def := range defs {
			builder.WriteString(def.Pretty(0))
			builder.WriteString("\n")
		}

		g := goldie.New(t)
		g.Assert(t, testfile, []byte(builder.String()))
	}
}
