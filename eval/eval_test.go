package eval_test

import (
	"errors"
	"fmt"
	"os"
	"strings"
	"testing"

	"github.com/sebdah/goldie/v2"
	"github.com/takoeight0821/malgo/driver"
	"github.com/takoeight0821/malgo/eval"
	"github.com/takoeight0821/malgo/nameresolve"
	"github.com/takoeight0821/malgo/token"
	"github.com/takoeight0821/malgo/utils"
)

func BenchmarkTestdata(b *testing.B) {
	testfiles, err := utils.FindSourceFiles("../testdata")
	if err != nil {
		b.Errorf("failed to find test files: %v", err)

		return
	}

	for _, testfile := range testfiles {
		source, err := os.ReadFile(testfile)
		if err != nil {
			b.Errorf("failed to read %s: %v", testfile, err)

			return
		}

		runner := driver.NewPassRunner()
		driver.AddPassesUntil(runner, nameresolve.NewResolver())

		nodes, err := runner.RunSource(testfile, string(source))
		if err != nil {
			b.Errorf("%s returned error: %v", testfile, err)

			return
		}

		evaluator := eval.NewEvaluator()
		var builder strings.Builder
		evaluator.Stdout = &builder
		evaluator.Stdin = strings.NewReader("test input\n")
		values := make([]eval.Value, len(nodes))

		for i, node := range nodes {
			values[i], err = evaluator.Eval(node)
			if err != nil {
				b.Errorf("%s returned error: %v", testfile, err)

				return
			}
		}

		if main, ok := evaluator.SearchMain(); ok {
			top := token.Token{Kind: token.IDENT, Lexeme: "toplevel", Location: token.Location{}, Literal: -1}
			_, err := main.Apply(top, eval.Unit())
			if err != nil {
				b.Errorf("%s returned error: %v", testfile, err)
			}
		} else {
			b.Errorf("%s does not have a main function", testfile)
		}
	}
}

func TestGolden(t *testing.T) {
	t.Parallel()

	testfiles, err := utils.FindSourceFiles("../testdata")
	if err != nil {
		t.Errorf("failed to find test files: %v", err)

		return
	}

	for _, testfile := range testfiles {
		t.Logf("testfile: %s", testfile)
		source, err := os.ReadFile(testfile)
		if err != nil {
			t.Errorf("failed to read %s: %v", testfile, err)

			return
		}

		runner := driver.NewPassRunner()
		driver.AddPassesUntil(runner, nameresolve.NewResolver())

		nodes, err := runner.RunSource(testfile, string(source))
		if err != nil {
			t.Errorf("%s returned error: %v", testfile, err)

			return
		}

		evaluator := eval.NewEvaluator()
		var builder strings.Builder
		evaluator.Stdout = &builder
		evaluator.Stdin = strings.NewReader("test input\n")
		values := make([]eval.Value, len(nodes))

		for i, node := range nodes {
			values[i], err = evaluator.Eval(node)
			if err != nil {
				t.Errorf("%s returned error: %v", testfile, err)

				return
			}
		}

		if main, ok := evaluator.SearchMain(); ok {
			top := token.Token{Kind: token.IDENT, Lexeme: "toplevel", Location: token.Location{}, Literal: -1}
			ret, err := main.Apply(top, eval.Unit())
			var exitErr eval.ExitError
			if errors.As(err, &exitErr) {
				fmt.Fprintf(&builder, "exit => %d\n", exitErr.Code)
			} else if err != nil {
				fmt.Fprintf(&builder, "error => %v\n", err)
			}
			if ret != nil {
				fmt.Fprintf(&builder, "result => %s\n", ret.String())
			}

			g := goldie.New(t)
			g.Assert(t, testfile, []byte(builder.String()))
		} else {
			t.Errorf("%s does not have a main function", testfile)
		}
	}
}
