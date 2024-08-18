package vm_test

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/sebdah/goldie/v2"
	"github.com/takoeight0821/malgo/driver"
	"github.com/takoeight0821/malgo/nameresolve"
	"github.com/takoeight0821/malgo/token"
	"github.com/takoeight0821/malgo/utils"
	"github.com/takoeight0821/malgo/vm"
)

func TestExecute(t *testing.T) {
	// t.Parallel()

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

		var code vm.Code
		for _, node := range nodes {
			var err error
			code, err = vm.Compile(node, code)
			if err != nil {
				t.Errorf("%s returned error: %v", testfile, err)
			}
		}

		machine := vm.NewMachine(code)
		var builder strings.Builder
		machine.Stdout = &builder
		machine.Stdin = strings.NewReader("test input\n")

		err = machine.Run()
		if err != nil {
			t.Errorf("%s returned error: %v", testfile, err)
		}

		top := token.Token{
			Kind:   token.IDENT,
			Lexeme: "toplevel",
			//nolint:exhaustruct
			Location: token.Location{},
			Literal:  -1,
		}
		if main, ok := vm.SearchMain(machine.Env); ok {
			machine.Code = machine.Code.Push(vm.Apply{Token: top})
			machine.Code = machine.Code.Push(vm.MkTuple{Token: top, Count: 0})
			machine.Code = machine.Code.Push(vm.Push{Value: main})
			err = machine.Run()

			var exitErr vm.ExitError
			if errors.As(err, &exitErr) {
				fmt.Fprintf(&builder, "exit => %d\n", exitErr.Code)
			} else if err != nil {
				fmt.Fprintf(&builder, "error => %v\n", err)
			}

			if machine.Stack != nil {
				fmt.Fprintf(&builder, "result => %v\n", machine.Stack.Head)
			}
		}

		g := goldie.New(t)
		testname := filepath.Base(testfile)
		g.Assert(t, "execute_"+testname, []byte(builder.String()))
	}
}
