package vm_test

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/sebdah/goldie/v2"
	"github.com/takoeight0821/malgo/driver"
	"github.com/takoeight0821/malgo/nameresolve"
	"github.com/takoeight0821/malgo/utils"
	"github.com/takoeight0821/malgo/vm"
)

func TestCompile(t *testing.T) {
	t.Parallel()

	testfiles, err := utils.FindSourceFiles("../testdata")
	if err != nil {
		t.Errorf("failed to find test files: %v", err)

		return
	}

	for _, testfile := range testfiles {
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

		var builder strings.Builder
		for _, node := range nodes {
			code, err := vm.Compile(node, nil)
			if err != nil {
				t.Errorf("%s returned error: %v", testfile, err)
			}

			for cmd := range code.All() {
				fmt.Fprintf(&builder, "%s\n", cmd.NestedString(0))
			}
		}

		g := goldie.New(t)
		testname := filepath.Base(testfile)
		g.Assert(t, "compile_"+testname, []byte(builder.String()))
	}
}
