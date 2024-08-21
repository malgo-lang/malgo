package nameresolve_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/malgo-lang/malgo/driver"
	"github.com/malgo-lang/malgo/nameresolve"
	"github.com/malgo-lang/malgo/utils"
	"github.com/sebdah/goldie/v2"
)

func TestGolden(t *testing.T) {
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
			builder.WriteString(node.String())
			builder.WriteString("\n")
		}

		g := goldie.New(t)
		g.Assert(t, filepath.Base(testfile), []byte(builder.String()))
	}
}
