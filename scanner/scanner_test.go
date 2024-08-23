package scanner_test

import (
	"fmt"
	"log"
	"os"
	"strings"
	"testing"

	"github.com/malgo-lang/malgo/scanner"
	"github.com/malgo-lang/malgo/token"
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
		t.Logf("testing %s", testfile)
		source, err := os.ReadFile(testfile)
		if err != nil {
			t.Errorf("failed to read %s: %v", testfile, err)

			return
		}

		tokens := scanner.Scan(testfile, string(source))

		var builder strings.Builder

		for tok, err := range tokens {
			log.Printf("DEBUG: %v", tok)
			if err != nil {
				t.Errorf("%s returned error: %v", testfile, err)

				return
			}

			fmt.Fprintf(&builder, "%v %q %v\n", tok.Kind, tok.Lexeme, tok.Location)
			if tok.Kind == token.EOF {
				break
			}
		}

		g := goldie.New(t)
		g.Assert(t, testfile, []byte(builder.String()))
	}
}
