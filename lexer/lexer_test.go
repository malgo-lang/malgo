package lexer_test

import (
	"fmt"
	"os"
	"strings"
	"testing"

	"github.com/sebdah/goldie/v2"
	"github.com/takoeight0821/malgo/lexer"
	"github.com/takoeight0821/malgo/token"
	"github.com/takoeight0821/malgo/utils"
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

		lex := lexer.NewLexer(testfile, string(source))
		var builder strings.Builder

		for {
			tok, err := lex.Next()
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
