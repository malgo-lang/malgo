package utils

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/google/go-cmp/cmp"
)

// Diff returns the diff between two s-expressions.
func Diff(expected, actual string) string {
	expectedSexp, err := ParseSexp(expected)
	if err != nil {
		panic(err)
	}
	actualSexp, err := ParseSexp(actual)
	if err != nil {
		panic(err)
	}

	return cmp.Diff(expectedSexp, actualSexp)
}

// Parser for s-expression.
// Based on https://rosettacode.org/wiki/S-expressions#Go

type Sexp interface {
	fmt.Stringer
}

type (
	Atom   string
	Int    int
	String string
	List   []Sexp
)

func (a Atom) String() string {
	return string(a)
}

func (i Int) String() string {
	return strconv.Itoa(int(i))
}

func (s String) String() string {
	return fmt.Sprintf("%q", string(s))
}

func (l List) String() string {
	if len(l) == 0 {
		return "()"
	}
	var builder strings.Builder
	builder.WriteString(fmt.Sprintf("(%v", l[0]))
	for _, e := range l[1:] {
		builder.WriteString(fmt.Sprintf(" %v", e))
	}
	builder.WriteString(")")

	return builder.String()
}

type parser struct {
	tokens  []string
	current int
}

type UnexpectedEOFError struct{}

func (e UnexpectedEOFError) Error() string {
	return "unexpected end of input"
}

type UnexpectedTokenError struct {
	expected string
	actual   string
}

func (e UnexpectedTokenError) Error() string {
	return fmt.Sprintf("unexpected token: expected %s, actual %s", e.expected, e.actual)
}

func (p *parser) advance() string {
	p.current++

	return p.tokens[p.current-1]
}

func ParseSexp(s string) ([]Sexp, error) {
	tokens := tokenize(s)
	p := parser{tokens: tokens, current: 0}
	sexps := make([]Sexp, 0)
	for p.current < len(p.tokens) {
		sexp, err := p.parse()
		if err != nil {
			return nil, err
		}
		sexps = append(sexps, sexp)
	}

	return sexps, nil
}

func tokenize(s string) []string {
	s = strings.ReplaceAll(s, "(", " ( ")
	s = strings.ReplaceAll(s, ")", " ) ")

	return strings.Fields(s)
}

func (p *parser) parse() (Sexp, error) {
	if p.current >= len(p.tokens) {
		return nil, UnexpectedEOFError{}
	}
	token := p.advance()
	switch token {
	case "(":
		return p.parseList()
	case ")":
		return nil, UnexpectedTokenError{expected: "atom", actual: token}
	default:
		return p.parseAtom(token)
	}
}

func (p *parser) parseList() (Sexp, error) {
	var list List
	for {
		if p.current >= len(p.tokens) {
			return nil, UnexpectedEOFError{}
		}
		token := p.tokens[p.current]
		if token == ")" {
			p.advance()

			break
		}
		exp, err := p.parse()
		if err != nil {
			return nil, err
		}
		list = append(list, exp)
	}

	return list, nil
}

func (p *parser) parseAtom(token string) (Sexp, error) {
	if i, err := parseInt(token); err == nil {
		return i, nil
	}
	if s, err := parseString(token); err == nil {
		return s, nil
	}

	return Atom(token), nil
}

func parseInt(s string) (Int, error) {
	var i int
	_, err := fmt.Sscanf(s, "%d", &i)

	return Int(i), err
}

func parseString(s string) (String, error) {
	var str string
	_, err := fmt.Sscanf(s, "%q", &str)

	return String(str), err
}
