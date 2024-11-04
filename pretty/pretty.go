package pretty

import (
	"fmt"
	"strings"
)

type PrettyOpts struct {
	afterParen bool
}

func DefaultPrettyOpts() *PrettyOpts {
	return &PrettyOpts{
		afterParen: false,
	}
}

type Option func(*PrettyOpts)

func AfterParen(o *PrettyOpts) {
	o.afterParen = true
}

func NoAfterParen(o *PrettyOpts) {
	o.afterParen = false
}

type Pretty interface {
	Pretty(level int, opts ...Option) fmt.Stringer
}

func Indent(level int) string {
	return strings.Repeat(" ", level)
}

const tabSize int = 2

const multilineThreshold int = 16

func ShouldMultiline[T Pretty](nodes []T) bool {
	for _, node := range nodes {
		str := node.Pretty(0).String()

		if strings.Contains(str, "\n") {
			return true
		}

		if len(str) > multilineThreshold {
			return true
		}
	}

	return false
}

type prettyJoin struct {
	elems []Pretty
}

func Join(elems ...Pretty) Pretty {
	return prettyJoin{elems}
}

func JoinList[T Pretty](elems []T) Pretty {
	prettyElems := make([]Pretty, 0, len(elems))
	for _, elem := range elems {
		prettyElems = append(prettyElems, elem)
	}

	return Join(prettyElems...)
}

func (j prettyJoin) Pretty(level int, opts ...Option) fmt.Stringer {
	var builder strings.Builder

	var sep string
	if ShouldMultiline(j.elems) {
		sep = "\n"
	} else {
		sep = " "
	}

	for i, elem := range j.elems {
		if i > 0 {
			fmt.Fprintf(&builder, "%s%v", sep, elem.Pretty(level, opts...))
		} else {
			fmt.Fprintf(&builder, "%v", elem.Pretty(level, opts...))
		}
	}

	return &builder
}

type prettyString string

func (s prettyString) Pretty(level int, opts ...Option) fmt.Stringer {
	opt := DefaultPrettyOpts()
	for _, o := range opts {
		o(opt)
	}

	var builder strings.Builder

	if opt.afterParen {
		fmt.Fprintf(&builder, "%s", s)
	} else {
		fmt.Fprintf(&builder, "%s%s", Indent(level), s)
	}

	return &builder
}

func String(s string) Pretty {
	return prettyString(s)
}

type prettyParens []Pretty

func Parens(elem ...Pretty) Pretty {
	return prettyParens(elem)
}

func (p prettyParens) Pretty(level int, opts ...Option) fmt.Stringer {
	elems := []Pretty(p)

	return between(level, opts, "(", ")", elems)
}

// ParensList is a helper function to create a Pretty object with a list of elements.
// This function is useful when you want to convert a slice to a Pretty object.
func ParensList[T Pretty](elems []T) Pretty {
	prettyElems := make([]Pretty, 0, len(elems))
	for _, elem := range elems {
		prettyElems = append(prettyElems, elem)
	}

	return Parens(prettyElems...)
}

type prettyBraces []Pretty

func Braces(elem ...Pretty) Pretty {
	return prettyBraces(elem)
}

func (b prettyBraces) Pretty(level int, opts ...Option) fmt.Stringer {
	elems := []Pretty(b)

	return between(level, opts, "{", "}", elems)
}

func between(level int, opts []Option, left string, right string, elems []Pretty) fmt.Stringer {
	opt := DefaultPrettyOpts()
	for _, o := range opts {
		o(opt)
	}

	var builder strings.Builder

	if opt.afterParen {
		// If afterParen is true, we should skip the indent.
		fmt.Fprintf(&builder, "%s", left)
	} else {
		fmt.Fprintf(&builder, "%s%s", Indent(level), left)
	}

	if ShouldMultiline(elems) {
		for i, elem := range elems {
			if i > 0 {
				opts = append(opts, NoAfterParen)
				fmt.Fprintf(&builder, "\n%v", elem.Pretty(level+1, opts...))
			} else {
				opts = append(opts, AfterParen)
				fmt.Fprintf(&builder, "%v", elem.Pretty(level+1, opts...))
			}
		}

		fmt.Fprintf(&builder, "%s", right)

		return &builder
	}

	// If ShouldMultiline is false, we can use a single line and there is no indent.
	// So we can use 0 as the level.
	for i, elem := range elems {
		if i > 0 {
			fmt.Fprintf(&builder, " %v", elem.Pretty(0))
		} else {
			fmt.Fprintf(&builder, "%v", elem.Pretty(0, AfterParen))
		}
	}

	fmt.Fprintf(&builder, "%s", right)

	return &builder
}
