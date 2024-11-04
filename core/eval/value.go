package eval

import (
	"fmt"
	"io"
	"strings"

	"github.com/malgo-lang/malgo/core"
)

type Value struct {
	Repr  core.Repr
	Trace Trace
}

func (v Value) String() string {
	var trace strings.Builder
	v.Trace.Print(&trace, 0)

	return fmt.Sprintf("%v{%s}", v.Repr, trace.String())
}

type Covalue struct {
	Corepr     core.Corepr
	Annotation Annotation
}

// Trace is a type that represents a trace of evaluation.
type Trace interface {
	fmt.Stringer
	Print(w io.Writer, level int)
	isTrace()
}

type Root struct{}

//exhaustruct:ignore
var _ Trace = &Root{}

func (r *Root) String() string {
	return "root"
}

func (r *Root) Print(w io.Writer, level int) {
	fmt.Fprintf(w, "%vroot", indent(level))
}

func (r *Root) isTrace() {}

type Construct struct {
	Origin Value
	Name   string
	Args   []Value
	Conts  []Covalue
	Trace  Trace
}

//exhaustruct:ignore
var _ Trace = &Construct{}

func (c *Construct) String() string {
	var trace strings.Builder
	c.Print(&trace, 0)

	return trace.String()
}

func (c *Construct) Print(w io.Writer, level int) {
	fmt.Fprintf(w, "%vconstruct %v %s\n", indent(level), c.Origin, c.Name)
	fmt.Fprintf(w, "%vorigin:\n", indent(level))
	c.Origin.Trace.Print(w, level+1)
	fmt.Fprintf(w, "%vargs:\n", indent(level))
	for _, arg := range c.Args {
		arg.Trace.Print(w, level+1)
	}
	fmt.Fprintf(w, "%vconts:\n", indent(level))
	for _, cont := range c.Conts {
		fmt.Fprintf(w, "%s\n", cont.Corepr.Pretty(level+1))
	}
	c.Trace.Print(w, level)
}

func (c *Construct) isTrace() {}

type Annotation func(Value) Value

func NoAnnotation(v Value) Value {
	return v
}

func indent(level int) string {
	return strings.Repeat("\t", level)
}
