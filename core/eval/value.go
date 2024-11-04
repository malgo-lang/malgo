package eval

import (
	"fmt"
	"strings"

	"github.com/malgo-lang/malgo/core"
)

type Value struct {
	Repr  core.Repr
	Trace Trace
}

func (v Value) String() string {
	return fmt.Sprintf("{%s}%s", v.Trace, v.Repr)
}

type Covalue struct {
	Corepr     core.Corepr
	Annotation Annotation
}

// Trace is a type that represents a trace of evaluation.
type Trace interface {
	fmt.Stringer
	isTrace()
}

type Root struct{}

//exhaustruct:ignore
var _ Trace = &Root{}

func (r *Root) String() string {
	return "#"
}

func (r *Root) isTrace() {}

type Construct struct {
	Origin Value
	Name   string
	Args   []Value
	Conts  []Covalue
}

//exhaustruct:ignore
var _ Trace = &Construct{}

func (c *Construct) String() string {
	var builder strings.Builder

	fmt.Fprintf(&builder, "%s.%s(", c.Origin.Repr.String(), c.Name)

	for i, arg := range c.Args {
		if i > 0 {
			builder.WriteString(", ")
		}

		builder.WriteString(arg.Repr.String())
	}

	builder.WriteString(";")

	for i, cont := range c.Conts {
		if i == 0 {
			builder.WriteString(" ")
		} else {
			builder.WriteString(", ")
		}

		builder.WriteString(cont.Corepr.String())
	}

	builder.WriteString(")")

	return builder.String()
}

func (c *Construct) isTrace() {}

type Annotation func(Value) Value

func NoAnnotation(v Value) Value {
	return v
}
