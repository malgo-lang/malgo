package vm

import (
	"fmt"
	"strconv"
	"strings"
)

type Int struct {
	value int
	trace Trace
}

func (i Int) String() string {
	return strconv.Itoa(i.value)
}

func (i Int) Trace() Trace {
	return i.trace
}

func (i Int) WithTrace(trace Trace) Value {
	i.trace = trace

	return i
}

var _ Value = Int{}

type String struct {
	value string
	trace Trace
}

func (s String) String() string {
	return fmt.Sprintf("%q", s.value)
}

func (s String) Trace() Trace {
	return s.trace
}

func (s String) WithTrace(trace Trace) Value {
	s.trace = trace

	return s
}

var _ Value = String{}

type Symbol struct {
	name  string
	trace Trace
}

func (s Symbol) String() string {
	return ":" + s.name
}

func (s Symbol) Trace() Trace {
	return s.trace
}

func (s Symbol) WithTrace(trace Trace) Value {
	s.trace = trace

	return s
}

var _ Value = Symbol{}

type Tuple struct {
	fields []Value
	trace  Trace
}

func (t Tuple) String() string {
	var builder strings.Builder

	builder.WriteString("(")

	for i, field := range t.fields {
		if i > 0 {
			builder.WriteString(", ")
		}
		builder.WriteString(field.String())
	}

	builder.WriteString(")")

	return builder.String()
}

func (t Tuple) Trace() Trace {
	return t.trace
}

func (t Tuple) WithTrace(trace Trace) Value {
	t.trace = trace

	return t
}

var _ Value = Tuple{}
