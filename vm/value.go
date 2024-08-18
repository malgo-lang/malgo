package vm

import (
	"fmt"
	"maps"
	"slices"
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

//nolint:exhaustruct
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

//nolint:exhaustruct
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

//nolint:exhaustruct
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

//nolint:exhaustruct
var _ Value = Tuple{}

type Closure struct {
	Param string
	Env   Env
	Code  Code
	trace Trace
}

func (c Closure) String() string {
	return fmt.Sprintf("λ.%v", c.Param)
}

func (c Closure) Trace() Trace {
	return c.trace
}

func (c Closure) WithTrace(trace Trace) Value {
	c.trace = trace

	return c
}

//nolint:exhaustruct
var _ Value = Closure{}

type Record struct {
	Env    Env
	Fields map[string]Code
	trace  Trace
}

func (r Record) String() string {
	var builder strings.Builder

	builder.WriteString("{")
	keys := slices.Sorted(maps.Keys(r.Fields))
	for i, key := range keys {
		if i > 0 {
			builder.WriteString(", ")
		}
		builder.WriteString(key)
	}
	builder.WriteString("}")

	return builder.String()
}

func (r Record) Trace() Trace {
	return r.trace
}

func (r Record) WithTrace(trace Trace) Value {
	r.trace = trace

	return r
}
