package vm

import (
	"fmt"
	"strings"

	"github.com/takoeight0821/malgo/token"
)

type Var struct {
	name string
}

func (v Var) String() string {
	return v.name
}

func (v Var) Match(bindings map[string]Value, value Value) bool {
	bindings[v.name] = value

	return true
}

//nolint:exhaustruct
var _ Pattern = Var{}

type Literal struct {
	token.Token
}

func (l Literal) String() string {
	return l.Lexeme
}

func (l Literal) Match(_ map[string]Value, value Value) bool {
	switch value := value.(type) {
	case Int:
		if l.Literal == value.value {
			return true
		}
	case String:
		if l.Literal == value.value {
			return true
		}
	}

	return false
}

//nolint:exhaustruct
var _ Pattern = Literal{}

type PSymbol struct {
	name string
}

func (p PSymbol) String() string {
	return ":" + p.name
}

func (p PSymbol) Match(_ map[string]Value, value Value) bool {
	if symbol, ok := value.(Symbol); ok && symbol.name == p.name {
		return true
	}

	return false
}

//nolint:exhaustruct
var _ Pattern = PSymbol{}

type PTuple struct {
	fields []Pattern
}

func (p PTuple) String() string {
	var builder strings.Builder
	builder.WriteString("(")

	for i, field := range p.fields {
		if i > 0 {
			builder.WriteString(", ")
		}
		builder.WriteString(field.String())
	}

	builder.WriteString(")")

	return builder.String()
}

func (p PTuple) Match(bindings map[string]Value, value Value) bool {
	if tuple, ok := value.(Tuple); ok && len(p.fields) == len(tuple.fields) {
		for i, field := range p.fields {
			if !field.Match(bindings, tuple.fields[i]) {
				return false
			}
		}

		return true
	}

	return false
}

//nolint:exhaustruct
var _ Pattern = PTuple{}

type PCall struct {
	fun Pattern
	arg Pattern
}

func (p PCall) String() string {
	return fmt.Sprintf("%v(%v)", p.fun, p.arg)
}

func (p PCall) Match(bindings map[string]Value, value Value) bool {
	return p.MatchTrace(bindings, value.Trace())
}

func (p PCall) MatchTrace(bindings map[string]Value, trace Trace) bool {
	switch trace := trace.(type) {
	case Root:
		return false
	case Call:
		if p.fun.Match(bindings, trace.fun) {
			if p.arg.Match(bindings, trace.arg) {
				return true
			}
		}

		return p.MatchTrace(bindings, trace.trace)
	case Access:
		return p.MatchTrace(bindings, trace.trace)
	}

	panic("unreachable")
}

//nolint:exhaustruct
var _ Pattern = PCall{}

type PAccess struct {
	receiver Pattern
	name     string
}

func (p PAccess) String() string {
	return fmt.Sprintf("%v.%v", p.receiver, p.name)
}

func (p PAccess) Match(bindings map[string]Value, value Value) bool {
	return p.MatchTrace(bindings, value.Trace())
}

func (p PAccess) MatchTrace(bindings map[string]Value, trace Trace) bool {
	switch trace := trace.(type) {
	case Root:
		return false
	case Call:
		return p.MatchTrace(bindings, trace.trace)
	case Access:
		if p.name == trace.name && p.receiver.Match(bindings, trace.receiver) {
			return true
		}

		return p.MatchTrace(bindings, trace.trace)
	}

	panic("unreachable")
}
