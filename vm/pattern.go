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

func (v Var) Match(value Value) (map[string]Value, bool) {
	return map[string]Value{v.name: value}, true
}

var _ Pattern = Var{}

type Literal struct {
	token.Token
}

func (l Literal) String() string {
	return l.Lexeme
}

func (l Literal) Match(value Value) (map[string]Value, bool) {
	switch value := value.(type) {
	case Int:
		if l.Literal == value.value {
			return nil, true
		}
	case String:
		if l.Literal == value.value {
			return nil, true
		}
	}

	return nil, false
}

var _ Pattern = Literal{}

type PSymbol struct {
	name string
}

func (p PSymbol) String() string {
	return ":" + p.name
}

func (p PSymbol) Match(value Value) (map[string]Value, bool) {
	if symbol, ok := value.(Symbol); ok && symbol.name == p.name {
		return nil, true
	}

	return nil, false
}

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

func (p PTuple) Match(value Value) (map[string]Value, bool) {
	if tuple, ok := value.(Tuple); ok && len(p.fields) == len(tuple.fields) {
		bindings := make(map[string]Value)
		for i, field := range p.fields {
			if b, ok := field.Match(tuple.fields[i]); ok {
				for k, v := range b {
					bindings[k] = v
				}
			} else {
				return nil, false
			}
		}

		return bindings, true
	}

	return nil, false
}

var _ Pattern = PTuple{}

type PCall struct {
	fun Pattern
	arg Pattern
}

func (p PCall) String() string {
	return fmt.Sprintf("%v(%v)", p.fun, p.arg)
}

func (p PCall) Match(value Value) (map[string]Value, bool) {
	return p.MatchTrace(value.Trace())
}

func (p PCall) MatchTrace(trace Trace) (map[string]Value, bool) {
	switch trace := trace.(type) {
	case Root:
		return nil, false
	case Call:
		if bindings, ok := p.fun.Match(trace.fun); ok {
			if b, ok := p.arg.Match(trace.arg); ok {
				for k, v := range b {
					bindings[k] = v
				}

				return bindings, true
			}
		}

		return p.MatchTrace(trace.trace)
	case Access:
		return p.MatchTrace(trace.trace)
	}

	panic("unreachable")
}

var _ Pattern = PCall{}

type PAccess struct {
	receiver Pattern
	name     string
}

func (p PAccess) String() string {
	return fmt.Sprintf("%v.%v", p.receiver, p.name)
}

func (p PAccess) Match(value Value) (map[string]Value, bool) {
	return p.MatchTrace(value.Trace())
}

func (p PAccess) MatchTrace(trace Trace) (map[string]Value, bool) {
	switch trace := trace.(type) {
	case Root:
		return nil, false
	case Call:
		return p.MatchTrace(trace.trace)
	case Access:
		if bindings, ok := p.receiver.Match(trace.receiver); ok && p.name == trace.name {
			return bindings, true
		}

		return p.MatchTrace(trace.trace)
	}

	panic("unreachable")
}
