package eval

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/takoeight0821/malgo/ast"
	"github.com/takoeight0821/malgo/token"
	"github.com/takoeight0821/malgo/utils"
)

type Value interface {
	fmt.Stringer
	Match(pattern ast.Node) (map[Name]Value, bool)
	Trace() Trace
	WithTrace(trace Trace) Value
}

type Callable interface {
	Apply(where token.Token, args ...Value) (Value, error)
}

func Unit() Tuple {
	unit := Tuple{values: make([]Value, 0), trace: Root{}}

	return unit
}

type Tuple struct {
	values []Value
	trace  Trace
}

func (t Tuple) String() string {
	var builder strings.Builder

	fmt.Fprintf(&builder, "[")
	for i, val := range t.values {
		if i != 0 {
			fmt.Fprintf(&builder, ", ")
		}
		fmt.Fprintf(&builder, "%v", val)
	}
	fmt.Fprintf(&builder, "]")

	return builder.String()
}

func (t Tuple) Match(pattern ast.Node) (map[Name]Value, bool) {
	switch pattern := pattern.(type) {
	case *ast.Var:
		return map[Name]Value{tokenToName(pattern.Name): t}, true
	case *ast.Tuple:
		matches := make(map[Name]Value)
		for i, elem := range t.values {
			if i >= len(pattern.Exprs) {
				return nil, false
			}
			m, ok := elem.Match(pattern.Exprs[i])
			if !ok {
				return nil, false
			}
			for k, v := range m {
				matches[k] = v
			}
		}

		return matches, true
	}

	return t.trace.Match(pattern)
}

func (t Tuple) Trace() Trace {
	return t.trace
}

func (t Tuple) WithTrace(trace Trace) Value {
	return Tuple{values: t.values, trace: NewLog(trace, t)}
}

var _ Value = Tuple{}

type Int struct {
	value int
	trace Trace
}

func (i Int) String() string {
	return strconv.Itoa(i.value)
}

func (i Int) Match(pattern ast.Node) (map[Name]Value, bool) {
	switch pattern := pattern.(type) {
	case *ast.Var:
		return map[Name]Value{tokenToName(pattern.Name): i}, true
	case *ast.Literal:
		if pattern.Kind != token.INTEGER {
			return nil, false
		}
		if v, ok := pattern.Literal.(int); ok && v == i.value {
			return map[Name]Value{}, true
		}
	}

	return i.trace.Match(pattern)
}

func (i Int) Trace() Trace {
	return i.trace
}

func (i Int) WithTrace(trace Trace) Value {
	return Int{value: i.value, trace: NewLog(trace, i)}
}

var _ Value = Int{}

type String struct {
	value string
	trace Trace
}

func (s String) String() string {
	return fmt.Sprintf("%q", s.value)
}

func (s String) Match(pattern ast.Node) (map[Name]Value, bool) {
	switch pattern := pattern.(type) {
	case *ast.Var:
		return map[Name]Value{tokenToName(pattern.Name): s}, true
	case *ast.Literal:
		if pattern.Kind == token.STRING && pattern.Literal == s.value {
			return map[Name]Value{}, true
		}
	}

	return s.trace.Match(pattern)
}

func (s String) Trace() Trace {
	return s.trace
}

func (s String) WithTrace(trace Trace) Value {
	return String{value: s.value, trace: NewLog(trace, s)}
}

var _ Value = String{}

// Function represents a closure value.
type Function struct {
	Evaluator
	Params []Name
	Body   ast.Node
	trace  Trace
}

func (f Function) String() string {
	var builder strings.Builder
	builder.WriteString("<function")
	for _, param := range f.Params {
		builder.WriteString(" ")
		builder.WriteString(string(param))
	}
	builder.WriteString(">")

	return builder.String()
}

func (f Function) Match(pattern ast.Node) (map[Name]Value, bool) {
	switch pattern := pattern.(type) {
	case *ast.Var:
		return map[Name]Value{tokenToName(pattern.Name): f}, true
	default:
		return f.trace.Match(pattern)
	}
}

func (f Function) Apply(where token.Token, args ...Value) (Value, error) {
	if len(f.Params) != len(args) {
		return nil, errorAt(where, InvalidArgumentCountError{Expected: len(f.Params), Actual: len(args)})
	}

	evaluator := Evaluator{
		evEnv:  newEvEnv(f.evEnv),
		Stdin:  f.Stdin,
		Stdout: f.Stdout,
	}

	for i, param := range f.Params {
		evaluator.evEnv.set(param, args[i])
	}

	var ret Value
	ret, err := evaluator.Eval(f.Body)
	if err != nil {
		return nil, err
	}
	f.evEnv = f.evEnv.parent

	return ret.WithTrace(Call{Func: f, Args: args}), nil
}

func (f Function) Trace() Trace {
	return f.trace
}

func (f Function) WithTrace(trace Trace) Value {
	return Function{Evaluator: f.Evaluator, Params: f.Params, Body: f.Body, trace: NewLog(trace, f)}
}

var (
	_ Value    = Function{}
	_ Callable = Function{}
)

// Thunk represents a thunk value.
// It is used to delay the evaluation of object fields.
type Thunk struct {
	Evaluator
	Body  ast.Node
	trace Trace
}

func (t Thunk) String() string {
	return "<thunk>"
}

func (t Thunk) Match(pattern ast.Node) (map[Name]Value, bool) {
	switch pattern := pattern.(type) {
	case *ast.Var:
		return map[Name]Value{tokenToName(pattern.Name): t}, true
	default:
		return nil, false
	}
}

func (t Thunk) Trace() Trace {
	return t.trace
}

func (t Thunk) WithTrace(trace Trace) Value {
	return Thunk{Evaluator: t.Evaluator, Body: t.Body, trace: NewLog(trace, t)}
}

func runThunk(value Value) (Value, error) {
	switch value := value.(type) {
	case Thunk:
		ret, err := value.Eval(value.Body)
		if err != nil {
			return nil, err
		}
		if _, ok := ret.(Thunk); ok {
			panic("unreachable: thunk cannot return thunk")
		}

		return ret, nil
	default:
		return value, nil
	}
}

var _ Value = Thunk{}

// Object represents an object value.
type Object struct {
	Fields map[string]Value
	trace  Trace
}

func (o Object) String() string {
	return "<object>"
}

func (o Object) Match(pattern ast.Node) (map[Name]Value, bool) {
	switch pattern := pattern.(type) {
	case *ast.Var:
		return map[Name]Value{tokenToName(pattern.Name): o}, true
	default:
		return o.trace.Match(pattern)
	}
}

func (o Object) Trace() Trace {
	return o.trace
}

func (o Object) WithTrace(trace Trace) Value {
	return Object{Fields: o.Fields, trace: NewLog(trace, o)}
}

var _ Value = Object{}

// Data represents a algebraic data type value.
type Data struct {
	Tag   Name
	Elems []Value
	trace Trace
}

func (d Data) String() string {
	var builder strings.Builder
	builder.WriteString(string(d.Tag))
	builder.WriteString("(")
	for i, elem := range d.Elems {
		if i > 0 {
			builder.WriteString(", ")
		}
		builder.WriteString(elem.String())
	}
	builder.WriteString(")")

	return builder.String()
}

func (d Data) Match(pattern ast.Node) (map[Name]Value, bool) {
	switch pattern := pattern.(type) {
	case *ast.Var:
		return map[Name]Value{tokenToName(pattern.Name): d}, true
	case *ast.Call:
		if fn, ok := pattern.Func.(*ast.Var); ok && utils.IsUpper(fn.Name.Lexeme) {
			if tokenToName(fn.Name) != d.Tag {
				return nil, false
			}
			matches := make(map[Name]Value)
			for i, elem := range d.Elems {
				if i >= len(pattern.Args) {
					return nil, false
				}
				m, ok := elem.Match(pattern.Args[i])
				if !ok {
					return nil, false
				}
				for k, v := range m {
					matches[k] = v
				}
			}

			return matches, true
		}
	}

	return d.trace.Match(pattern)
}

func (d Data) Trace() Trace {
	return d.trace
}

func (d Data) WithTrace(trace Trace) Value {
	return Data{Tag: d.Tag, Elems: d.Elems, trace: NewLog(trace, d)}
}

var _ Value = Data{}

type Constructor struct {
	Evaluator
	Tag    Name
	Params int
	trace  Trace
}

func (c Constructor) String() string {
	return fmt.Sprintf("%s/%d", c.Tag, c.Params)
}

func (c Constructor) Match(pattern ast.Node) (map[Name]Value, bool) {
	switch pattern := pattern.(type) {
	case *ast.Var:
		// Maybe instead of doing this, we should check if it is a constructor pattern or not.
		return map[Name]Value{tokenToName(pattern.Name): c}, true
	default:
		return c.trace.Match(pattern)
	}
}

func (c Constructor) Apply(where token.Token, args ...Value) (Value, error) {
	if len(args) != c.Params {
		return nil, errorAt(where, InvalidArgumentCountError{Expected: c.Params, Actual: len(args)})
	}

	return Data{Tag: c.Tag, Elems: args, trace: Call{Func: c, Args: args}}, nil
}

func (c Constructor) Trace() Trace {
	return c.trace
}

func (c Constructor) WithTrace(trace Trace) Value {
	return Constructor{Evaluator: c.Evaluator, Tag: c.Tag, Params: c.Params, trace: NewLog(trace, c)}
}

var (
	_ Value    = Constructor{}
	_ Callable = Constructor{}
)

type Trace interface {
	fmt.Stringer
	Match(pattern ast.Node) (map[Name]Value, bool)
}

type Root struct{}

func (r Root) String() string {
	return "<root>"
}

func (r Root) Match(_ ast.Node) (map[Name]Value, bool) {
	return nil, false
}

var _ Trace = Root{}

type Call struct {
	Func Value
	Args []Value
}

func (c Call) String() string {
	fun := c.Func.Trace()
	args := make([]Trace, len(c.Args))
	for i, arg := range c.Args {
		args[i] = arg.Trace()
	}

	return utils.Parenthesize("call", fun, utils.Concat(args)).String()
}

func (c Call) Match(pattern ast.Node) (map[Name]Value, bool) {
	if pattern, ok := pattern.(*ast.Call); ok {
		matches := make(map[Name]Value)
		m, ok := c.Func.Match(pattern.Func)
		if !ok {
			return nil, false
		}
		for k, v := range m {
			matches[k] = v
		}

		if len(pattern.Args) != len(c.Args) {
			return nil, false
		}

		for i, arg := range c.Args {
			m, ok := arg.Match(pattern.Args[i])
			if !ok {
				return nil, false
			}
			for k, v := range m {
				matches[k] = v
			}
		}

		return matches, true
	}

	return nil, false
}

type Access struct {
	Receiver Value
	Name     token.Token
}

func (a Access) String() string {
	return utils.Parenthesize("access", a.Receiver.Trace(), a.Name).String()
}

func (a Access) Match(pattern ast.Node) (map[Name]Value, bool) {
	if pattern, ok := pattern.(*ast.Access); ok && pattern.Name.Lexeme == a.Name.Lexeme {
		matches := make(map[Name]Value)
		m, ok := a.Receiver.Match(pattern.Receiver)
		if !ok {
			return nil, false
		}
		for k, v := range m {
			matches[k] = v
		}

		return matches, true
	}

	return nil, false
}

type Log struct {
	new Trace
	old Trace
}

func NewLog(trace Trace, value Value) Trace {
	if _, ok := value.Trace().(Root); ok {
		return trace
	}

	return Log{new: trace, old: value.Trace()}
}

func (b Log) String() string {
	traces := []Trace{b.new}

	rest := b.old

	for {
		if log, ok := rest.(Log); ok {
			traces = append(traces, log.new)
			rest = log.old
		} else {
			traces = append(traces, rest)

			var builder strings.Builder
			builder.WriteString("[")
			fmt.Fprintf(&builder, "%d ", len(traces))
			tracesStr := utils.Concat(traces).String()
			builder.WriteString(tracesStr)
			builder.WriteString("]")

			return builder.String()
		}
	}
}

func (b Log) Match(pattern ast.Node) (map[Name]Value, bool) {
	m, ok := b.new.Match(pattern)
	if ok {
		return m, true
	}

	return b.old.Match(pattern)
}
