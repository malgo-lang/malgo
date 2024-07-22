package eval

import (
	"fmt"
	"log"
	"slices"
	"strconv"
	"strings"

	"github.com/takoeight0821/malgo/ast"
	"github.com/takoeight0821/malgo/token"
	"github.com/takoeight0821/malgo/utils"
	"github.com/xlab/treeprint"
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

	return t.trace.MatchTrace(pattern)
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

	return i.trace.MatchTrace(pattern)
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

	return s.trace.MatchTrace(pattern)
}

func (s String) Trace() Trace {
	return s.trace
}

func (s String) WithTrace(trace Trace) Value {
	return String{value: s.value, trace: NewLog(trace, s)}
}

var _ Value = String{}

type Symbol struct {
	Name   string
	Values []Value
	trace  Trace
}

func (s Symbol) String() string {
	var builder strings.Builder
	builder.WriteString(s.Name)
	if len(s.Values) > 0 {
		builder.WriteString("(")
		for i, v := range s.Values {
			if i > 0 {
				builder.WriteString(", ")
			}
			builder.WriteString(v.String())
		}
		builder.WriteString(")")
	}

	return builder.String()
}

func (s Symbol) Match(pattern ast.Node) (map[Name]Value, bool) {
	switch pattern := pattern.(type) {
	case *ast.Var:
		return map[Name]Value{tokenToName(pattern.Name): s}, true
	case *ast.Symbol:
		if pattern.Name.Lexeme == s.Name && len(s.Values) == 0 {
			return map[Name]Value{}, true
		}
	case *ast.Call:
		if fn, ok := pattern.Func.(*ast.Symbol); ok && fn.Name.Lexeme == s.Name {
			if len(pattern.Args) != len(s.Values) {
				return nil, false
			}
			matches := make(map[Name]Value)
			for i, arg := range s.Values {
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
	}

	return s.Trace().MatchTrace(pattern)
}

func (s Symbol) Trace() Trace {
	return s.trace
}

func (s Symbol) WithTrace(trace Trace) Value {
	return Symbol{Name: s.Name, Values: s.Values, trace: NewLog(trace, s)}
}

func (s Symbol) Apply(_ token.Token, args ...Value) (Value, error) {
	return Symbol{Name: s.Name, Values: append(s.Values, args...), trace: Call{Func: s, Args: args}}, nil
}

var (
	_ Value    = Symbol{}
	_ Callable = Symbol{}
)

// Function represents a closure value.
type Function struct {
	Evaluator
	Params []Name
	Body   ast.Node
	trace  Trace
}

func (f Function) String() string {
	return "<fn>"
}

func (f Function) Match(pattern ast.Node) (map[Name]Value, bool) {
	switch pattern := pattern.(type) {
	case *ast.Var:
		return map[Name]Value{tokenToName(pattern.Name): f}, true
	default:
		return f.trace.MatchTrace(pattern)
	}
}

func (f Function) Apply(where token.Token, args ...Value) (Value, error) {
	if len(f.Params) != len(args) {
		log.Printf("Params: %v", f.Params)
		log.Printf("Args: %v", args)
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
	var builder strings.Builder
	keys := make([]string, 0, len(o.Fields))
	for key := range o.Fields {
		keys = append(keys, key)
	}
	slices.Sort(keys)

	builder.WriteString("{")
	for i, key := range keys {
		if i != 0 {
			builder.WriteString(", ")
		}
		fmt.Fprintf(&builder, "%s: %v", key, o.Fields[key])
	}
	builder.WriteString("}")

	return builder.String()
}

func (o Object) Match(pattern ast.Node) (map[Name]Value, bool) {
	switch pattern := pattern.(type) {
	case *ast.Var:
		return map[Name]Value{tokenToName(pattern.Name): o}, true
	default:
		return o.trace.MatchTrace(pattern)
	}
}

func (o Object) Trace() Trace {
	return o.trace
}

func (o Object) WithTrace(trace Trace) Value {
	return Object{Fields: o.Fields, trace: NewLog(trace, o)}
}

var _ Value = Object{}

type Trace interface {
	fmt.Stringer
	MatchTrace(pattern ast.Node) (map[Name]Value, bool)
}

func traceAsString(value Value) fmt.Stringer {
	if _, ok := value.Trace().(Root); ok {
		var builder strings.Builder
		fmt.Fprintf(&builder, "%v", value)

		return &builder
	}

	if trace, ok := value.Trace().(Var); ok {
		var builder strings.Builder
		fmt.Fprintf(&builder, "%v@%v", trace.Name, value)

		return &builder
	}

	return value.Trace()
}

func TraceAsTree(value Value, trace Trace, tree treeprint.Tree) treeprint.Tree {
	switch trace := trace.(type) {
	case Root:
		return tree.AddNode(value.String())
	case Var:
		return tree.AddNode(fmt.Sprintf("%v@%v", trace.Name, value))
	case Call:
		fun, args := uncurryCall(trace)
		call := tree.AddMetaBranch(value, "call")
		TraceAsTree(fun, fun.Trace(), call)
		for _, arg := range args {
			TraceAsTree(arg, arg.Trace(), call)
		}

		return tree
	case Access:
		access := tree.AddMetaBranch(value, "access")
		access.AddNode(trace.Name)
		TraceAsTree(trace.Receiver, trace.Receiver.Trace(), access)

		return tree
	case Log:
		logSlice := log2slice(trace)
		logTree := tree.AddMetaBranch(value, "log")
		for _, log := range logSlice {
			TraceAsTree(value, log, logTree)
		}

		return tree
	}

	return tree
}

type Root struct{}

func (r Root) String() string {
	return "<root>"
}

func (r Root) MatchTrace(_ ast.Node) (map[Name]Value, bool) {
	return nil, false
}

var _ Trace = Root{}

type Var struct {
	Name token.Token
}

func (v Var) String() string {
	return v.Name.String()
}

func (v Var) MatchTrace(_ ast.Node) (map[Name]Value, bool) {
	return nil, false
}

var _ Trace = Var{}

type Call struct {
	Func Value
	Args []Value
}

func uncurryCall(call Call) (Value, []Value) {
	if fun, ok := call.Func.Trace().(Call); ok {
		return uncurryCall(Call{Func: fun.Func, Args: append(fun.Args, call.Args...)})
	}

	return call.Func, call.Args
}

func (c Call) String() string {
	fun := traceAsString(c.Func)
	args := make([]fmt.Stringer, len(c.Args))
	for i, arg := range c.Args {
		args[i] = traceAsString(arg)
	}

	return utils.Parenthesize("call", fun, utils.Concat(args)).String()
}

func (c Call) MatchTrace(pattern ast.Node) (map[Name]Value, bool) {
	if pattern, ok := pattern.(*ast.Call); ok && len(pattern.Args) == len(c.Args) {
		matches := make(map[Name]Value)
		m, ok := c.Func.Match(pattern.Func)
		if !ok {
			return nil, false
		}
		for k, v := range m {
			matches[k] = v
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
	return utils.Parenthesize("access", traceAsString(a.Receiver), a.Name).String()
}

func (a Access) MatchTrace(pattern ast.Node) (map[Name]Value, bool) {
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

func log2slice(log Trace) []Trace {
	if log, ok := log.(Log); ok {
		return append(log2slice(log.old), log2slice(log.new)...)
	}

	return []Trace{log}
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

func (b Log) MatchTrace(pattern ast.Node) (map[Name]Value, bool) {
	m, ok := b.new.MatchTrace(pattern)
	if ok {
		return m, true
	}

	return b.old.MatchTrace(pattern)
}
