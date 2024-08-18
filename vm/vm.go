package vm

import (
	"fmt"
	"io"
	"iter"
	"os"
	"strings"

	"github.com/takoeight0821/malgo/token"
	"github.com/xlab/treeprint"
)

type Machine struct {
	Stack  *Stack[Value]
	Env    Env
	Code   Code
	Dump   *Stack[Dump]
	Stdout io.Writer
	Stdin  io.Reader
}

func NewMachine(code Code) *Machine {
	return &Machine{
		Stack:  nil,
		Env:    NewEnv(),
		Code:   code,
		Dump:   nil,
		Stdout: os.Stdout,
		Stdin:  os.Stdin,
	}
}

func (m *Machine) Run() error {
	for !m.Code.IsEmpty() {
		command := m.Code.Head
		m.Code = m.Code.Tail
		err := command.Execute(m)
		if err != nil {
			return fmt.Errorf("runtime error: %w", err)
		}
	}

	return nil
}

type Stack[T any] struct {
	Head T
	Tail *Stack[T]
}

func (s *Stack[T]) String() string {
	var builder strings.Builder

	builder.WriteString("[")
	isFirst := true
	for value := range s.All() {
		if !isFirst {
			builder.WriteString(", ")
		}
		builder.WriteString(fmt.Sprintf("%v", value))
		isFirst = false
	}
	builder.WriteString("]")

	return builder.String()
}

func (s *Stack[T]) IsEmpty() bool {
	return s == nil
}

// Push pushes a value onto the stack.
// The return value is a new stack with the pushed value. The original stack is not modified.
func (s *Stack[T]) Push(head T) *Stack[T] {
	return &Stack[T]{Head: head, Tail: s}
}

func (s *Stack[T]) All() iter.Seq[T] {
	return func(yield func(T) bool) {
		for stack := s; stack != nil; stack = stack.Tail {
			if !yield(stack.Head) {
				return
			}
		}
	}
}

type Env = *Stack[map[string]Value]

func NewEnv() Env {
	return &Stack[map[string]Value]{Head: map[string]Value{}, Tail: nil}
}

func Lookup(env Env, name string) (Value, bool) {
	for e := range env.All() {
		if value, ok := e[name]; ok {
			return value, true
		}
	}

	return nil, false
}

func Extend(env Env) Env {
	return env.Push(map[string]Value{})
}

func Bind(env Env, name string, value Value) {
	env.Head[name] = value
}

func SearchMain(env Env) (Value, bool) {
	for e := range env.All() {
		for name, value := range e {
			if strings.HasPrefix(name, "main.") {
				return value, true
			}
		}
	}

	return nil, false
}

type Dump struct {
	Env   Env
	Code  Code
	Trace Trace
}

type Value interface {
	fmt.Stringer
	Trace() Trace
	WithTrace(trace Trace) Value
}

type Code = *Stack[Command]

type Command interface {
	Execute(m *Machine) error
	NestedString(level int) string
	Where() token.Token
}

type Pattern interface {
	fmt.Stringer
	Match(value Value) (map[string]Value, bool)
}

type Trace interface {
	AddTo(tree treeprint.Tree)
}

func Render(trace Trace) string {
	tree := treeprint.New()
	trace.AddTo(tree)

	return tree.String()
}
