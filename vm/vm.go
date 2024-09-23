package vm

import (
	"fmt"
	"io"
	"iter"
	"os"
	"strings"
	"unique"

	"github.com/malgo-lang/malgo/token"
)

type Machine struct {
	Stack  *Stack[Value]
	Env    Env
	Code   *Stack[Command]
	Dump   *Stack[Dump]
	Stdout io.Writer
	Stdin  io.Reader
}

func NewMachine(code *Stack[Command]) *Machine {
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

type Name = unique.Handle[string]

type Env = *Stack[map[Name]Value]

func NewEnv() Env {
	return &Stack[map[Name]Value]{Head: map[Name]Value{}, Tail: nil}
}

func Lookup(env Env, name Name) (Value, bool) {
	for e := range env.All() {
		if value, ok := e[name]; ok {
			return value, true
		}
	}

	return nil, false
}

func Extend(env Env) Env {
	return env.Push(make(map[Name]Value))
}

func Bind(env Env, name Name, value Value) {
	env.Head[name] = value
}

func SearchMain(env Env) (Value, bool) {
	for e := range env.All() {
		for name, value := range e {
			if strings.HasPrefix(name.Value(), "main.") {
				return value, true
			}
		}
	}

	return nil, false
}

type Dump struct {
	Env   Env
	Code  *Stack[Command]
	Trace Trace
}

type Value interface {
	fmt.Stringer
	Trace() Trace
	WithTrace(trace Trace) Value
}

type Command interface {
	Execute(m *Machine) error
	NestedString(level int) string
	Where() token.Token
}

type Pattern interface {
	fmt.Stringer
	Match(bindings map[Name]Value, value Value) bool
}

type Trace interface {
	Print(w io.Writer, level int)
	Wrap(old Trace) Trace
}

func Render(trace Trace) string {
	var builder strings.Builder
	trace.Print(&builder, 0)

	return builder.String()
}
