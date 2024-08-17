package vm

import (
	"fmt"
	"iter"

	"github.com/takoeight0821/malgo/token"
	"github.com/xlab/treeprint"
)

type Machine struct {
	Stack *Stack[Value]
	Env   map[string]Value
	Code  *Stack[Command]
	Dump  *Stack[Dump]
}

type Stack[T any] struct {
	Head T
	Tail *Stack[T]
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

type Dump struct {
	Env  map[string]Value
	Code *Stack[Command]
}

type Value interface {
	fmt.Stringer
	Trace() Trace
	WithTrace(trace Trace) Value
}

type Command interface {
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
