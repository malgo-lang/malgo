package vm

import (
	"fmt"
	"iter"

	"github.com/takoeight0821/malgo/token"
	"github.com/xlab/treeprint"
)

type Machine struct {
	Stack *Stack[Value]
	Env   Env
	Code  Code
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

type Env = *EnvChain

type EnvChain struct {
	variables map[string]Value
	parent    *EnvChain
}

type Dump struct {
	Env  Env
	Code Code
}

type Value interface {
	fmt.Stringer
	Trace() Trace
	WithTrace(trace Trace) Value
}

type Code = *Stack[Command]

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
