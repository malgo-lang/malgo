package vm

import (
	"fmt"

	"github.com/xlab/treeprint"
)

type Root struct{}

func (r Root) AddTo(tree treeprint.Tree) {
	tree.AddNode("root")
}

var _ Trace = Root{}

type Call struct {
	fun   Value
	arg   Value
	trace Trace
}

func (c Call) AddTo(tree treeprint.Tree) {
	call := tree.AddMetaBranch(fmt.Sprintf("%v %v", c.fun, c.arg), "call")
	c.fun.Trace().AddTo(call)
	c.arg.Trace().AddTo(call)
	c.trace.AddTo(tree)
}

type Access struct {
	receiver Value
	name     string
	trace    Trace
}

func (a Access) AddTo(tree treeprint.Tree) {
	access := tree.AddMetaBranch(fmt.Sprintf("%v.%v", a.receiver, a.name), "access")
	a.receiver.Trace().AddTo(access)
	a.trace.AddTo(tree)
}
