package vm

import (
	"fmt"
	"io"
)

type Root struct{}

func (r Root) Print(w io.Writer, level int) {
	fmt.Fprintf(w, "%vroot\n", indent(level))
}

var _ Trace = Root{}

type Call struct {
	fun   Value
	arg   Value
	trace Trace
}

func (c Call) Print(w io.Writer, level int) {
	fmt.Fprintf(w, "%vcall %v %v\n", indent(level), c.fun, c.arg)
	fmt.Fprintf(w, "%vfun:\n", indent(level))
	c.fun.Trace().Print(w, level+1)
	fmt.Fprintf(w, "%varg:\n", indent(level))
	c.arg.Trace().Print(w, level+1)
	c.trace.Print(w, level)
}

type Access struct {
	receiver Value
	name     string
	trace    Trace
}

func (a Access) Print(w io.Writer, level int) {
	fmt.Fprintf(w, "%vaccess %v %v\n", indent(level), a.receiver, a.name)
	fmt.Fprintf(w, "%vreceiver:\n", indent(level))
	a.receiver.Trace().Print(w, level+1)
	a.trace.Print(w, level)
}
