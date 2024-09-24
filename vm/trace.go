package vm

import (
	"fmt"
	"io"
)

type Root struct{}

func (r Root) Print(w io.Writer, level int) {
	fmt.Fprintf(w, "%vroot\n", indent(level))
}

func (r Root) Wrap(_ Trace) Trace {
	panic("Root cannot wrap anything")
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

func (c Call) Wrap(old Trace) Trace {
	return Call{
		fun:   c.fun,
		arg:   c.arg,
		trace: old,
	}
}

type Access struct {
	receiver Value
	name     Name
	trace    Trace
}

func (a Access) Print(w io.Writer, level int) {
	fmt.Fprintf(w, "%vaccess %v %s\n", indent(level), a.receiver, a.name.Value())
	fmt.Fprintf(w, "%vreceiver:\n", indent(level))
	a.receiver.Trace().Print(w, level+1)
	a.trace.Print(w, level)
}

func (a Access) Wrap(old Trace) Trace {
	return Access{
		receiver: a.receiver,
		name:     a.name,
		trace:    old,
	}
}
