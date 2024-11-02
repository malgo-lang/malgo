package eval

import (
	"fmt"

	"github.com/malgo-lang/malgo/core"
)

type Value struct {
	Repr  core.Repr
	Trace Trace
}

type Covalue struct {
	Corepr  core.Corepr
	Adopter Adopter
}

// Trace is a type that represents a trace of evaluation.
type Trace interface {
	core.Pretty
}

type Root struct{}

//exhaustruct:ignore
var _ Trace = &Root{}

func (r *Root) String() string {
	return r.Pretty(0)
}

func (r *Root) Pretty(level int, opts ...core.Option) string {
	o := core.DefaultPrettyOpts()

	for _, opt := range opts {
		opt(o)
	}

	return fmt.Sprintf("%vroot", core.Indent(level))
}

// Adopter is a type that represents a trace modifier.
type Adopter func(Trace) Trace

func Identity(t Trace) Trace {
	return t
}