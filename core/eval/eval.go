package eval

import (
	"io"
	"strings"

	"github.com/malgo-lang/malgo/core"
	"github.com/malgo-lang/malgo/utils"
)

type Evaluator struct {
	*core.Converter
	*env
	Toplevel map[string]*core.Def
	Stdin    io.Reader
	Stdout   io.Writer
	Stderr   io.Writer
}

func NewEvaluator(c *core.Converter) *Evaluator {
	return &Evaluator{
		Converter: c,
		env:       newEnv(nil),
		Toplevel:  make(map[string]*core.Def),
		Stdin:     nil,
		Stdout:    nil,
		Stderr:    nil,
	}
}

func (e *Evaluator) Def(def *core.Def) error {
	name := def.Name.String()

	// Check if the definition is already defined
	if _, ok := e.Toplevel[name]; ok {
		return utils.PosError{
			Where: def.Base(),
			Err: AlreadyDefinedError{
				Name: name,
			},
		}
	}

	e.Toplevel[name] = def

	return nil
}

func (e *Evaluator) InvokeMain() error {
	// search for the main function

	for name, def := range e.Toplevel {
		if strings.HasPrefix(name, "main.") {
			return e.Invoke(def, []Covalue{{Corepr: &core.Toplevel{}, Adopter: Identity}})
		}
	}

	return NoMainError{}
}

// Invoke invokes the given definition with the given continuations.
func (e *Evaluator) Invoke(def *core.Def, conts []Covalue) error {
	panic("not implemented")
}
