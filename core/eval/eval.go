package eval

import (
	"fmt"
	"io"
	"os"

	"github.com/malgo-lang/malgo/core"
	"github.com/malgo-lang/malgo/token"
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
		env:       &env{parent: nil, values: make(map[string]Value)},
		Toplevel:  make(map[string]*core.Def),
		Stdin:     os.Stdin,
		Stdout:    os.Stdout,
		Stderr:    os.Stderr,
	}
}

type env struct {
	parent *env
	values map[string]Value
}

func newEnv(parent *env) *env {
	return &env{parent: parent, values: make(map[string]Value)}
}

func (e *env) get(name string) Value {
	if v, ok := e.values[name]; ok {
		return v
	}

	if e.parent != nil {
		return e.parent.get(name)
	}

	panic(fmt.Sprintf("unreachable: %s", name))
}

func (e *env) set(name string, v Value) {
	e.values[name] = v
}

type Value fmt.Stringer

func (e *Evaluator) InvokeMain() error {
	for name, def := range e.Toplevel {
		// If name starts with "main.", it is a main function.
		if name[:5] == "main." {
			return e.invoke(def, []core.Consumer{&core.Toplevel{}})
		}
	}

	return utils.PosError{
		Where: token.Dummy(),
		Err:   NoMainError{},
	}
}

type NoMainError struct{}

func (e NoMainError) Error() string {
	return "no main function"
}

func (e *Evaluator) invoke(def *core.Def, consumers []core.Consumer) error {
	e.env = newEnv(e.env)

	for i, ret := range def.Returns {
		e.set(ret.String(), consumers[i])
	}

	return e.statement(def.Body)
}

func (e *Evaluator) statement(stmt core.Statement) error {
	switch stmt := stmt.(type) {
	case *core.Cut:
		return e.cut(stmt)
	case *core.Invoke:
		panic("not implemented")
	case *core.Prim:
		panic("not implemented")
	default:
		panic(fmt.Sprintf("unexpected core.Statement: %#v", stmt))
	}
}

// producer normalizes a producer.
func (e *Evaluator) producer(p core.Producer) (Value, error) {
	panic("not implemented")
}

// consumer normalizes a consumer.
func (e *Evaluator) consumer(c core.Consumer) (Value, error) {
	panic("not implemented")
}

// cut evaluates a cut statement.
func (e *Evaluator) cut(stmt *core.Cut) error {
	producer, err := e.producer(stmt.Producer)
	if err != nil {
		return err
	}

	consumer, err := e.consumer(stmt.Consumer)
	if err != nil {
		return err
	}

	if producer, ok := producer.(*core.Do); ok {
		bind := producer.Name.String()
		e.set(bind, consumer)

		return e.statement(producer.Body)
	} else if consumer, ok := consumer.(*core.Then); ok {
		bind := consumer.Name.String()
		e.set(bind, producer)

		return e.statement(consumer.Body)
	}

	panic(fmt.Sprintf("not implemented: %T, %T", producer, consumer))
}

func (e *Evaluator) Def(def *core.Def) error {
	name := def.Name.String()

	if _, ok := e.Toplevel[name]; ok {
		return utils.PosError{
			Where: def.Name,
			Err:   AlreadyDefinedError{Name: def.Name},
		}
	}

	e.Toplevel[name] = def

	return nil
}

type AlreadyDefinedError struct {
	Name token.Token
}

func (e AlreadyDefinedError) Error() string {
	return fmt.Sprintf("redefinition of %s", e.Name)
}
