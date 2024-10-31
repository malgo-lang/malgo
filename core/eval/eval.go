package eval

import (
	"fmt"
	"io"
	"os"
	"strings"

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

func (e *env) get(name token.Token) (Value, error) {
	if v, ok := e.values[name.String()]; ok {
		return v, nil
	}

	if e.parent != nil {
		return e.parent.get(name)
	}

	return nil, utils.PosError{
		Where: name,
		Err:   NotFoundError{Name: name.String()},
	}
}

type NotFoundError struct {
	Name string
}

func (e NotFoundError) Error() string {
	return fmt.Sprintf("not found: %s", e.Name)
}

func (e *env) set(name string, v Value) {
	e.values[name] = v
}

type Value any

func (e *Evaluator) InvokeMain() error {
	for name, def := range e.Toplevel {
		// If name starts with "main.", it is a main function.
		if strings.HasPrefix(name, "main.") {
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
// TODO: This function may only be used to replace the variable with its value.
// If so, it should be renamed to `expandVar(n core.Node) (core.Node, error)`.
func (e *Evaluator) producer(p core.Producer) (Value, error) {
	//nolint:varnamelen
	switch p := p.(type) {
	case *core.Var:
		return e.get(p.Name)
	case *core.Literal:
		return p, nil
	case *core.Symbol:
		return p, nil
	case *core.Do:
		return p, nil
	case *core.Cocase:
		return p, nil
	default:
		panic(fmt.Sprintf("unexpected core.Producer: %#v", p))
	}
}

// consumer normalizes a consumer.
func (e *Evaluator) consumer(c core.Consumer) (Value, error) {
	//nolint:varnamelen
	switch c := c.(type) {
	case *core.Var:
		return e.get(c.Name)
	case *core.Case:
		return c, nil
	case *core.Destruct:
		return c, nil
	case *core.Then:
		return c, nil
	case *core.Toplevel:
		return c, nil
	default:
		panic(fmt.Sprintf("unexpected core.Consumer: %#v", c))
	}
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

	// <μx. s | c> => s[x := c]
	if producer, ok := producer.(*core.Do); ok {
		bind := producer.Name.String()
		e.set(bind, consumer)

		return e.statement(producer.Body)
	}

	// <p | μ~x. s> => s[x := p]
	if consumer, ok := consumer.(*core.Then); ok {
		bind := consumer.Name.String()
		e.set(bind, producer)

		return e.statement(consumer.Body)
	}

	// <_ | *> => finish the evaluation
	if _, ok := consumer.(*core.Toplevel); ok {
		fmt.Fprintf(e.Stdout, "%v\n", producer)

		return nil
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
