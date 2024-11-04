package eval

import (
	"fmt"
	"io"
	"log"
	"strings"

	"github.com/malgo-lang/malgo/core"
	"github.com/malgo-lang/malgo/token"
	"github.com/malgo-lang/malgo/utils"
)

type Evaluator struct {
	*core.UniqueGen
	*env
	Toplevel map[string]*core.Def
	Stdin    io.Reader
	Stdout   io.Writer
	Stderr   io.Writer
}

func NewEvaluator(g *core.UniqueGen) *Evaluator {
	return &Evaluator{
		UniqueGen: g,
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
			cont := &core.Destruct{
				Name:  "ap",
				Args:  []core.Producer{},
				Conts: []core.Consumer{&core.Toplevel{}},
			}

			return e.Invoke(def, []Covalue{{Corepr: cont, Annotation: NoAnnotation}})
		}
	}

	return NoMainError{}
}

// Invoke invokes the given definition with the given continuations.
func (e *Evaluator) Invoke(def *core.Def, conts []Covalue) error {
	e.env = newEnv(e.env)
	defer func() {
		e.env = e.env.parent
	}()

	for i, ret := range def.Returns {
		if i >= len(conts) {
			return utils.PosError{
				Where: def.Base(),
				Err:   NotEnoughArgumentsError{},
			}
		}

		e.env.SetCo(ret.String(), conts[i])
	}

	return e.statement(def.Body)
}

func (e *Evaluator) statement(s core.Statement) error {
	switch statement := s.(type) {
	case *core.Cut:
		return e.cut(statement.Producer, statement.Consumer)
	case *core.Invoke:
		def, ok := e.Toplevel[statement.Name.String()]
		if !ok {
			panic(fmt.Sprintf("undefined function: %s", statement.Name))
		}

		covalues := make([]Covalue, len(statement.Conts))
		for i, cont := range statement.Conts {
			var err error
			covalues[i], err = e.consumer(cont)
			if err != nil {
				return err
			}
		}

		return e.Invoke(def, covalues)
	case *core.Prim:
		panic("not implemented")
	default:
		panic(fmt.Sprintf("unexpected core.Statement: %#v", statement))
	}
}

func (e *Evaluator) cut(producer core.Producer, consumer core.Consumer) error {
	if p, ok := producer.(*core.Do); ok {
		return e.cutDo(p.Name, p.Body, consumer)
	}

	value, err := e.producer(producer)
	if err != nil {
		return err
	}

	covalue, err := e.consumer(consumer)
	if err != nil {
		return err
	}

	value = covalue.Annotation(value)
	log.Printf("DEBUG: %#v", value)

	switch corepr := covalue.Corepr.(type) {
	case *core.Case:
		return e.cutCase(value, corepr.Clauses)
	case *core.Destruct:
		return e.cutDestruct(corepr.Base(), value, corepr.Name, corepr.Args, corepr.Conts)
	case *core.Then:
		return e.cutThen(value, corepr.Name, corepr.Body)
	case *core.Toplevel:
		return e.cutToplevel(value)
	default:
		panic(fmt.Sprintf("unexpected core.Corepr: %#v", corepr))
	}
}

func (e *Evaluator) cutDo(name token.Token, body core.Statement, c core.Consumer) error {
	covalue, err := e.consumer(c)
	if err != nil {
		return err
	}

	e.env.SetCo(name.String(), covalue)

	return e.statement(body)
}

func (e *Evaluator) cutCase(v Value, clauses []*core.Clause) error {
	panic("not implemented")
}

func (e *Evaluator) cutDestruct(where token.Token, v Value, name string, args []core.Producer, conts []core.Consumer) error {
	if symbol, ok := v.Repr.(*core.Symbol); ok {
		e.cutDestructSymbol(where, symbol, v.Trace, name, args, conts)
	}

	cocase, ok := v.Repr.(*core.Cocase)
	if !ok {
		return utils.PosError{Where: where, Err: InvalidValueError{
			Expect: "cocase",
			Actual: v.Repr.String(),
		}}
	}

	for _, method := range cocase.Methods {
		if method.Name == name {
			values := make([]Value, len(args))
			for i, arg := range args {
				var err error
				values[i], err = e.producer(arg)
				if err != nil {
					return err
				}
			}

			covalues := make([]Covalue, len(conts))
			for i, cont := range conts {
				var err error
				covalues[i], err = e.consumer(cont)
				if err != nil {
					return err
				}
			}

			for i, covalue := range covalues {
				old := covalue.Annotation
				covalue.Annotation = func(value Value) Value {
					value = old(value)
					value.Trace = &Construct{
						Origin: old(value),
						Name:   name,
						Args:   values,
						Conts:  covalues,
					}

					return value
				}

				log.Printf("DEBUG: %#v", old)
				log.Printf("DEBUG: %#v", covalue.Annotation)

				covalues[i] = covalue
			}

			for _, covalue := range covalues {
				log.Printf("DEBUG: %#v", covalue)
			}

			e.env = newEnv(e.env)
			defer func() {
				e.env = e.env.parent
			}()

			for i, param := range method.Params {
				e.env.Set(param.String(), values[i])
			}

			for i, label := range method.Labels {
				e.env.SetCo(label.String(), covalues[i])
			}

			return e.statement(method.Body)
		}
	}

	methodNames := make([]string, len(cocase.Methods))
	for i, method := range cocase.Methods {
		methodNames[i] = method.Name
	}

	return utils.PosError{Where: cocase.Base(), Err: NoMethodError{
		Expect: name,
		Given:  methodNames,
	}}
}

// cutDestructSymbol evaluates `:x | .f(a, b; c, d)` form.
// `:x` is treated as `cocase .f(a, b; c, d) -> :x | c`.
func (e *Evaluator) cutDestructSymbol(where token.Token, symbol *core.Symbol, trace Trace, name string, args []core.Producer, conts []core.Consumer) error {
	values := make([]Value, len(args))
	for i, arg := range args {
		var err error
		values[i], err = e.producer(arg)
		if err != nil {
			return err
		}
	}

	covalues := make([]Covalue, len(conts))
	for i, cont := range conts {
		var err error
		covalues[i], err = e.consumer(cont)
		if err != nil {
			return err
		}
	}

	for i, covalue := range covalues {
		old := covalue.Annotation
		covalue.Annotation = func(value Value) Value {
			value = old(value)
			value.Trace = &Construct{
				Origin: old(value),
				Name:   name,
				Args:   values,
				Conts:  covalues,
			}

			return value
		}

		covalues[i] = covalue
	}

	e.env = newEnv(e.env)
	defer func() {
		e.env = e.env.parent
	}()

	firstCovalue := covalues[0]
	symbolValue := firstCovalue.Annotation(
		Value{
			Repr:  symbol,
			Trace: trace,
		},
	)

	switch corepr := firstCovalue.Corepr.(type) {
	case *core.Case:
		return e.cutCase(symbolValue, corepr.Clauses)
	case *core.Destruct:
		return e.cutDestruct(corepr.Base(), symbolValue, corepr.Name, corepr.Args, corepr.Conts)
	case *core.Then:
		return e.cutThen(symbolValue, corepr.Name, corepr.Body)
	case *core.Toplevel:
		return e.cutToplevel(symbolValue)
	default:
		panic(fmt.Sprintf("unexpected core.Corepr: %#v", corepr))
	}
}

func (e *Evaluator) cutThen(v Value, name token.Token, body core.Statement) error {
	panic("not implemented")
}

func (e *Evaluator) cutToplevel(v Value) error {
	log.Printf("cutToplevel:\n%v", v.String())

	return nil
}

// producer evaluates the given producer and returns the result value.
// Basically, it expands the variable and returns the value.
func (e *Evaluator) producer(p core.Producer) (Value, error) {
	switch repr := p.(type) {
	case *core.Cocase:
		return Value{
			Repr:  repr,
			Trace: &Root{},
		}, nil
	case *core.Do:
		panic(fmt.Sprintf("Do must be handled in cut: %#v", repr))
	case *core.Literal:
		return Value{
			Repr:  repr,
			Trace: &Root{},
		}, nil
	case *core.Symbol:
		return Value{
			Repr:  repr,
			Trace: &Root{},
		}, nil
	case *core.Var:
		value, ok := e.Get(repr.Name.String())
		if !ok {
			return Value{}, utils.PosError{Where: repr.Base(), Err: UndefinedError{Name: repr.Name.String()}}
		}

		return value, nil
	default:
		panic(fmt.Sprintf("unexpected core.Producer: %#v", repr))
	}
}

// consumer evaluates the given consumer and returns the result covalue.
// Basically, it expands the covariable and returns the covalue.
func (e *Evaluator) consumer(c core.Consumer) (Covalue, error) {
	switch corepr := c.(type) {
	case *core.Case:
		return Covalue{
			Corepr:     corepr,
			Annotation: NoAnnotation,
		}, nil
	case *core.Destruct:
		return Covalue{
			Corepr:     corepr,
			Annotation: NoAnnotation,
		}, nil
	case *core.Then:
		return Covalue{
			Corepr:     corepr,
			Annotation: NoAnnotation,
		}, nil
	case *core.Toplevel:
		return Covalue{
			Corepr:     corepr,
			Annotation: NoAnnotation,
		}, nil
	case *core.Var:
		covalue, ok := e.GetCo(corepr.Name.String())
		if !ok {
			return Covalue{}, utils.PosError{Where: corepr.Base(), Err: UndefinedError{Name: corepr.Name.String()}}
		}

		return covalue, nil
	default:
		panic(fmt.Sprintf("unexpected core.Consumer: %#v", corepr))
	}
}
