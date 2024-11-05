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

//nolint:gochecknoglobals
var primitives = make(map[string]func(*Evaluator, token.Token, []Value, Covalue) error)

//nolint:gochecknoinits
func init() {
	primitives["exit"] = primExit
	primitives["print_cps"] = primPrintCPS
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
		return e.prim(statement)
	default:
		panic(fmt.Sprintf("unexpected core.Statement: %#v", statement))
	}
}

func (e *Evaluator) prim(statement *core.Prim) error {
	values, err := e.producers(statement.Args)
	if err != nil {
		return err
	}

	covalue, err := e.consumer(statement.Cont)
	if err != nil {
		return err
	}

	log.Printf("prim: %s", statement.Name)
	for _, value := range values {
		log.Printf("\t%s", value.Repr)
	}
	log.Printf("\t%s", covalue.Corepr)

	return primitives[statement.Name.Lexeme](e, statement.Base(), values, covalue)
}

// cut evaluates `producer | consumer` form.
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

	return e.cutValue(producer.Base(), value, covalue)
}

// cutValue passes the value to the covalue.
func (e *Evaluator) cutValue(where token.Token, value Value, covalue Covalue) error {
	// Update the trace of the value using the covalue's annotation.
	value = covalue.Annotation(value)

	switch corepr := covalue.Corepr.(type) {
	case *core.Case:
		return e.cutCase(value, corepr)
	case *core.Destruct:
		return e.cutDestruct(where, value, corepr)
	case *core.Then:
		return e.cutThen(value, corepr)
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

func (e *Evaluator) cutCase(value Value, corepr *core.Case) error {
	for _, branch := range corepr.Clauses {
		values, covalues, ok := e.match(value, branch.Pattern)
		if ok {
			e.env = newEnv(e.env)
			defer func() {
				e.env = e.env.parent
			}()

			for name, value := range values {
				e.env.Set(name, value)
			}

			for name, covalue := range covalues {
				e.env.SetCo(name, covalue)
			}

			return e.statement(branch.Body)
		}
	}

	return utils.PosError{
		Where: corepr.Base(),
		Err: NoMatchError{
			Value: value,
		},
	}
}

// match matches the given value with the given pattern.
func (e *Evaluator) match(value Value, pattern core.Pattern) (map[string]Value, map[string]Covalue, bool) {
	switch pattern := pattern.(type) {
	case *core.Extract:
		return e.matchExtract(value.Trace, pattern)
	case *core.Literal:
		return e.matchLiteral(value, pattern)
	case *core.Symbol:
		return e.matchSymbol(value, pattern)
	case *core.Var:
		return e.matchVar(value, pattern)
	}

	panic(fmt.Sprintf("unexpected core.Pattern: %#v", pattern))
}

// matchCo matches the given covalue with the given pattern.
func (e *Evaluator) matchCo(c Covalue, pattern core.Pattern) (map[string]Covalue, bool) {
	if variable, ok := pattern.(*core.Var); ok {
		return map[string]Covalue{variable.Name.String(): c}, true
	}

	panic(fmt.Sprintf("unexpected core.Pattern: %#v", pattern))
}

func (e *Evaluator) matchExtract(trace Trace, extract *core.Extract) (map[string]Value, map[string]Covalue, bool) {
	switch trace := trace.(type) {
	case *Construct:
		if trace.Name != extract.Name {
			return e.matchExtract(trace.Trace, extract)
		}

		values := make(map[string]Value)
		covalues := make(map[string]Covalue)

		originValues, originCovalues, ok := e.match(trace.Origin, extract.Target)
		if !ok {
			return e.matchExtract(trace.Trace, extract)
		}

		for name, value := range originValues {
			values[name] = value
		}

		for name, covalue := range originCovalues {
			covalues[name] = covalue
		}

		for i, arg := range extract.Args {
			argValues, argCovalues, ok := e.match(trace.Args[i], arg)
			if !ok {
				return e.matchExtract(trace.Trace, extract)
			}

			for name, value := range argValues {
				values[name] = value
			}

			for name, covalue := range argCovalues {
				covalues[name] = covalue
			}
		}

		for i, cont := range extract.Conts {
			contCovalues, ok := e.matchCo(trace.Conts[i], cont)
			if !ok {
				return e.matchExtract(trace.Trace, extract)
			}

			for name, covalue := range contCovalues {
				covalues[name] = covalue
			}
		}

		return values, covalues, true
	case *Root:
		return nil, nil, false
	default:
		panic(fmt.Sprintf("unexpected eval.Trace: %#v", trace))
	}
}

func (e *Evaluator) matchLiteral(v Value, literal *core.Literal) (map[string]Value, map[string]Covalue, bool) {
	if vLiteral, ok := v.Repr.(*core.Literal); ok {
		if vLiteral.Literal == literal.Literal {
			return make(map[string]Value), make(map[string]Covalue), true
		}
	}

	return nil, nil, false
}

func (e *Evaluator) matchSymbol(v Value, symbol *core.Symbol) (map[string]Value, map[string]Covalue, bool) {
	if vSymbol, ok := v.Repr.(*core.Symbol); ok {
		if vSymbol.Name.Lexeme == symbol.Name.Lexeme {
			return make(map[string]Value), make(map[string]Covalue), true
		}
	}

	return nil, nil, false
}

func (e *Evaluator) matchVar(v Value, variable *core.Var) (map[string]Value, map[string]Covalue, bool) {
	return map[string]Value{variable.Name.String(): v}, make(map[string]Covalue), true
}

func (e *Evaluator) cutDestruct(where token.Token, value Value, destruct *core.Destruct) error {
	args := destruct.Args
	conts := destruct.Conts

	if symbol, ok := value.Repr.(*core.Symbol); ok {
		return e.cutDestructSymbol(where, symbol, value.Trace, destruct)
	}

	cocase, ok := value.Repr.(*core.Cocase)
	if !ok {
		return utils.PosError{Where: where, Err: InvalidValueError{
			Expect: "cocase",
			Actual: value.Repr.String(),
		}}
	}

	for _, method := range cocase.Methods {
		if method.Name == destruct.Name {
			values, err := e.producers(args)
			if err != nil {
				return err
			}

			covalues, err := e.consumers(conts)
			if err != nil {
				return err
			}

			e.annotateCovalues(value, covalues, destruct.Name, values)

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
		Expect: destruct.Name,
		Given:  methodNames,
	}}
}

func (*Evaluator) annotateCovalues(origin Value, covalues []Covalue, name string, values []Value) {
	for i, covalue := range covalues {
		old := covalue.Annotation
		covalue.Annotation = func(value Value) Value {
			value = old(value)
			value.Trace = &Construct{
				Origin: origin,
				Name:   name,
				Args:   values,
				Conts:  covalues,
				Trace:  value.Trace,
			}

			return value
		}

		covalues[i] = covalue
	}
}

// cutDestructSymbol evaluates `:x | .f(a, b; c, d)` form.
// `:x` is treated as `cocase .f(a, b; c, d) -> :x | c`.
func (e *Evaluator) cutDestructSymbol(
	where token.Token, symbol *core.Symbol, trace Trace, destruct *core.Destruct,
) error {
	name := destruct.Name
	args := destruct.Args
	conts := destruct.Conts

	values, err := e.producers(args)
	if err != nil {
		return err
	}

	covalues, err := e.consumers(conts)
	if err != nil {
		return err
	}

	e.annotateCovalues(Value{
		Repr:  symbol,
		Trace: trace,
	}, covalues, name, values)

	e.env = newEnv(e.env)
	defer func() {
		e.env = e.env.parent
	}()

	return e.cutValue(where, Value{
		Repr:  symbol,
		Trace: trace,
	}, covalues[0])
}

func (e *Evaluator) cutThen(v Value, then *core.Then) error {
	e.Set(then.Name.String(), v)

	return e.statement(then.Body)
}

func (e *Evaluator) cutToplevel(_ Value) error {
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

func (e *Evaluator) producers(args []core.Producer) ([]Value, error) {
	values := make([]Value, len(args))
	for i, arg := range args {
		var err error
		values[i], err = e.producer(arg)
		if err != nil {
			return nil, err
		}
	}

	return values, nil
}

func (e *Evaluator) consumers(conts []core.Consumer) ([]Covalue, error) {
	covalues := make([]Covalue, len(conts))
	for i, cont := range conts {
		var err error
		covalues[i], err = e.consumer(cont)
		if err != nil {
			return nil, err
		}
	}

	return covalues, nil
}

func primExit(e *Evaluator, where token.Token, _ []Value, _ Covalue) error {
	return ExitError{Code: 0}
}

func primPrintCPS(e *Evaluator, where token.Token, args []Value, ret Covalue) error {
	arg := args[0]
	cont := args[1]

	fmt.Fprintf(e.Stdout, "%v\n", arg)

	contVar := e.FreshName("cont", where.Location)
	retVar := e.FreshName("ret", where.Location)

	e.env = newEnv(e.env)
	defer func() {
		e.env = e.env.parent
	}()

	e.env.Set(contVar.String(), cont)
	e.env.SetCo(retVar.String(), ret)

	return e.statement(
		&core.Cut{
			Producer: &core.Var{Name: contVar},
			Consumer: &core.Destruct{
				Name: "ap",
				Args: []core.Producer{},
				Conts: []core.Consumer{
					&core.Var{Name: retVar},
				},
			},
		},
	)
}
