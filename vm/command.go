package vm

import (
	"fmt"
	"maps"
	"slices"
	"strings"
	"unique"

	"github.com/malgo-lang/malgo/token"
	"github.com/malgo-lang/malgo/utils"
)

// Push pushes a value onto the stack.
type Push struct {
	token token.Token
	Value Value
}

func (cmd Push) Execute(m *Machine) error {
	m.Stack = m.Stack.Push(cmd.Value)

	return nil
}

func (cmd Push) Where() token.Token {
	return cmd.token
}

func (cmd Push) NestedString(level int) string {
	return fmt.Sprintf("%v%v", indent(level), cmd.Value)
}

//exhaustruct:ignore
var _ Command = Push{}

// Get gets a value from the environment and pushes it onto the stack.
type Get struct {
	token token.Token
	Name  Name
}

func (cmd Get) Execute(machine *Machine) error {
	value, ok := Lookup(machine.Env, cmd.Name)
	if !ok {
		return utils.PosError{Where: cmd.token, Err: UndefinedVariableError{Name: cmd.Name.Value()}}
	}

	machine.Stack = machine.Stack.Push(value)

	return nil
}

type UndefinedVariableError struct {
	Name string
}

func (err UndefinedVariableError) Error() string {
	return fmt.Sprintf("undefined variable %v", err.Name)
}

func (cmd Get) Where() token.Token {
	return cmd.token
}

func (cmd Get) NestedString(level int) string {
	return fmt.Sprintf("%vget %v", indent(level), cmd.Name.Value())
}

//exhaustruct:ignore
var _ Command = Get{}

type MkTuple struct {
	Token token.Token
	Count int
}

func (cmd MkTuple) Execute(machine *Machine) error {
	fields := make([]Value, cmd.Count)
	for i := cmd.Count - 1; i >= 0; i-- {
		value, stack := machine.Stack.Head, machine.Stack.Tail
		fields[i], machine.Stack = value, stack
	}

	machine.Stack = machine.Stack.Push(NewTuple(fields))

	return nil
}

func (cmd MkTuple) Where() token.Token {
	return cmd.Token
}

func (cmd MkTuple) NestedString(level int) string {
	return fmt.Sprintf("%vtuple %v", indent(level), cmd.Count)
}

// Select selects a branch based on the top value of the stack.
type Select struct {
	token    token.Token
	Count    int
	Branches []Branch
}

func (cmd Select) Execute(machine *Machine) error {
	values := make([]Value, cmd.Count)
	for i := cmd.Count - 1; i >= 0; i-- {
		value, stack := machine.Stack.Head, machine.Stack.Tail
		values[i], machine.Stack = value, stack
	}

	for _, branch := range cmd.Branches {
		bindings := make(map[Name]Value)

		isMatch := true
		for i, pat := range branch.Pattern {
			if !pat.Match(bindings, values[i]) {
				isMatch = false

				break
			}
		}

		if isMatch {
			machine.Dump = machine.Dump.Push(Dump{Env: machine.Env, Code: machine.Code, Trace: nil})
			machine.Env = &Stack[map[Name]Value]{Head: bindings, Tail: machine.Env}
			machine.Code = branch.Code

			return nil
		}
	}

	return utils.PosError{Where: cmd.token, Err: NoMatchError{}}
}

type NoMatchError struct{}

func (err NoMatchError) Error() string {
	return "no matching branch"
}

func (cmd Select) Where() token.Token {
	return cmd.token
}

func (cmd Select) NestedString(level int) string {
	var builder strings.Builder
	fmt.Fprintf(&builder, "%vselect %d {\n", indent(level), cmd.Count)

	for _, branch := range cmd.Branches {
		builder.WriteString(branch.NestedString(level))
		builder.WriteString("\n")
	}

	fmt.Fprintf(&builder, "%v}", indent(level))

	return builder.String()
}

//exhaustruct:ignore
var _ Command = Select{}

type Branch struct {
	token   token.Token
	Pattern []Pattern
	Code    Code
}

func (branch Branch) Where() token.Token {
	return branch.token
}

func (branch Branch) NestedString(level int) string {
	var builder strings.Builder

	fmt.Fprintf(&builder, "%vcase", indent(level+1))

	for _, pat := range branch.Pattern {
		fmt.Fprintf(&builder, " %v", pat)
	}

	fmt.Fprintf(&builder, " {")

	for cmd := range branch.Code.All() {
		fmt.Fprintf(&builder, "\n%s", cmd.NestedString(level+2))
	}

	fmt.Fprintf(&builder, "\n%v}", indent(level+1))

	return builder.String()
}

// Join finishes a branch and restores the code from the dump.
type Join struct {
	token token.Token
}

func (cmd Join) Execute(m *Machine) error {
	dump, stack := m.Dump.Head, m.Dump.Tail
	m.Dump = stack

	m.Env = dump.Env
	m.Code = dump.Code

	return nil
}

func (cmd Join) Where() token.Token {
	return cmd.token
}

func (cmd Join) NestedString(level int) string {
	return fmt.Sprintf("%vjoin", indent(level))
}

//exhaustruct:ignore
var _ Command = Join{}

// Lambda creates a new (recursive) closure and pushes it onto the stack.
type Lambda struct {
	token token.Token
	Param Name
	Code  Code
}

func (cmd Lambda) Execute(m *Machine) error {
	m.Stack = m.Stack.Push(NewClosure(cmd.Param, m.Env, cmd.Code))

	return nil
}

func (cmd Lambda) Where() token.Token {
	return cmd.token
}

func (cmd Lambda) NestedString(level int) string {
	var builder strings.Builder

	fmt.Fprintf(&builder, "%vlam %v {", indent(level), cmd.Param.Value())

	for c := range cmd.Code.All() {
		fmt.Fprintf(&builder, "\n%s", c.NestedString(level+1))
	}

	fmt.Fprintf(&builder, "\n%v}", indent(level))

	return builder.String()
}

//exhaustruct:ignore
var _ Command = Lambda{}

type Return struct {
	token token.Token
}

func (cmd Return) Execute(machine *Machine) error {
	dump, stack := machine.Dump.Head, machine.Dump.Tail
	machine.Dump = stack

	machine.Env = dump.Env
	machine.Code = dump.Code

	machine.Stack.Head = machine.Stack.Head.WithTrace(dump.Trace.Wrap(machine.Stack.Head.Trace()))

	return nil
}

func (cmd Return) Where() token.Token {
	return cmd.token
}

func (cmd Return) NestedString(level int) string {
	return fmt.Sprintf("%vret", indent(level))
}

//exhaustruct:ignore
var _ Command = Return{}

type Object struct {
	token  token.Token
	Fields map[Name]Code
}

func (cmd Object) Execute(m *Machine) error {
	m.Stack = m.Stack.Push(NewRecord(m.Env, cmd.Fields))

	return nil
}

func (cmd Object) Where() token.Token {
	return cmd.token
}

func (cmd Object) NestedString(level int) string {
	var builder strings.Builder

	fmt.Fprintf(&builder, "%vobject {", indent(level))

	keys := slices.Collect(maps.Keys(cmd.Fields))
	strs := make([]string, len(keys))
	for i, key := range keys {
		strs[i] = key.Value()
	}
	slices.Sort(strs)

	for _, field := range strs {
		code := cmd.Fields[unique.Make(field)]
		fmt.Fprintf(&builder, "\n%v%v {", indent(level+1), field)

		for c := range code.All() {
			fmt.Fprintf(&builder, "\n%s", c.NestedString(level+2))
		}

		fmt.Fprintf(&builder, "\n%v}", indent(level+1))
	}

	fmt.Fprintf(&builder, "\n%v}", indent(level))

	return builder.String()
}

//exhaustruct:ignore
var _ Command = Object{}

// Apply applies a closure to the top value of the stack.
type Apply struct {
	Token token.Token
}

func (cmd Apply) Execute(machine *Machine) error {
	var fun, arg Value

	arg, machine.Stack = machine.Stack.Head, machine.Stack.Tail
	fun, machine.Stack = machine.Stack.Head, machine.Stack.Tail

	closure, ok := fun.(Closure)
	if !ok {
		if symbol, ok := fun.(Symbol); ok {
			symbol.trace = Call{fun: fun, arg: arg, trace: symbol.trace}
			machine.Stack = machine.Stack.Push(symbol)

			return nil
		}

		return utils.PosError{Where: cmd.Token, Err: NotFunctionError{Value: fun}}
	}

	machine.Dump = machine.Dump.Push(Dump{
		Env:   machine.Env,
		Code:  machine.Code,
		Trace: Call{fun: fun, arg: arg, trace: nil},
	})
	machine.Env = Extend(closure.Env)
	Bind(machine.Env, closure.Param, arg)
	machine.Code = closure.Code

	return nil
}

type NotFunctionError struct {
	Value Value
}

func (err NotFunctionError) Error() string {
	return fmt.Sprintf("not a function: %v", err.Value)
}

func (cmd Apply) Where() token.Token {
	return cmd.Token
}

func (cmd Apply) NestedString(level int) string {
	return fmt.Sprintf("%vapply", indent(level))
}

//exhaustruct:ignore
var _ Command = Apply{}

type Primitive struct {
	token token.Token
	Name  Name
}

func (cmd Primitive) Execute(m *Machine) error {
	return executePrimitive(m, cmd.token, cmd.Name)
}

func (cmd Primitive) Where() token.Token {
	return cmd.token
}

func (cmd Primitive) NestedString(level int) string {
	return fmt.Sprintf("%vprim %v", indent(level), cmd.Name.Value())
}

//exhaustruct:ignore
var _ Command = Primitive{}

// Proj projects a field from a record.
type Proj struct {
	token token.Token
	Field Name
}

func (cmd Proj) Execute(machine *Machine) error {
	value, stack := machine.Stack.Head, machine.Stack.Tail
	machine.Stack = stack

	record, ok := value.(Record)
	if !ok {
		if symbol, ok := value.(Symbol); ok {
			symbol.trace = Access{receiver: value, name: cmd.Field, trace: symbol.trace}
			machine.Stack = machine.Stack.Push(symbol)

			return nil
		}

		return utils.PosError{Where: cmd.token, Err: NotRecordError{Value: value}}
	}

	if code, ok := record.Fields[cmd.Field]; ok {
		machine.Dump = machine.Dump.Push(Dump{
			Env:   machine.Env,
			Code:  machine.Code,
			Trace: Access{receiver: value, name: cmd.Field, trace: nil},
		})
		machine.Env = Extend(record.Env)
		machine.Code = code

		return nil
	}

	return utils.PosError{Where: cmd.token, Err: NoSuchFieldError{Field: cmd.Field}}
}

type NotRecordError struct {
	Value Value
}

func (err NotRecordError) Error() string {
	return fmt.Sprintf("not a record: %v", err.Value)
}

type NoSuchFieldError struct {
	Field Name
}

func (err NoSuchFieldError) Error() string {
	return fmt.Sprintf("no such field: %v", err.Field)
}

func (cmd Proj) Where() token.Token {
	return cmd.token
}

func (cmd Proj) NestedString(level int) string {
	return fmt.Sprintf("%vproj %v", indent(level), cmd.Field.Value())
}

//exhaustruct:ignore
var _ Command = Proj{}

// Assign assigns the top value of the stack to a variable in the environment.
type Assign struct {
	token token.Token
	Bind  Pattern
}

func (cmd Assign) Execute(machine *Machine) error {
	value, stack := machine.Stack.Head, machine.Stack.Tail
	machine.Stack = stack

	if cmd.Bind.Match(machine.Env.Head, value) {
		return nil
	}

	return utils.PosError{Where: cmd.token, Err: NotMatchError{Pattern: cmd.Bind, Value: value}}
}

type NotMatchError struct {
	Pattern Pattern
	Value   Value
}

func (err NotMatchError) Error() string {
	return fmt.Sprintf("pattern match failed: %v %v", err.Pattern, err.Value)
}

func (cmd Assign) Where() token.Token {
	return cmd.token
}

func (cmd Assign) NestedString(level int) string {
	return fmt.Sprintf("%vset %v", indent(level), cmd.Bind)
}

//exhaustruct:ignore
var _ Command = Assign{}

func indent(level int) string {
	return strings.Repeat("\t", level)
}
