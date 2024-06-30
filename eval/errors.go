package eval

import (
	"fmt"

	"github.com/takoeight0821/malgo/ast"
	"github.com/takoeight0821/malgo/token"
)

// UndefinedVariableError is an error that is returned when a variable is not defined.
type UndefinedVariableError struct {
	Name token.Token
}

func (e UndefinedVariableError) Error() string {
	return fmt.Sprintf("undefined variable `%v`", e.Name)
}

// InvalidLiteralError is an error that is returned when a token of given literal is invalid.
type InvalidLiteralError struct {
	Kind token.Kind
}

func (e InvalidLiteralError) Error() string {
	return fmt.Sprintf("invalid literal `%v`", e.Kind)
}

// UndefinedFieldError is an error that is returned when a field is not defined.
type UndefinedFieldError struct {
	Receiver Object
	Name     string
}

func (e UndefinedFieldError) Error() string {
	return fmt.Sprintf("undefined field `%v` of %s", e.Name, e.Receiver)
}

type NotObjectError struct {
	Receiver Value
}

func (e NotObjectError) Error() string {
	return fmt.Sprintf("not an object: %v", e.Receiver)
}

// InvalidIndexError is an error that is returned when the number of arguments is not equal to expected.
type InvalidArgumentCountError struct {
	Expected int
	Actual   int
}

func (e InvalidArgumentCountError) Error() string {
	return fmt.Sprintf("invalid argument count: expected %d, actual %d", e.Expected, e.Actual)
}

// NotCallableError is an error that is returned when a value is not Callable.
type NotCallableError struct {
	Func Value
}

func (e NotCallableError) Error() string {
	return fmt.Sprintf("not a function: %v", e.Func)
}

// InvalidArgumentTypeError is an error that is returned when the type of argument is not equal to expected.
type InvalidArgumentTypeError struct {
	Expected string
	Actual   Value
}

func (e InvalidArgumentTypeError) Error() string {
	return fmt.Sprintf("invalid argument type: expected %s, actual %v", e.Expected, e.Actual)
}

// UndefinedPrimError is an error that is returned when a primitive operator is not defined.
type UndefinedPrimError struct {
	Name token.Token
}

func (e UndefinedPrimError) Error() string {
	return fmt.Sprintf("undefined prim `%v`", e.Name)
}

// PatternMatchError is an error that is returned when a pattern match failed.
type PatternMatchError struct {
	Patterns []ast.Node
	Values   []Value
}

func (e PatternMatchError) Error() string {
	return fmt.Sprintf("pattern match failed: %v = %v", e.Patterns, e.Values)
}

// NotConstructorError is an error that is returned when a value is not Constructor.
type NotConstructorError struct {
	Node ast.Node
}

func (e NotConstructorError) Error() string {
	return fmt.Sprintf("not a constructor: %v", e.Node)
}
