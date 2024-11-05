package eval

import (
	"fmt"
	"strings"

	"github.com/malgo-lang/malgo/pretty"
)

type ExitError struct {
	Code int
}

func (e ExitError) Error() string {
	return fmt.Sprintf("exit with code %d", e.Code)
}

type AlreadyDefinedError struct {
	Name string
}

func (e AlreadyDefinedError) Error() string {
	return "already defined: " + e.Name
}

type UndefinedError struct {
	Name string
}

func (e UndefinedError) Error() string {
	return "undefined: " + e.Name
}

type InvalidValueError struct {
	Expect string
	Actual string
}

func (e InvalidValueError) Error() string {
	return "invalid value: expect " + e.Expect + ", but got " + e.Actual
}

type NoMethodError struct {
	Expect string
	Given  []string
}

func (e NoMethodError) Error() string {
	return "no method: expect " + e.Expect + ", but got " + strings.Join(e.Given, ", ")
}

type InvalidPatternError struct {
	Expect string
	Actual string
}

func (e InvalidPatternError) Error() string {
	return "invalid pattern: expect " + e.Expect + ", but got " + e.Actual
}

type InvalidTraceError struct {
	Expect string
	Actual string
}

func (e InvalidTraceError) Error() string {
	return "invalid trace: expect " + e.Expect + ", but got " + e.Actual
}

type NoMatchError struct {
	Value Value
}

func (e NoMatchError) Error() string {
	return pretty.Join(pretty.String("no match:"), e.Value).Pretty(0).String()
}

type NoMainError struct{}

func (NoMainError) Error() string {
	return "no main function found"
}

type NotEnoughArgumentsError struct{}

func (NotEnoughArgumentsError) Error() string {
	return "not enough arguments"
}
