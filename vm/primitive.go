package vm

import (
	"fmt"
	"io"

	"github.com/takoeight0821/malgo/token"
	"github.com/takoeight0821/malgo/utils"
)

func executePrimitive(machine *Machine, where token.Token, name string) error {
	pmap := map[string]func(*Machine, token.Token) error{
		"exit":         primExit,
		"print_cps":    primPrintCPS,
		"read_all_cps": primReadAllCPS,
		"print":        primPrint,
		"print_trace":  primPrintTrace,
		"mul":          primMul,
		"add":          primAdd,
		"sub":          primSub,
		"less_equal":   primLessEqual,
	}

	return pmap[name](machine, where)
}

func primExit(_ *Machine, _ token.Token) error {
	return ExitError{Code: 0}
}

type ExitError struct {
	Code int
}

func (e ExitError) Error() string {
	return fmt.Sprintf("exit with code %d", e.Code)
}

//nolint:unparam
func primPrintCPS(machine *Machine, where token.Token) error {
	var cont Value
	var arg Value
	cont, machine.Stack = machine.Stack.Head, machine.Stack.Tail
	arg, machine.Stack = machine.Stack.Head, machine.Stack.Tail

	fmt.Fprintf(machine.Stdout, "%v\n", arg)

	machine.Stack = machine.Stack.Push(cont)
	machine.Code = machine.Code.Push(Apply{Token: where})
	machine.Code = machine.Code.Push(MkTuple{Token: where, Count: 0})

	return nil
}

func primReadAllCPS(machine *Machine, where token.Token) error {
	bytes, err := io.ReadAll(machine.Stdin)
	if err != nil {
		return utils.PosError{Where: where, Err: err}
	}

	machine.Stack = machine.Stack.Push(String{value: string(bytes), trace: Root{}})
	machine.Code = machine.Code.Push(Apply{Token: where})

	return nil
}

//nolint:unparam
func primPrint(machine *Machine, _ token.Token) error {
	var arg Value
	arg, machine.Stack = machine.Stack.Head, machine.Stack.Tail

	fmt.Fprintf(machine.Stdout, "%v\n", arg)

	machine.Stack = machine.Stack.Push(Tuple{fields: make([]Value, 0), trace: Root{}})

	return nil
}

//nolint:unparam
func primPrintTrace(machine *Machine, _ token.Token) error {
	var arg Value
	arg, machine.Stack = machine.Stack.Head, machine.Stack.Tail

	arg.Trace().Print(machine.Stdout, 0)

	machine.Stack = machine.Stack.Push(Tuple{fields: make([]Value, 0), trace: Root{}})

	return nil
}

func primMul(machine *Machine, where token.Token) error {
	var arg1 Value
	var arg2 Value
	arg2, machine.Stack = machine.Stack.Head, machine.Stack.Tail
	arg1, machine.Stack = machine.Stack.Head, machine.Stack.Tail

	if arg1, ok := arg1.(Int); ok {
		if arg2, ok := arg2.(Int); ok {
			machine.Stack = machine.Stack.Push(Int{value: arg1.value * arg2.value, trace: Root{}})

			return nil
		}
	}

	return utils.PosError{Where: where, Err: InvalidArgumentTypeError{Expected: "Int", Actual: arg1}}
}

func primAdd(machine *Machine, where token.Token) error {
	var arg1 Value
	var arg2 Value
	arg2, machine.Stack = machine.Stack.Head, machine.Stack.Tail
	arg1, machine.Stack = machine.Stack.Head, machine.Stack.Tail

	if arg1, ok := arg1.(Int); ok {
		if arg2, ok := arg2.(Int); ok {
			machine.Stack = machine.Stack.Push(Int{value: arg1.value + arg2.value, trace: Root{}})

			return nil
		}
	}

	return utils.PosError{Where: where, Err: InvalidArgumentTypeError{Expected: "Int", Actual: arg1}}
}

func primSub(machine *Machine, where token.Token) error {
	var arg1 Value
	var arg2 Value
	arg2, machine.Stack = machine.Stack.Head, machine.Stack.Tail
	arg1, machine.Stack = machine.Stack.Head, machine.Stack.Tail

	if arg1, ok := arg1.(Int); ok {
		if arg2, ok := arg2.(Int); ok {
			machine.Stack = machine.Stack.Push(Int{value: arg1.value - arg2.value, trace: Root{}})

			return nil
		}
	}

	return utils.PosError{Where: where, Err: InvalidArgumentTypeError{Expected: "Int", Actual: arg1}}
}

func primLessEqual(machine *Machine, where token.Token) error {
	var arg1 Value
	var arg2 Value
	arg2, machine.Stack = machine.Stack.Head, machine.Stack.Tail
	arg1, machine.Stack = machine.Stack.Head, machine.Stack.Tail

	if arg1, ok := arg1.(Int); ok {
		if arg2, ok := arg2.(Int); ok {
			if arg1.value <= arg2.value {
				machine.Stack = machine.Stack.Push(Int{value: 1, trace: Root{}})
			} else {
				machine.Stack = machine.Stack.Push(Int{value: 0, trace: Root{}})
			}

			return nil
		}
	}

	return utils.PosError{Where: where, Err: InvalidArgumentTypeError{Expected: "Int", Actual: arg1}}
}

type InvalidArgumentTypeError struct {
	Expected string
	Actual   Value
}

func (e InvalidArgumentTypeError) Error() string {
	return fmt.Sprintf("invalid argument type: expected %s, but got %v", e.Expected, e.Actual)
}
