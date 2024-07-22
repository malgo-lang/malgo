package eval

import (
	"fmt"
	"io"

	"github.com/takoeight0821/malgo/token"
	"github.com/takoeight0821/malgo/utils"
	"github.com/xlab/treeprint"
)

type primitiveEvaluator struct {
	*Evaluator
	where token.Token
}

type primitive func(...Value) (Value, error)

func (p *primitiveEvaluator) primitive(name string) primitive {
	pmap := map[string]primitive{
		"exit":         p.exit,
		"print_cps":    p.printCPS,
		"read_all_cps": p.readAllCPS,
		"print":        p.print,
		"print_trace":  p.printTrace,
		"mul":          p.mul,
		"add":          p.add,
		"sub":          p.sub,
		"less_equal":   p.lessEqual,
	}

	return pmap[name]
}

func (p *primitiveEvaluator) exit(args ...Value) (Value, error) {
	if len(args) != 0 {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentCountError{Expected: 0, Actual: len(args)}}
	}

	return nil, ExitError{Code: 0}
}

func (p *primitiveEvaluator) printCPS(args ...Value) (Value, error) {
	if len(args) != 2 {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentCountError{Expected: 2, Actual: len(args)}}
	}

	arg, ok := args[0].(String)
	if !ok {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentTypeError{Expected: "String", Actual: args[0]}}
	}

	fmt.Fprintf(p.Stdout, "%s", arg.value)

	cont, ok := args[1].(Callable)
	if !ok {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentTypeError{Expected: "Callable", Actual: args[1]}}
	}

	result, err := cont.Apply(p.where)
	if err != nil {
		return nil, utils.PosError{Where: p.where, Err: err}
	}

	return result, nil
}

func (p *primitiveEvaluator) readAllCPS(args ...Value) (Value, error) {
	if len(args) != 1 {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentCountError{
			Expected: 1,
			Actual:   len(args),
		}}
	}
	cont, ok := args[0].(Callable)
	if !ok {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentTypeError{Expected: "Callable", Actual: args[0]}}
	}
	bytes, err := io.ReadAll(p.Stdin)
	if err != nil {
		return nil, utils.PosError{Where: p.where, Err: err}
	}

	result, err := cont.Apply(p.where, String{value: string(bytes)})
	if err != nil {
		return nil, utils.PosError{Where: p.where, Err: err}
	}

	return result, nil
}

func (p *primitiveEvaluator) print(args ...Value) (Value, error) {
	if len(args) != 1 {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentCountError{Expected: 1, Actual: len(args)}}
	}
	fmt.Fprintln(p.Stdout, args[0])

	return Unit(), nil
}

func (p *primitiveEvaluator) printTrace(args ...Value) (Value, error) {
	if len(args) != 1 {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentCountError{Expected: 1, Actual: len(args)}}
	}
	tree := treeprint.New()
	fmt.Fprintln(p.Stdout, TraceAsTree(args[0], args[0].Trace(), tree))

	return Unit(), nil
}

func (p *primitiveEvaluator) mul(args ...Value) (Value, error) {
	if len(args) != 2 {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentCountError{Expected: 2, Actual: len(args)}}
	}
	left, ok := asInt(args[0])
	if !ok {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentTypeError{Expected: "Int", Actual: args[0]}}
	}
	right, ok := asInt(args[1])
	if !ok {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentTypeError{Expected: "Int", Actual: args[1]}}
	}

	return Int{value: left.value * right.value, trace: Root{}}, nil
}

func (p *primitiveEvaluator) add(args ...Value) (Value, error) {
	if len(args) != 2 {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentCountError{Expected: 2, Actual: len(args)}}
	}
	left, ok := asInt(args[0])
	if !ok {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentTypeError{Expected: "Int", Actual: args[0]}}
	}
	right, ok := asInt(args[1])
	if !ok {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentTypeError{Expected: "Int", Actual: args[1]}}
	}

	return Int{value: left.value + right.value, trace: Root{}}, nil
}

func (p *primitiveEvaluator) sub(args ...Value) (Value, error) {
	if len(args) != 2 {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentCountError{Expected: 2, Actual: len(args)}}
	}
	left, ok := asInt(args[0])
	if !ok {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentTypeError{Expected: "Int", Actual: args[0]}}
	}
	right, ok := asInt(args[1])
	if !ok {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentTypeError{Expected: "Int", Actual: args[1]}}
	}

	return Int{value: left.value - right.value, trace: Root{}}, nil
}

func (p *primitiveEvaluator) lessEqual(args ...Value) (Value, error) {
	if len(args) != 2 {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentCountError{Expected: 2, Actual: len(args)}}
	}
	left, ok := asInt(args[0])
	if !ok {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentTypeError{Expected: "Int", Actual: args[0]}}
	}
	right, ok := asInt(args[1])
	if !ok {
		return nil, utils.PosError{Where: p.where, Err: InvalidArgumentTypeError{Expected: "Int", Actual: args[1]}}
	}

	if left.value <= right.value {
		return Int{value: 1, trace: Root{}}, nil
	}

	return Int{value: 0, trace: Root{}}, nil
}
