package vm

import (
	"fmt"

	"github.com/takoeight0821/malgo/ast"
	"github.com/takoeight0821/malgo/token"
)

func Compile(node ast.Node, cont *Stack[Command]) (*Stack[Command], error) {
	switch node := node.(type) {
	case *ast.Var:
		name := tokenToName(node.Name)

		return cons(Get{token: node.Base(), Name: name}, cont), nil
	case *ast.Literal:
		return compileLiteral(node, cont), nil
	case *ast.Symbol:
		name := tokenToName(node.Name)

		return cons(Push{token: node.Base(), Value: Symbol{name: name}}, cont), nil
	case *ast.Tuple:
		return compileTuple(node, cont)
	case *ast.Access:
		return compileAccess(node, cont)
	case *ast.Call:
		return compileCall(node, cont)
	case *ast.Prim:
		return compilePrim(node, cont)
	case *ast.Let:
		return compileLet(node, cont)
	case *ast.Seq:
		return compileSeq(node, cont)
	case *ast.Case:
		return compileCase(node, cont)
	}

	panic(fmt.Sprintf("Compile: %T", node))
}

func compileLiteral(node *ast.Literal, cont *Stack[Command]) *Stack[Command] {
	//exhaustive:ignore
	switch node.Kind {
	case token.INTEGER:
		if value, ok := node.Literal.(int); !ok {
			panic(fmt.Sprintf("compileLiteral: %s", node))
		} else {
			return cons(Push{token: node.Base(), Value: Int{value: value}}, cont)
		}
	case token.STRING:
		if value, ok := node.Literal.(string); !ok {
			panic(fmt.Sprintf("compileLiteral: %s", node))
		} else {
			return cons(Push{token: node.Base(), Value: String{value: value}}, cont)
		}
	}

	panic(fmt.Sprintf("compileLiteral: %s", node))
}

func compileTuple(node *ast.Tuple, cont *Stack[Command]) (*Stack[Command], error) {
	cont = cons(MkTuple{token: node.Base(), Count: len(node.Exprs)}, cont)
	for i := len(node.Exprs) - 1; i >= 0; i-- {
		var err error
		cont, err = Compile(node.Exprs[i], cont)
		if err != nil {
			return nil, err
		}
	}

	return cont, nil
}

func compileAccess(node *ast.Access, cont *Stack[Command]) (*Stack[Command], error) {
	cont = cons(Proj{token: node.Base(), Field: tokenToName(node.Name)}, cont)

	return Compile(node.Receiver, cont)
}

func compileCall(node *ast.Call, cont *Stack[Command]) (*Stack[Command], error) {
	if len(node.Args) != 1 {
		panic(fmt.Sprintf("compileCall: %s", node))
	}

	cont = cons(Apply{token: node.Base()}, cont)

	var err error
	cont, err = Compile(node.Args[0], cont)
	if err != nil {
		return nil, err
	}

	cont, err = Compile(node.Func, cont)
	if err != nil {
		return nil, err
	}

	return cont, nil
}

func compilePrim(node *ast.Prim, cont *Stack[Command]) (*Stack[Command], error) {
	cont = cons(Primitive{token: node.Base(), Name: tokenToName(node.Name)}, cont)

	for i := len(node.Args) - 1; i >= 0; i-- {
		var err error
		cont, err = Compile(node.Args[i], cont)
		if err != nil {
			return nil, err
		}
	}

	return cont, nil
}

func compileLet(node *ast.Let, cont *Stack[Command]) (*Stack[Command], error) {
	pattern, err := compilePattern(node.Bind)
	if err != nil {
		return nil, err
	}

	cont = cons(Assign{token: node.Base(), Bind: pattern}, cont)

	return Compile(node.Body, cont)
}

func compilePattern(node ast.Node) (Pattern, error) {
	switch node := node.(type) {
	case *ast.Var:
		name := tokenToName(node.Name)

		return Var{name: name}, nil
	}

	panic(fmt.Sprintf("compilePattern: %T", node))
}

func compileSeq(node *ast.Seq, cont *Stack[Command]) (*Stack[Command], error) {
	for i := len(node.Exprs) - 1; i >= 0; i-- {
		var err error
		cont, err = Compile(node.Exprs[i], cont)
		if err != nil {
			return nil, err
		}
	}

	return cont, nil
}

func compileCase(node *ast.Case, cont *Stack[Command]) (*Stack[Command], error) {
	for i := len(node.Scrutinees) - 1; i >= 0; i-- {
		var err error
		cont, err = Compile(node.Scrutinees[i], cont)
		if err != nil {
			return nil, err
		}
	}

	branches := make([]Branch, len(node.Clauses))

	for i, clause := range node.Clauses {
		if len(clause.Patterns) != len(node.Scrutinees) {
			panic(fmt.Sprintf("compileCase: %s", node))
		}

		patterns := make([]Pattern, len(clause.Patterns))
		for j, pattern := range clause.Patterns {
			var err error
			patterns[j], err = compilePattern(pattern)
			if err != nil {
				return nil, err
			}
		}

		var err error
		branches[i].token = clause.Base()
		branches[i].Pattern = patterns
		branches[i].Code, err = Compile(clause.Expr, &Stack[Command]{Head: Join{token: clause.Base()}, Tail: nil})
		if err != nil {
			return nil, err
		}
	}

	return cons(Select{
		token:    node.Base(),
		Count:    len(node.Scrutinees),
		Branches: branches,
	}, cont), nil
}

func tokenToName(tok token.Token) string {
	if tok.Kind != token.IDENT && tok.Kind != token.OPERATOR {
		panic(fmt.Sprintf("tokenToName: %s", tok))
	}

	return fmt.Sprintf("%s.%#v", tok.Lexeme, tok.Literal)
}

func cons(command Command, cont *Stack[Command]) *Stack[Command] {
	return &Stack[Command]{command, cont}
}
