package alpha

import "github.com/takoeight0821/malgo/internal/ast"

// {coalt^n} p^m
// p is a pattern, not a copattern.
func staticApply(info *ast.Info, lambda ast.LambdaCase, args []ast.Node) []ast.Clause {
	subst := make(map[ast.Ident]ast.Node)
	for i, param := range lambda.Parameters {
		subst[param] = args[i]
	}

	cases := make([]ast.Clause, 0, len(lambda.Cases))
	for _, c := range lambda.Cases {
		cases = append(cases, ast.Clause{Pattern: Convert(info, c.Pattern, subst).(ast.Pattern), Body: Convert(info, c.Body, subst)})
	}
	return cases
}

// H_i {coalt^n}
func staticAccess(label ast.Ident, object ast.Object) ast.Expr {
	return object.Fields[label]
}
