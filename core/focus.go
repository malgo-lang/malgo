package core

type Focus struct {
	*Converter
}

func (f *Focus) Focus(node Node) Node {
	switch node := node.(type) {
	// Producer
	case *Var:
		return f.focusVar(node)
	case *Literal:
		return f.focusLiteral(node)
	case *Symbol:
		return f.focusSymbol(node)
	case *Do:
		return f.focusDo(node)
	case *Cocase:
		return f.focusCocase(node)
	// Consumer
	case *Then:
		return f.focusThen(node)
	case *Case:
		return f.focusCase(node)
	case *Destruct:
		return f.focusDestruct(node)
	// Statement
	case *Cut:
		return f.focusCut(node)
	case *Prim:
		return f.focusPrim(node)
	case *Invoke:
		return f.focusInvoke(node)
	// Other
	case *Def:
		return f.FocusDef(node)
	case *Extract:
		return node
	case *Method:
		return f.focusMethod(node)
	case *Clause:
		return f.focusClause(node)
	}

	panic("unreachable: Focus")
}

func (f *Focus) focusVar(node *Var) Node {
	return node
}

func (f *Focus) focusLiteral(node *Literal) Node {
	return node
}

func (f *Focus) focusSymbol(node *Symbol) Node {
	return node
}

func (f *Focus) focusDo(node *Do) Node {
	body, ok := f.Focus(node.Body).(Statement)
	if !ok {
		panic("unreachable: focusDo")
	}

	return &Do{
		Name: node.Name,
		Body: body,
	}
}

func (f *Focus) focusCocase(node *Cocase) Node {
	methods := make([]*Method, len(node.Methods))
	for i, method := range node.Methods {
		var ok bool
		methods[i], ok = f.Focus(method).(*Method)
		if !ok {
			panic("unreachable: focusCocase")
		}
	}

	return &Cocase{
		Methods: methods,
	}
}

func (f *Focus) focusThen(node *Then) Node {
	body, ok := f.Focus(node.Body).(Statement)
	if !ok {
		panic("unreachable: focusThen")
	}

	return &Then{
		Name: node.Name,
		Body: body,
	}
}

func (f *Focus) focusCase(node *Case) Node {
	clauses := make([]*Clause, len(node.Clauses))
	for i, clause := range node.Clauses {
		var ok bool
		clauses[i], ok = f.Focus(clause).(*Clause)
		if !ok {
			panic("unreachable: focusCase")
		}
	}

	return &Case{
		Clauses: clauses,
	}
}

func (f *Focus) focusDestruct(node *Destruct) Consumer {
	values, rest := splitBy(node.Args, func(p Producer) bool {
		return p.isValue()
	})

	if len(rest) == 0 {
		return f.focusDestructSimple(node)
	}

	first, ok := f.Focus(rest[0]).(Producer)
	if !ok {
		panic("unreachable: focusDestruct")
	}
	rest = rest[1:]

	outer := f.newName("y", node.Base().Location)
	inner := f.newName("x", node.Base().Location)

	destruct := f.focusDestruct(&Destruct{
		Name:  node.Name,
		Args:  append(append(values, &Var{Name: inner}), rest...),
		Conts: node.Conts,
	})

	return &Then{
		Name: outer,
		Body: &Cut{
			Producer: first,
			Consumer: &Then{
				Name: inner,
				Body: &Cut{
					Producer: &Var{Name: outer},
					Consumer: destruct,
				},
			},
		},
	}
}

func (f *Focus) focusDestructSimple(node *Destruct) Consumer {
	args := make([]Producer, len(node.Args))
	for i, arg := range node.Args {
		var ok bool
		args[i], ok = f.Focus(arg).(Producer)
		if !ok {
			panic("unreachable: focusDestruct")
		}
	}

	conts := make([]Consumer, len(node.Conts))
	for i, cont := range node.Conts {
		var ok bool
		conts[i], ok = f.Focus(cont).(Consumer)
		if !ok {
			panic("unreachable: focusDestruct")
		}
	}

	return &Destruct{
		Name:  node.Name,
		Args:  args,
		Conts: conts,
	}
}

func (f *Focus) focusCut(node *Cut) Node {
	producer, ok := f.Focus(node.Producer).(Producer)
	if !ok {
		panic("unreachable: focusCut")
	}

	consumer, ok := f.Focus(node.Consumer).(Consumer)
	if !ok {
		panic("unreachable: focusCut")
	}

	return &Cut{
		Producer: producer,
		Consumer: consumer,
	}
}

func (f *Focus) focusPrim(node *Prim) Statement {
	values, rest := splitBy(node.Args, func(p Producer) bool {
		return p.isValue()
	})

	if len(rest) == 0 {
		return f.focusPrimSimple(node)
	}

	first, ok := f.Focus(rest[0]).(Producer)
	if !ok {
		panic("unreachable: focusPrim")
	}

	rest = rest[1:]

	bind := f.newName("x", node.Base().Location)

	return &Cut{
		Producer: first,
		Consumer: &Then{
			Name: bind,
			Body: f.focusPrim(&Prim{
				Name: node.Name,
				Args: append(append(values, &Var{Name: bind}), rest...),
				Cont: node.Cont,
			}),
		},
	}
}

func (f *Focus) focusPrimSimple(node *Prim) Statement {
	args := make([]Producer, len(node.Args))
	for i, arg := range node.Args {
		var ok bool
		args[i], ok = f.Focus(arg).(Producer)
		if !ok {
			panic("unreachable: focusPrim")
		}
	}

	cont, ok := f.Focus(node.Cont).(Consumer)
	if !ok {
		panic("unreachable: focusPrim")
	}

	return &Prim{
		Name: node.Name,
		Args: args,
		Cont: cont,
	}
}

func (f *Focus) focusInvoke(node *Invoke) Node {
	conts := make([]Consumer, len(node.Conts))
	for i, cont := range node.Conts {
		var ok bool
		conts[i], ok = f.Focus(cont).(Consumer)
		if !ok {
			panic("unreachable: focusInvoke")
		}
	}

	return &Invoke{
		Name:  node.Name,
		Conts: conts,
	}
}

func (f *Focus) FocusDef(node *Def) *Def {
	body, ok := f.Focus(node.Body).(Statement)
	if !ok {
		panic("unreachable: focusDef")
	}

	return &Def{
		Name:    node.Name,
		Returns: node.Returns,
		Body:    body,
	}
}

func (f *Focus) focusMethod(node *Method) Node {
	body, ok := f.Focus(node.Body).(Statement)
	if !ok {
		panic("unreachable: focusMethod")
	}

	return &Method{
		Name:   node.Name,
		Params: node.Params,
		Labels: node.Labels,
		Body:   body,
	}
}

func (f *Focus) focusClause(node *Clause) Node {
	body, ok := f.Focus(node.Body).(Statement)
	if !ok {
		panic("unreachable: focusClause")
	}

	return &Clause{
		Pattern: node.Pattern,
		Body:    body,
	}
}

func splitBy[T any](slice []T, pred func(p T) bool) ([]T, []T) {
	var init []T
	for _, elem := range slice {
		if pred(elem) {
			init = append(init, elem)
		}
	}

	return init, slice[len(init):]
}
