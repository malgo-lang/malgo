package eval

type env struct {
	parent   *env
	values   map[string]Value
	covalues map[string]Covalue
}

func newEnv(parent *env) *env {
	return &env{
		parent:   parent,
		values:   make(map[string]Value),
		covalues: make(map[string]Covalue),
	}
}

func (e *env) Get(name string) (Value, bool) {
	if v, ok := e.values[name]; ok {
		return v, true
	}

	if e.parent != nil {
		return e.parent.Get(name)
	}

	return Value{}, false
}

func (e *env) Set(name string, value Value) {
	e.values[name] = value
}

func (e *env) GetCo(name string) (Covalue, bool) {
	if v, ok := e.covalues[name]; ok {
		return v, true
	}

	if e.parent != nil {
		return e.parent.GetCo(name)
	}

	return Covalue{}, false
}

func (e *env) SetCo(name string, value Covalue) {
	e.covalues[name] = value
}
