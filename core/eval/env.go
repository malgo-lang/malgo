package eval

type env struct {
	parent *env
	values map[string]Value
}

func newEnv(parent *env) *env {
	return &env{
		parent: parent,
		values: make(map[string]Value),
	}
}
