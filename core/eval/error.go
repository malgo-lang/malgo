package eval

type AlreadyDefinedError struct {
	Name string
}

func (e AlreadyDefinedError) Error() string {
	return "already defined: " + e.Name
}

type NoMainError struct{}

func (NoMainError) Error() string {
	return "no main function found"
}
