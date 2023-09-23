package ast

import "fmt"

// Infomations across the compile unit.
type Info struct {
	Input string
	Names map[string]int
}

func NewInfo(input string) *Info {
	return &Info{
		Input: input,
		Names: map[string]int{},
	}
}

type ID struct {
	RawName string
	Unique  int
}

func (id ID) Name() string {
	return id.RawName + "_" + fmt.Sprint(id.Unique)
}

func (i *Info) NewName(name Ident) ID {
	if _, ok := i.Names[name.Name()]; !ok {
		i.Names[name.Name()] = 0
		return ID{RawName: name.Name(), Unique: 0}
	}

	i.Names[name.Name()]++
	return ID{RawName: name.Name(), Unique: i.Names[name.Name()]}
}
