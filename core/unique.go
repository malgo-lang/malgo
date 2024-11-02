package core

import "github.com/malgo-lang/malgo/token"

type UniqueGen struct {
	// The current value of the generator.
	value int
}

func NewGenerator() *UniqueGen {
	return &UniqueGen{value: -1}
}

// Fresh returns the next unique value.
func (g *UniqueGen) Fresh() int {
	g.value++

	return g.value
}

// FreshName returns the next unique name.
func (g *UniqueGen) FreshName(lexeme string, base token.Location) token.Token {
	return token.Token{
		Kind:     token.IDENT,
		Lexeme:   "$" + lexeme,
		Location: base,
		Literal:  g.Fresh(),
	}
}
