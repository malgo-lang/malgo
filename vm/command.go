package vm

import (
	"fmt"
	"strings"

	"github.com/takoeight0821/malgo/token"
	"github.com/takoeight0821/malgo/utils"
)

// Push pushes a value onto the stack.
type Push struct {
	token token.Token
	Value Value
}

func (cmd Push) Where() token.Token {
	return cmd.token
}

func (cmd Push) NestedString(level int) string {
	return fmt.Sprintf("%v%v", indent(level), cmd.Value)
}

var _ Command = Push{}

// Get gets a value from the environment and pushes it onto the stack.
type Get struct {
	token token.Token
	Name  string
}

func (cmd Get) Where() token.Token {
	return cmd.token
}

func (cmd Get) NestedString(level int) string {
	return fmt.Sprintf("%vget %v", indent(level), cmd.Name)
}

var _ Command = Get{}

type MkTuple struct {
	token token.Token
	Count int
}

func (cmd MkTuple) Where() token.Token {
	return cmd.token
}

func (cmd MkTuple) NestedString(level int) string {
	return fmt.Sprintf("%vtuple %v", indent(level), cmd.Count)
}

// Select selects a branch based on the top value of the stack.
type Select struct {
	token    token.Token
	Count    int
	Branches []Branch
}

func (cmd Select) Where() token.Token {
	return cmd.token
}

func (cmd Select) NestedString(level int) string {
	var builder strings.Builder
	fmt.Fprintf(&builder, "%vselect %d {\n", indent(level), cmd.Count)

	for _, branch := range cmd.Branches {
		builder.WriteString(branch.NestedString(level))
		builder.WriteString("\n")
	}

	fmt.Fprintf(&builder, "%v}", indent(level))

	return builder.String()
}

var _ Command = Select{}

type Branch struct {
	token   token.Token
	Pattern []Pattern
	Code    Code
}

func (branch Branch) Where() token.Token {
	return branch.token
}

func (branch Branch) NestedString(level int) string {
	var builder strings.Builder

	fmt.Fprintf(&builder, "%vcase", indent(level+1))

	for _, pat := range branch.Pattern {
		fmt.Fprintf(&builder, " %v", pat)
	}

	fmt.Fprintf(&builder, " {")

	for cmd := range branch.Code.All() {
		fmt.Fprintf(&builder, "\n%s", cmd.NestedString(level+2))
	}

	fmt.Fprintf(&builder, "\n%v}", indent(level+1))

	return builder.String()
}

// Join finishes a branch and restores the code from the dump.
type Join struct {
	token token.Token
}

func (cmd Join) Where() token.Token {
	return cmd.token
}

func (cmd Join) NestedString(level int) string {
	return fmt.Sprintf("%vjoin", indent(level))
}

var _ Command = Join{}

// Lambda creates a new (recursive) closure and pushes it onto the stack.
type Lambda struct {
	token token.Token
	Param string
	Code  Code
}

func (cmd Lambda) Where() token.Token {
	return cmd.token
}

func (cmd Lambda) NestedString(level int) string {
	var builder strings.Builder

	fmt.Fprintf(&builder, "%vlam %v {", indent(level), cmd.Param)

	for c := range cmd.Code.All() {
		fmt.Fprintf(&builder, "\n%s", c.NestedString(level+1))
	}

	fmt.Fprintf(&builder, "\n%v}", indent(level))

	return builder.String()
}

var _ Command = Lambda{}

type Return struct {
	token token.Token
}

func (cmd Return) Where() token.Token {
	return cmd.token
}

func (cmd Return) NestedString(level int) string {
	return fmt.Sprintf("%vret", indent(level))
}

var _ Command = Return{}

type Object struct {
	token  token.Token
	Fields map[string]Code
}

func (cmd Object) Where() token.Token {
	return cmd.token
}

func (cmd Object) NestedString(level int) string {
	var builder strings.Builder

	fmt.Fprintf(&builder, "%vobject {", indent(level))

	for field, code := range utils.Ordered(cmd.Fields) {
		fmt.Fprintf(&builder, "\n%v%v {", indent(level+1), field)

		for c := range code.All() {
			fmt.Fprintf(&builder, "\n%s", c.NestedString(level+2))
		}

		fmt.Fprintf(&builder, "\n%v}", indent(level+1))
	}

	fmt.Fprintf(&builder, "\n%v}", indent(level))

	return builder.String()
}

var _ Command = Object{}

// Apply applies a closure to the top value of the stack.
type Apply struct {
	token token.Token
}

func (cmd Apply) Where() token.Token {
	return cmd.token
}

func (cmd Apply) NestedString(level int) string {
	return fmt.Sprintf("%vapply", indent(level))
}

var _ Command = Apply{}

type Primitive struct {
	token token.Token
	Name  string
}

func (cmd Primitive) Where() token.Token {
	return cmd.token
}

func (cmd Primitive) NestedString(level int) string {
	return fmt.Sprintf("%vprim %v", indent(level), cmd.Name)
}

var _ Command = Primitive{}

// Proj projects a field from a record.
type Proj struct {
	token token.Token
	Field string
}

func (cmd Proj) Where() token.Token {
	return cmd.token
}

func (cmd Proj) NestedString(level int) string {
	return fmt.Sprintf("%vproj %v", indent(level), cmd.Field)
}

// Assign assigns the top value of the stack to a variable in the environment.
type Assign struct {
	token token.Token
	Bind  Pattern
}

func (cmd Assign) Where() token.Token {
	return cmd.token
}

func (cmd Assign) NestedString(level int) string {
	return fmt.Sprintf("%vset %v", indent(level), cmd.Bind)
}

var _ Command = Assign{}

func indent(level int) string {
	return strings.Repeat("\t", level)
}
