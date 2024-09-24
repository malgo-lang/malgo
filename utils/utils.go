package utils

import (
	"cmp"
	"fmt"
	"io/fs"
	"iter"
	"maps"
	"path/filepath"
	"slices"
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/malgo-lang/malgo/token"
)

func IsUpper(s string) bool {
	if len(s) == 0 {
		return false
	}

	r, _ := utf8.DecodeRuneInString(s)

	return unicode.IsUpper(r)
}

// PosError represents an error that occurred at a specific position in the code.
type PosError struct {
	Where token.Token // The token indicating the position of the error.
	Err   error       // The underlying error.
}

func (e PosError) Error() string {
	if e.Where.Kind == token.EOF {
		return "at end: " + e.Err.Error()
	}

	return fmt.Sprintf("at %v: `%s`\n\t%s", e.Where.Location, e.Where.Lexeme, e.Err.Error())
}

func (e PosError) Unwrap() error {
	return e.Err
}

func FindSourceFiles(path string) ([]string, error) {
	var files []string
	err := filepath.WalkDir(path, func(path string, _ fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if filepath.Ext(path) == ".mlg" {
			files = append(files, path)
		}

		return nil
	})
	if err != nil {
		return nil, fmt.Errorf("find source files: %w", err)
	}

	return files, nil
}

// Parenthesize takes a head string and a variadic number of nodes that implement the fmt.Stringer interface.
// It returns a fmt.Stringer that represents a string where each node is parenthesized and separated by a space.
// If the head string is not empty, it is added at the beginning of the string.
//
//tool:ignore
func Parenthesize(head string, elems ...fmt.Stringer) fmt.Stringer {
	var builder strings.Builder
	builder.WriteString("(")
	elemsStr := Concat(elems).String()
	if head != "" {
		builder.WriteString(head)
	}
	if elemsStr != "" {
		if head != "" {
			builder.WriteString(" ")
		}
		builder.WriteString(elemsStr)
	}
	builder.WriteString(")")

	return &builder
}

// concat takes a slice of nodes that implement the fmt.Stringer interface.
// It returns a fmt.Stringer that represents a string where each node is separated by a space.
//
//tool:ignore
func Concat[T fmt.Stringer](elems []T) fmt.Stringer {
	var builder strings.Builder
	for i, elem := range elems {
		// ignore empty string
		// e.g. concat({}) == ""
		str := elem.String()
		if str == "" {
			continue
		}
		if i != 0 {
			builder.WriteString(" ")
		}
		builder.WriteString(str)
	}

	return &builder
}

// Ordered takes a map and returns a sequence of key-value pairs in the order of the keys.
// The keys are sorted in ascending order.
func Ordered[K cmp.Ordered, V any](m map[K]V) iter.Seq2[K, V] {
	keys := slices.Sorted(maps.Keys(m))

	return func(yield func(K, V) bool) {
		for _, key := range keys {
			if !yield(key, m[key]) {
				break
			}
		}
	}
}
