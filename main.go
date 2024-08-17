package main

import (
	"errors"
	"flag"
	"fmt"
	"os"
	"path/filepath"

	"github.com/adrg/xdg"
	"github.com/peterh/liner"
	"github.com/takoeight0821/malgo/driver"
	"github.com/takoeight0821/malgo/eval"
	"github.com/takoeight0821/malgo/nameresolve"
	"github.com/takoeight0821/malgo/token"
)

func main() {
	const (
		inputUsage = "input file path"
	)
	var inputPath string
	flag.StringVar(&inputPath, "input", "", inputUsage)
	flag.StringVar(&inputPath, "i", "", inputUsage+" (shorthand)")

	flag.Parse()

	if inputPath == "" {
		// If no input file is specified, run the REPL.
		err := RunPrompt()
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
	} else {
		if err := RunFile(inputPath); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
	}
}

func historyPath() string {
	return filepath.Join(xdg.DataHome, "malgo", ".malgo_history")
}

// writeHistory writes the history of the REPL to a file.
func writeHistory(line *liner.State) {
	// Create the directory for the history file if it does not exist.
	if err := os.MkdirAll(filepath.Dir(historyPath()), os.ModePerm); err != nil {
		fmt.Fprintln(os.Stderr, err)
	}
	// Write the history file.
	// If the file does not exist, it will be created automatically.
	// If the file exists, it will be overwritten.
	if f, err := os.Create(historyPath()); err == nil {
		defer f.Close()
		if _, err := line.WriteHistory(f); err != nil {
			fmt.Fprintln(os.Stderr, err)
		}
	}
	line.Close()
}

// readHistory reads the history of the REPL from a file.
func readHistory(line *liner.State) {
	if f, err := os.Open(historyPath()); err == nil {
		defer f.Close()
		if _, err := line.ReadHistory(f); err != nil {
			fmt.Fprintln(os.Stderr, err)
		}
	}
}

// RunPrompt runs the REPL.
func RunPrompt() error {
	line := liner.NewLiner()
	defer writeHistory(line)
	readHistory(line)

	runner := driver.NewPassRunner()
	driver.AddPassesUntil(runner, nameresolve.NewResolver())

	evaluator := eval.NewEvaluator()

	for {
		input, err := line.Prompt("> ")
		if err != nil {
			return fmt.Errorf("prompt: %w", err)
		}
		line.AppendHistory(input)

		nodes, err := runner.RunSource("repl", input)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)

			continue
		}

		// Evaluate all nodes.
		for _, node := range nodes {
			value, err := evaluator.Eval(node)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error: %v\n", err)

				continue
			}
			fmt.Println(value)
		}
	}
}

// RunFile runs the specified file.
func RunFile(path string) error {
	runner := driver.NewPassRunner()
	driver.AddPassesUntil(runner, nameresolve.NewResolver())

	// Read the source code from the file.
	bytes, err := os.ReadFile(path)
	if err != nil {
		return fmt.Errorf("read file: %w", err)
	}

	nodes, err := runner.RunSource(path, string(bytes))
	if err != nil {
		return fmt.Errorf("run file: %w", err)
	}

	evaluator := eval.NewEvaluator()
	// Evaluate all nodes for loading definitions.
	for _, node := range nodes {
		_, err := evaluator.Eval(node)
		if err != nil {
			return fmt.Errorf("run file: %w", err)
		}
	}

	main, ok := evaluator.SearchMain()
	if !ok {
		return noMainError{}
	}
	// top is a dummy token.
	top := token.Token{Kind: token.IDENT, Lexeme: "toplevel", Location: token.Location{}, Literal: -1}
	_, err = main.Apply(top, eval.Unit())
	var exitErr eval.ExitError
	if errors.As(err, &exitErr) {
		os.Exit(exitErr.Code)
	} else if err != nil {
		return fmt.Errorf("run file: %w", err)
	}

	return nil
}

type noMainError struct{}

func (noMainError) Error() string {
	return "no main function"
}
