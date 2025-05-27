package repl

import (
	"bufio"
	"fmt"
	"io"
	"monkey/evaluator"
	"monkey/lexer"
	"monkey/object"
	"monkey/parser"
)

// PROMPT is the string displayed to the user at the beginning of each line in the REPL,
// indicating that the system is ready for input. For example: ">> ".
const PROMPT = ">> "

// Start initializes and runs the Monkey Read-Eval-Print Loop (REPL).
// The REPL allows users to interactively type Monkey language expressions and statements,
// see their results, and experiment with the language.
// - `in`: An io.Reader from which user input is read (typically os.Stdin).
// - `out`: An io.Writer to which output (prompts, results, errors) is written (typically os.Stdout).
func Start(in io.Reader, out io.Writer) {
	// scanner is used to read user input line by line from the `in` stream.
	// bufio.NewScanner provides a convenient way to handle buffered input.
	scanner := bufio.NewScanner(in)

	// env is the evaluation environment that persists throughout the REPL session.
	// It stores variable bindings created by `let` statements. A new, clean environment
	// is created for each REPL session using object.NewEnvironment().
	env := object.NewEnvironment()

	// The main REPL loop continues indefinitely until the input stream is closed
	// or an unrecoverable error occurs (though this REPL handles parser errors gracefully).
	for {
		// Prompting: Display the PROMPT to the user via the `out` stream.
		// Using Printf directly to `out` might be more idiomatic if `out` is not always os.Stdout,
		// but `fmt.Printf` (to os.Stdout by default if `out` is not specified, here it writes to `out` via Fprintf effectively)
		// is used, which is fine.
		// A more direct way would be `io.WriteString(out, PROMPT)`.
		// The current code `fmt.Printf(PROMPT)` will print to standard output if `out` is not considered by Printf.
		// For robustness, explicitly writing to `out` is better:
		io.WriteString(out, PROMPT) // Corrected to explicitly use `out`.

		// Reading Input: scanner.Scan() reads the next line of input from the user.
		// It returns false if there's an error or if the end of the input stream is reached.
		scanned := scanner.Scan()
		if !scanned {
			// If scanning fails (e.g., EOF from Ctrl+D), exit the REPL.
			return
		}

		// Get the line of text that was read.
		line := scanner.Text()

		// Lexing: Create a new lexer (`lexer.New`) for each line of input.
		// The lexer tokenizes the input string into a stream of tokens.
		l := lexer.New(line)

		// Parsing: Create a new parser (`parser.New`) with the lexer.
		p := parser.New(l)
		// Parse the program to generate an Abstract Syntax Tree (AST).
		program := p.ParseProgram()

		// Error Handling (Parsing): Check if the parser encountered any errors.
		if len(p.Errors()) != 0 {
			// If there are parser errors, print them using `printParserErrors`
			// and then `continue` to the next iteration of the loop to get fresh input.
			// This allows the REPL to remain active even if the user types syntactically incorrect code.
			printParserErrors(out, p.Errors())
			continue
		}

		// Evaluation: If parsing was successful (no errors), evaluate the AST (`program`)
		// using `evaluator.Eval()` within the current persistent `env`.
		evaluated := evaluator.Eval(program, env)

		// Output: If the evaluation resulted in a non-null object (Eval might return nil for unhandled AST nodes,
		// though ideally it should always return an object or an error object),
		// print its string representation (via `Inspect()`) to the `out` stream, followed by a newline.
		if evaluated != nil {
			io.WriteString(out, evaluated.Inspect())
			io.WriteString(out, "\n")
		}
	}
}

// MONKEY_FACE is an ASCII art representation of a monkey, used in error messages
// to add a bit of thematic flair.
const MONKEY_FACE = `            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
`

// printParserErrors is a helper function to display parser errors to the user in a formatted way.
// - `out`: The io.Writer to which the errors are written.
// - `errors`: A slice of strings, where each string is a parser error message.
// It prints a "monkey face" ASCII art, a playful error message, and then lists
// each parser error, indented for readability. This signifies that something went wrong during parsing.
func printParserErrors(out io.Writer, errors []string) {
	// Print the ASCII art monkey face for a touch of whimsy.
	io.WriteString(out, MONKEY_FACE)
	io.WriteString(out, "Woops! We ran into some monkey business here!\n")
	io.WriteString(out, " parser errors:\n")
	// Iterate over the collected parser errors and print each one, indented.
	for _, msg := range errors {
		io.WriteString(out, "\t"+msg+"\n")
	}
}
