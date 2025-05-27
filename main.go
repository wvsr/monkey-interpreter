package main

import (
	"fmt"
	"monkey/repl" // Import the repl package which contains the Read-Eval-Print Loop logic.
	"os"          // Import os package for standard input/output operations.
	"os/user"     // Import os/user package to get information about the current user.
)

// main is the entry point for the Monkey interpreter application.
// When the program is run, this function is executed.
func main() {
	// Attempt to get the current user's information.
	// This is used to personalize the welcome message.
	currentUser, err := user.Current()
	if err != nil {
		// Error Handling (User Information):
		// If there's an error getting the current user (e.g., on systems where user info
		// is not available or due to permissions), the program will panic.
		// This is a simple error handling strategy; in a production system,
		// a more graceful fallback or error message might be preferred.
		panic(err)
	}

	// User Information and Welcome Message:
	// Print a personalized welcome message to the console, including the current user's username.
	fmt.Printf("Hello %s! This is the Monkey programming language!\n",
		currentUser.Username)
	// Provide basic instructions to the user.
	// Note: While this message says "type in commands", it doesn't explicitly mention how to exit.
	// Standard ways to exit REPLs (like Ctrl+D for EOF, or sometimes typing "exit")
	// would depend on the repl.Start implementation's handling of scanner.Scan().
	// For this REPL, Ctrl+D (which closes os.Stdin) will cause scanner.Scan() to return false,
	// thus terminating the REPL loop.
	fmt.Printf("Feel free to type in commands. (Ctrl+D to exit)\n")

	// Starting the REPL:
	// Call the `repl.Start` function to begin the Read-Eval-Print Loop.
	// - `os.Stdin`: This is passed as the input stream, meaning the REPL will read commands
	//   typed by the user into their standard input (usually the keyboard).
	// - `os.Stdout`: This is passed as the output stream, meaning the REPL's prompts,
	//   evaluation results, and error messages will be written to the user's standard output
	//   (usually the console).
	// This effectively connects the REPL to the user's console.
	repl.Start(os.Stdin, os.Stdout)
}
