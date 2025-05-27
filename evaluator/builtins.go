package evaluator

import (
	"fmt"
	"monkey/object"
)

// builtins is a map that stores all built-in functions available in Monkey.
// The keys are the string names used to call these functions from Monkey code (e.g., "len", "puts").
// The values are pointers to `object.Builtin` structs, where the `Fn` field contains
// the actual Go function that implements the built-in's behavior.
// These functions are registered and can be looked up by the evaluator when an identifier
// is not found in the current environment.
var builtins = map[string]*object.Builtin{
	// "len" built-in function:
	// - Purpose: Returns the length of its argument.
	// - Expected argument types: String or Array.
	// - Implementation:
	//   - Checks for exactly one argument.
	//   - If the argument is a String, returns an Integer object with the string's length.
	//   - If the argument is an Array, returns an Integer object with the number of elements.
	//   - If the argument type is unsupported or argument count is wrong, returns an Error object.
	"len": &object.Builtin{Fn: func(args ...object.Object) object.Object {
		// Check for the correct number of arguments (exactly one).
		if len(args) != 1 {
			return newError("wrong number of arguments. got=%d, want=1",
				len(args))
		}

		// Type switch to handle different argument types.
		switch arg := args[0].(type) {
		case *object.Array: // If argument is an Array.
			return &object.Integer{Value: int64(len(arg.Elements))}
		case *object.String: // If argument is a String.
			return &object.Integer{Value: int64(len(arg.Value))}
		default: // If argument type is not supported by `len`.
			return newError("argument to `len` not supported, got %s",
				args[0].Type())
		}
	},
	},
	// "puts" built-in function:
	// - Purpose: Prints the string representation of each argument to standard output, followed by a newline.
	// - Expected argument types: Any number of arguments of any type (their `Inspect()` method will be called).
	// - Implementation:
	//   - Iterates through each argument.
	//   - Calls `arg.Inspect()` to get its string representation.
	//   - Prints the string to `fmt.Println`.
	// - Returns: `NULL` object, as `puts` is used for its side effect.
	"puts": &object.Builtin{
		Fn: func(args ...object.Object) object.Object {
			for _, arg := range args {
				fmt.Println(arg.Inspect()) // Side effect: print to stdout.
			}
			return NULL // `puts` itself doesn't return a meaningful value.
		},
	},
	// "first" built-in function:
	// - Purpose: Returns the first element of an array.
	// - Expected argument types: Exactly one Array argument.
	// - Implementation:
	//   - Checks for exactly one argument and that it's an Array.
	//   - If the array is not empty, returns its first element.
	//   - If the array is empty, returns `NULL`.
	//   - Returns an Error object for incorrect argument count or type.
	"first": &object.Builtin{
		Fn: func(args ...object.Object) object.Object {
			if len(args) != 1 {
				return newError("wrong number of arguments. got=%d, want=1",
					len(args))
			}
			if args[0].Type() != object.ARRAY_OBJ {
				return newError("argument to `first` must be ARRAY, got %s",
					args[0].Type())
			}

			arr := args[0].(*object.Array)
			if len(arr.Elements) > 0 { // Check if the array has elements.
				return arr.Elements[0] // Return the first element.
			}
			return NULL // Return NULL if the array is empty.
		},
	},
	// "last" built-in function:
	// - Purpose: Returns the last element of an array.
	// - Expected argument types: Exactly one Array argument.
	// - Implementation:
	//   - Checks for exactly one argument and that it's an Array.
	//   - If the array is not empty, returns its last element.
	//   - If the array is empty, returns `NULL`.
	//   - Returns an Error object for incorrect argument count or type.
	"last": &object.Builtin{
		Fn: func(args ...object.Object) object.Object {
			if len(args) != 1 {
				return newError("wrong number of arguments. got=%d, want=1",
					len(args))
			}
			if args[0].Type() != object.ARRAY_OBJ {
				return newError("argument to `last` must be ARRAY, got %s",
					args[0].Type())
			}

			arr := args[0].(*object.Array)
			length := len(arr.Elements)
			if length > 0 { // Check if the array has elements.
				return arr.Elements[length-1] // Return the last element.
			}
			return NULL // Return NULL if the array is empty.
		},
	},
	// "rest" built-in function:
	// - Purpose: Returns a new array containing all elements of the input array except for its first element.
	// - Expected argument types: Exactly one Array argument.
	// - Implementation:
	//   - Checks for exactly one argument and that it's an Array.
	//   - If the input array is not empty, creates a new array with the remaining elements.
	//   - If the input array is empty or has only one element (making the "rest" empty), returns `NULL` or an empty array respectively.
	//     (Current implementation returns NULL for an empty input array, and an empty array if the input has one element).
	//   - Returns an Error object for incorrect argument count or type.
	//   - Note: This function creates a *new* array; it does not modify the original.
	"rest": &object.Builtin{
		Fn: func(args ...object.Object) object.Object {
			if len(args) != 1 {
				return newError("wrong number of arguments. got=%d, want=1",
					len(args))
			}
			if args[0].Type() != object.ARRAY_OBJ {
				return newError("argument to `rest` must be ARRAY, got %s",
					args[0].Type())
			}

			arr := args[0].(*object.Array)
			length := len(arr.Elements)
			if length > 0 {
				// Create a new slice with capacity and length for the remaining elements.
				newElements := make([]object.Object, length-1, length-1)
				// Copy elements from the second element (index 1) to the end.
				copy(newElements, arr.Elements[1:length])
				return &object.Array{Elements: newElements} // Return the new array.
			}
			// If the array is empty, there is no "rest". Returning NULL is a design choice.
			// An alternative could be to return an empty array: &object.Array{Elements: []object.Object{}}
			return NULL
		},
	},
	// "push" built-in function:
	// - Purpose: Returns a new array with the given element added to the end of the input array.
	// - Expected argument types: Two arguments - an Array and an Object to add.
	// - Implementation:
	//   - Checks for exactly two arguments.
	//   - Verifies the first argument is an Array.
	//   - Creates a new array with space for one additional element.
	//   - Copies elements from the original array to the new array.
	//   - Appends the second argument (the element to push) to the new array.
	//   - Returns the new array.
	//   - Returns an Error object for incorrect argument count or type.
	//   - Note: This function creates a *new* array; it does not modify the original array.
	"push": &object.Builtin{
		Fn: func(args ...object.Object) object.Object {
			if len(args) != 2 {
				return newError("wrong number of arguments. got=%d, want=2",
					len(args))
			}
			if args[0].Type() != object.ARRAY_OBJ { // First argument must be an Array.
				return newError("argument to `push` must be ARRAY, got %s",
					args[0].Type())
			}

			arr := args[0].(*object.Array)
			elementToPush := args[1] // The second argument is the element to add.
			length := len(arr.Elements)

			// Create a new slice with capacity for one more element.
			newElements := make([]object.Object, length+1, length+1)
			copy(newElements, arr.Elements)      // Copy existing elements.
			newElements[length] = elementToPush // Add the new element at the end.

			return &object.Array{Elements: newElements} // Return the new array.
		},
	},
}
