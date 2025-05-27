package object

import (
	"bytes"
	"fmt"
	"hash/fnv"
	"monkey/ast"
	"strings"
)

// BuiltinFunction defines the signature for built-in functions provided by the Monkey language.
// These functions are implemented in Go and can take a variable number of Object arguments
// and must return an Object.
type BuiltinFunction func(args ...Object) Object

// ObjectType is a string type used to identify the type of an object at runtime.
// Using a string allows for easy debugging and clear representation of types.
// Examples: "INTEGER", "BOOLEAN", "FUNCTION_OBJ".
type ObjectType string

// Constants for all available object types in Monkey.
const (
	// NULL_OBJ represents the type of the Null object, signifying absence of value.
	NULL_OBJ = "NULL"
	// ERROR_OBJ represents the type of an Error object, used for runtime errors.
	ERROR_OBJ = "ERROR"

	// INTEGER_OBJ represents the type of an Integer object.
	INTEGER_OBJ = "INTEGER"
	// BOOLEAN_OBJ represents the type of a Boolean object.
	BOOLEAN_OBJ = "BOOLEAN"
	// STRING_OBJ represents the type of a String object.
	STRING_OBJ = "STRING"

	// RETURN_VALUE_OBJ represents the type of a ReturnValue object, a wrapper for return statements.
	RETURN_VALUE_OBJ = "RETURN_VALUE"

	// FUNCTION_OBJ represents the type of a user-defined Function object.
	FUNCTION_OBJ = "FUNCTION"
	// BUILTIN_OBJ represents the type of a Builtin function object.
	BUILTIN_OBJ = "BUILTIN"

	// ARRAY_OBJ represents the type of an Array object.
	ARRAY_OBJ = "ARRAY"
	// HASH_OBJ represents the type of a Hash (map/dictionary) object.
	HASH_OBJ = "HASH"
)

// HashKey is used as a key in the internal Go map that backs Monkey's Hash object.
// Since Monkey objects themselves (like *Integer or *String) can't be directly used as map keys
// if they are pointers or contain non-comparable fields, HashKey provides a comparable representation.
// It consists of the object's Type (to differentiate, e.g., integer 1 from string "1") and a Value (a hash).
type HashKey struct {
	Type  ObjectType // The type of the object being hashed (e.g., INTEGER_OBJ, STRING_OBJ).
	Value uint64     // The 64-bit hash value of the object's content.
}

// Hashable is an interface that objects must implement if they are to be used as keys in a Hash object.
// Only Integer, Boolean, and String types currently implement this interface in Monkey.
type Hashable interface {
	HashKey() HashKey // Method to generate a HashKey for the object.
}

// Object is the fundamental interface that every value in Monkey implements.
// All runtime data structures (integers, booleans, functions, etc.) are Objects.
type Object interface {
	// Type returns the ObjectType of the object, allowing for type checking and dispatch.
	Type() ObjectType
	// Inspect returns a string representation of the object's value.
	// This is primarily used for debugging, testing, and displaying output in the REPL.
	Inspect() string
}

// Integer represents an integer value in Monkey.
type Integer struct {
	Value int64 // Value holds the actual 64-bit integer.
}

// Type returns INTEGER_OBJ, identifying this as an Integer object.
func (i *Integer) Type() ObjectType { return INTEGER_OBJ }

// Inspect returns the string representation of the integer's value.
func (i *Integer) Inspect() string { return fmt.Sprintf("%d", i.Value) }

// HashKey generates a HashKey for an Integer object.
// The Type is INTEGER_OBJ, and the Value is the integer itself cast to uint64.
// This makes Integer objects usable as keys in Monkey Hashes.
func (i *Integer) HashKey() HashKey {
	return HashKey{Type: i.Type(), Value: uint64(i.Value)}
}

// Boolean represents a boolean value (true or false) in Monkey.
type Boolean struct {
	Value bool // Value holds the actual boolean value.
}

// Type returns BOOLEAN_OBJ, identifying this as a Boolean object.
func (b *Boolean) Type() ObjectType { return BOOLEAN_OBJ }

// Inspect returns "true" or "false" as the string representation.
func (b *Boolean) Inspect() string { return fmt.Sprintf("%t", b.Value) }

// HashKey generates a HashKey for a Boolean object.
// The Type is BOOLEAN_OBJ. The Value is 1 for true and 0 for false.
// This allows Boolean objects to be used as keys in Monkey Hashes.
func (b *Boolean) HashKey() HashKey {
	var value uint64

	if b.Value {
		value = 1 // Hash value for true.
	} else {
		value = 0 // Hash value for false.
	}
	return HashKey{Type: b.Type(), Value: value}
}

// Null represents the concept of 'null' or no value in Monkey.
// It's a singleton type; there's effectively only one null value.
type Null struct{}

// Type returns NULL_OBJ, identifying this as a Null object.
func (n *Null) Type() ObjectType { return NULL_OBJ }

// Inspect returns the string "null".
func (n *Null) Inspect() string { return "null" }

// ReturnValue is a wrapper object used to signal that a return statement has been executed.
// During evaluation, when a return statement is encountered, the expression being returned
// is wrapped in a ReturnValue object. This wrapper then propagates up the call stack
// until it's caught by the function call evaluation logic, which then unwraps the actual value.
// This mechanism is necessary because simply returning the value itself wouldn't distinguish
// it from any other expression's value within the function body. The wrapper acts as a signal.
type ReturnValue struct {
	Value Object // Value is the actual object being returned by a function.
}

// Type returns RETURN_VALUE_OBJ.
func (rv *ReturnValue) Type() ObjectType { return RETURN_VALUE_OBJ }

// Inspect returns the string representation of the wrapped Value.
// This means when a ReturnValue is printed (e.g. in tests), you see the underlying value.
func (rv *ReturnValue) Inspect() string { return rv.Value.Inspect() }

// Error represents a runtime error encountered during program execution.
// It contains a message describing the error.
type Error struct {
	Message string // Message stores the description of the error.
}

// Type returns ERROR_OBJ.
func (e *Error) Type() ObjectType { return ERROR_OBJ }

// Inspect returns a string formatted as "ERROR: <Message>".
func (e *Error) Inspect() string { return "ERROR: " + e.Message }

// Function represents a user-defined function (a closure) in Monkey.
// It encapsulates the function's parameters, its body, and the environment
// in which it was defined.
type Function struct {
	Parameters []*ast.Identifier   // Parameters is a slice of AST Identifiers representing the function's formal parameters.
	Body       *ast.BlockStatement // Body is the AST BlockStatement that constitutes the function's code.
	// Env is a pointer to the Environment in which the function was created.
	// This is the crucial component that enables closures and lexical scoping.
	// When the function is called, a new environment is created that encloses this `Env`.
	// This means the function "remembers" the environment where it was defined and can
	// access variables from that scope, even if it's called from a different scope.
	// This is lexical scoping: variables are resolved based on where the function is defined in the source code,
	// not where it is called.
	Env *Environment
}

// Type returns FUNCTION_OBJ.
func (f *Function) Type() ObjectType { return FUNCTION_OBJ }

// Inspect provides a string representation of the function, including its parameters and body.
// Example: "fn(x, y) { (x + y) }"
func (f *Function) Inspect() string {
	var out bytes.Buffer

	params := []string{}
	for _, p := range f.Parameters {
		params = append(params, p.String())
	}

	out.WriteString("fn")
	out.WriteString("(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(") {\n")
	out.WriteString(f.Body.String()) // Assumes Body.String() provides a readable form of the statements.
	out.WriteString("\n}")

	return out.String()
}

// String represents a string value in Monkey.
type String struct {
	Value string // Value holds the actual Go string.
}

// Type returns STRING_OBJ.
func (s *String) Type() ObjectType { return STRING_OBJ }

// Inspect returns the raw string value.
func (s *String) Inspect() string { return s.Value }

// HashKey generates a HashKey for a String object.
// The Type is STRING_OBJ. The Value is generated by hashing the string's content
// using the FNV-1a algorithm. This makes String objects usable as keys in Monkey Hashes.
// Only immutable types that can be reliably hashed should be hashable.
func (s *String) HashKey() HashKey {
	h := fnv.New64a()    // Create a new FNV-1a 64-bit hash object.
	h.Write([]byte(s.Value)) // Write the string's bytes to the hasher.
	return HashKey{Type: s.Type(), Value: h.Sum64()} // Return the hash key.
}

// Builtin represents a built-in function available in Monkey (e.g., `len`, `puts`).
// These are functions implemented in Go that are exposed to Monkey programs.
type Builtin struct {
	// Fn is the actual Go function that implements the built-in's behavior.
	// It conforms to the BuiltinFunction signature: `func(args ...Object) Object`.
	Fn BuiltinFunction
}

// Type returns BUILTIN_OBJ.
func (b *Builtin) Type() ObjectType { return BUILTIN_OBJ }

// Inspect returns "builtin function", a generic representation for built-ins.
func (b *Builtin) Inspect() string { return "builtin function" }

// Array represents an array data structure in Monkey.
// It holds an ordered list of Objects.
type Array struct {
	Elements []Object // Elements is a slice of Monkey Objects contained in the array.
}

// Type returns ARRAY_OBJ.
func (ao *Array) Type() ObjectType { return ARRAY_OBJ }

// Inspect returns a string representation of the array, like "[element1, element2, ...]".
// It calls Inspect() on each element to get its string form.
func (ao *Array) Inspect() string {
	var out bytes.Buffer

	elements := []string{}
	for _, e := range ao.Elements {
		elements = append(elements, e.Inspect())
	}

	out.WriteString("[")
	out.WriteString(strings.Join(elements, ", "))
	out.WriteString("]")

	return out.String()
}

// HashPair represents a single key-value pair within a Monkey Hash object.
// Both Key and Value are Monkey Objects.
type HashPair struct {
	Key   Object // The key of the pair. Must be a Hashable object (Integer, Boolean, or String).
	Value Object // The value associated with the key.
}

// Hash represents a hash map (dictionary or associative array) data structure in Monkey.
// It stores key-value pairs. Keys must be hashable types (Integer, Boolean, String).
type Hash struct {
	// Pairs maps a HashKey (derived from a Monkey Object) to a HashPair.
	// Using HashKey here allows the Go map to function correctly, as direct Monkey Objects
	// might not be comparable or could have mutable pointer-based identities.
	Pairs map[HashKey]HashPair
}

// Type returns HASH_OBJ.
func (h *Hash) Type() ObjectType { return HASH_OBJ }

// Inspect returns a string representation of the hash, like "{key1: value1, key2: value2, ...}".
// It iterates over the stored HashPairs and calls Inspect() on their keys and values.
func (h *Hash) Inspect() string {
	var out bytes.Buffer

	pairs := []string{}
	// Note: Iteration order over maps in Go is not guaranteed.
	for _, pair := range h.Pairs {
		pairs = append(pairs, fmt.Sprintf("%s: %s",
			pair.Key.Inspect(), pair.Value.Inspect()))
	}

	out.WriteString("{")
	out.WriteString(strings.Join(pairs, ", "))
	out.WriteString("}")

	return out.String()
}
