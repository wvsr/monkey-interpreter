package object

// NewEnclosedEnvironment creates a new Environment that is "enclosed" by an existing `outer` environment.
// This is fundamental for creating nested scopes, such as when a function is called or a new block scope is entered.
// The new environment has its own local `store`, but it retains a reference (`outer`) to the environment
// it was created from.
// - `outer`: A pointer to the Environment that this new environment should inherit from.
// Returns a pointer to the newly created enclosed Environment.
func NewEnclosedEnvironment(outer *Environment) *Environment {
	env := NewEnvironment() // Create a new, empty environment for the inner scope.
	env.outer = outer      // Set its 'outer' field to the provided outer environment.
	return env
}

// NewEnvironment creates a new, top-level Environment.
// It initializes an empty `store` for variables and sets its `outer` environment to `nil`,
// indicating that it has no enclosing scope (it's a global or outermost scope).
// Returns a pointer to the newly created Environment.
func NewEnvironment() *Environment {
	s := make(map[string]Object) // Initialize the map to store identifier-object bindings.
	return &Environment{store: s, outer: nil} // `outer` is nil for a new, non-enclosed environment.
}

// Environment holds the bindings of identifiers (variable names) to their corresponding Objects.
// It supports lexical scoping through the `outer` field, which points to an enclosing environment.
// This structure allows for a hierarchy of scopes, where inner scopes can access variables
// from outer scopes, but not vice-versa (unless outer scope variables are shadowed).
type Environment struct {
	// store is a map where keys are identifier names (strings) and values are the Monkey Objects
	// bound to those names within the current scope.
	store map[string]Object
	// outer points to the environment of the enclosing scope. If `outer` is `nil`,
	// this environment is the outermost (global) scope.
	// When looking up a variable, if it's not found in the current `store`, the lookup
	// continues in the `outer` environment, creating a chain of environments. This chain
	// is how lexical scoping is implemented: variables are resolved based on where they are
	// defined in the nested structure of the code.
	outer *Environment
}

// Get attempts to retrieve an Object bound to a given `name` (identifier) from the environment.
// It follows these steps:
// 1. It first checks the current environment's `store` for the `name`.
// 2. If the `name` is found in the current `store`, the corresponding Object and `true` are returned.
// 3. If the `name` is not found in the current `store` AND this environment has an `outer` environment
//    (i.e., `e.outer != nil`), the `Get` method is recursively called on the `outer` environment.
//    This process continues up the chain of environments until the name is found or the outermost
//    environment (where `outer` is `nil`) is reached.
// 4. If the `name` is not found in any environment in the chain, `nil` and `false` are returned.
// This mechanism directly implements lexical scoping for variable lookup.
// - `name`: The string identifier of the variable to look up.
// Returns the Object if found, and a boolean indicating success (true if found, false otherwise).
func (e *Environment) Get(name string) (Object, bool) {
	obj, ok := e.store[name] // Try to find the object in the current environment's store.
	if !ok && e.outer != nil { // If not found and an outer environment exists,
		obj, ok = e.outer.Get(name) // recursively search in the outer environment.
	}
	return obj, ok
}

// Set binds a given `name` (identifier) to a specific `val` (Object) in the current environment's `store`.
// Importantly, `Set` *always* assigns the variable in the *current* environment. It does not
// search the `outer` environments to see if the variable already exists there. If a variable
// with the same name exists in an outer scope, `Set` in an inner scope will "shadow" the outer
// variable, meaning the inner scope will use its own version.
// This behavior is characteristic of how `let` statements work: they always declare or assign
// variables in the scope where the `let` statement appears.
// - `name`: The string identifier of the variable to set.
// - `val`: The Object to be bound to the name.
// Returns the `val` that was set.
func (e *Environment) Set(name string, val Object) Object {
	e.store[name] = val // Assign the value to the name in the current environment's store.
	return val
}
