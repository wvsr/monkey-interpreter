package evaluator

import (
	"fmt"
	"monkey/ast"
	"monkey/object"
)

// Global pre-instantiated objects for NULL, TRUE, and FALSE.
// These are used for convenience and efficiency, avoiding re-creation of these
// common singleton objects during evaluation.
var (
	// NULL represents the null value in Monkey.
	NULL = &object.Null{}
	// TRUE represents the boolean true value in Monkey.
	TRUE = &object.Boolean{Value: true}
	// FALSE represents the boolean false value in Monkey.
	FALSE = &object.Boolean{Value: false}
)

// Eval is the heart of the Monkey interpreter. It recursively traverses an AST node
// and evaluates it within a given environment, producing a Monkey object.
//   - node: The ast.Node to evaluate. This can be any type of statement or expression.
//   - env: The *object.Environment in which the evaluation occurs. This environment
//     holds variable bindings and provides context for lexical scoping.
//
// Returns an object.Object representing the result of the evaluation.
func Eval(node ast.Node, env *object.Environment) object.Object {
	// The main switch statement dispatches based on the concrete type of the ast.Node.
	// This is the core of the tree-walking interpreter.
	switch node := node.(type) {

	// Statements
	// An *ast.Program node represents the entire program.
	// Evaluation involves iterating through its statements and evaluating them sequentially.
	case *ast.Program:
		return evalProgram(node, env)

	// An *ast.BlockStatement node represents a sequence of statements, typically
	// found as the body of a function or an if-expression's consequence/alternative.
	case *ast.BlockStatement:
		return evalBlockStatement(node, env)

	// An *ast.ExpressionStatement consists of a single expression.
	// The result of the expression is evaluated, but typically discarded unless it's
	// the last statement in a program or block being implicitly returned.
	case *ast.ExpressionStatement:
		return Eval(node.Expression, env) // Recursively evaluate the underlying expression.

	// An *ast.ReturnStatement evaluates its return value, wraps it in an
	// object.ReturnValue, and returns this wrapper. The wrapper acts as a signal
	// to stop further evaluation in the current (and potentially outer) execution
	// contexts (like function bodies or block statements) until it's unwrapped,
	// typically by the function call evaluation logic.
	case *ast.ReturnStatement:
		val := Eval(node.ReturnValue, env) // Evaluate the expression to be returned.
		if isError(val) {                  // Propagate errors immediately.
			return val
		}
		return &object.ReturnValue{Value: val} // Wrap the value.

	// An *ast.LetStatement evaluates the expression for the variable's value,
	// and then binds this value to the variable's name in the current environment.
	case *ast.LetStatement:
		val := Eval(node.Value, env) // Evaluate the expression on the right-hand side.
		if isError(val) {            // Propagate errors.
			return val
		}
		env.Set(node.Name.Value, val) // Store the value in the environment under the variable's name.
		// Let statements themselves don't produce a value in Monkey, so implicitly returns NULL or the last evaluated value.
		// However, here we don't return anything specific as `env.Set` handles the side effect.
		// The outer loop in evalProgram/evalBlockStatement will handle what to do next.

	// Expressions
	// An *ast.IntegerLiteral directly translates to an object.Integer.
	case *ast.IntegerLiteral:
		return &object.Integer{Value: node.Value}

	// An *ast.StringLiteral directly translates to an object.String.
	case *ast.StringLiteral:
		return &object.String{Value: node.Value}

	// An *ast.Boolean (from the AST, representing `true` or `false` literal)
	// translates to one of the pre-instantiated object.Boolean objects (TRUE or FALSE).
	case *ast.Boolean:
		return nativeBoolToBooleanObject(node.Value)

	// An *ast.PrefixExpression first evaluates its right operand.
	// Then, it calls evalPrefixExpression with the operator and the evaluated operand.
	case *ast.PrefixExpression:
		right := Eval(node.Right, env) // Evaluate the operand.
		if isError(right) {            // Propagate errors.
			return right
		}
		return evalPrefixExpression(node.Operator, right) // Evaluate the prefix operation.

	// An *ast.InfixExpression evaluates its left and right operands.
	// Then, it calls evalInfixExpression with the operator and both evaluated operands.
	case *ast.InfixExpression:
		left := Eval(node.Left, env) // Evaluate the left operand.
		if isError(left) {           // Propagate errors.
			return left
		}
		right := Eval(node.Right, env) // Evaluate the right operand.
		if isError(right) {            // Propagate errors.
			return right
		}
		return evalInfixExpression(node.Operator, left, right) // Evaluate the infix operation.

	// An *ast.IfExpression is evaluated by calling evalIfExpression.
	case *ast.IfExpression:
		return evalIfExpression(node, env)

	// An *ast.Identifier is evaluated by looking up its value in the environment
	// using evalIdentifier. This also handles built-in functions.
	case *ast.Identifier:
		return evalIdentifier(node, env)

	// An *ast.FunctionLiteral results in the creation of an object.Function.
	// Crucially, the current environment `env` is captured within the object.Function.
	// This captured environment is what makes closures work: the function "remembers"
	// the lexical scope in which it was defined.
	case *ast.FunctionLiteral:
		params := node.Parameters
		body := node.Body
		// The current `env` becomes the function's closure environment.
		return &object.Function{Parameters: params, Env: env, Body: body}

	// An *ast.CallExpression evaluates the expression that represents the function,
	// then evaluates all argument expressions. Finally, it calls `applyFunction`
	// to execute the function (either user-defined or built-in).
	case *ast.CallExpression:
		// Evaluate the expression that should result in a function object (user-defined or built-in).
		function := Eval(node.Function, env)
		if isError(function) { // If evaluating the function identifier/literal results in an error.
			return function
		}

		// Evaluate all argument expressions.
		args := evalExpressions(node.Arguments, env)
		// If any argument evaluation results in an error, `evalExpressions` returns a slice
		// containing just that error object.
		if len(args) == 1 && isError(args[0]) {
			return args[0]
		}

		// Apply the function to the evaluated arguments.
		return applyFunction(function, args)

	// An *ast.ArrayLiteral evaluates all its element expressions and then
	// creates an object.Array containing these evaluated objects.
	case *ast.ArrayLiteral:
		elements := evalExpressions(node.Elements, env)
		// If an error occurred during element evaluation, propagate it.
		if len(elements) == 1 && isError(elements[0]) {
			return elements[0]
		}
		return &object.Array{Elements: elements}

	// An *ast.IndexExpression evaluates the object being indexed (the "left") and
	// the index expression. Then, it calls `evalIndexExpression` to perform the lookup.
	case *ast.IndexExpression:
		left := Eval(node.Left, env) // Evaluate the array or hash object.
		if isError(left) {
			return left
		}
		index := Eval(node.Index, env) // Evaluate the index expression.
		if isError(index) {
			return index
		}
		return evalIndexExpression(left, index) // Perform the index operation.

	// An *ast.HashLiteral is evaluated by calling evalHashLiteral.
	case *ast.HashLiteral:
		return evalHashLiteral(node, env)
	}

	// If the node type is not recognized, return nil. This might indicate
	// an unimplemented AST node type or an issue in the parser/AST structure.
	return nil
}

// evalProgram evaluates the statements in a Program node sequentially.
// If a ReturnValue or Error object is encountered during the evaluation of any statement,
// the evaluation of the program stops immediately, and that object is returned.
// This ensures that `return` statements halt execution of the current function/script,
// and errors propagate up without further processing.
func evalProgram(program *ast.Program, env *object.Environment) object.Object {
	var result object.Object

	for _, statement := range program.Statements {
		result = Eval(statement, env) // Evaluate each statement.

		// Check the type of the result from the statement.
		switch result := result.(type) {
		case *object.ReturnValue: // If it's a return value, unwrap it and return immediately.
			return result.Value
		case *object.Error: // If it's an error, return it immediately.
			return result
		}
		// Otherwise, continue to the next statement. The `result` of the last evaluated
		// statement will be the implicit return value of the program if no explicit return occurs.
	}
	return result
}

// evalBlockStatement evaluates the statements within a BlockStatement sequentially.
// Similar to evalProgram, if a ReturnValue or Error object is encountered,
// evaluation of the block stops, and that object is returned directly (without unwrapping
// the ReturnValue here; that's the responsibility of the function call evaluator).
func evalBlockStatement(
	block *ast.BlockStatement,
	env *object.Environment,
) object.Object {
	var result object.Object

	for _, statement := range block.Statements {
		result = Eval(statement, env)

		// If a result is obtained (not nil, though Eval should always return something or an error),
		// check its type.
		if result != nil {
			rt := result.Type()
			// If it's a ReturnValue or an Error, it must be propagated upwards immediately.
			// This stops execution of subsequent statements in the block.
			if rt == object.RETURN_VALUE_OBJ || rt == object.ERROR_OBJ {
				return result
			}
		}
	}
	// The result of the last statement in the block is its implicit return value,
	// unless an explicit return or error occurred.
	return result
}

// nativeBoolToBooleanObject is a helper function that converts a Go native `bool`
// to one of the globally defined `object.Boolean` singletons (TRUE or FALSE).
// This avoids allocating new Boolean objects repeatedly.
func nativeBoolToBooleanObject(input bool) *object.Boolean {
	if input {
		return TRUE
	}
	return FALSE
}

// evalPrefixExpression evaluates prefix expressions like `!true` or `-10`.
// It takes the operator string and the already evaluated right-hand operand.
func evalPrefixExpression(operator string, right object.Object) object.Object {
	switch operator {
	case "!": // Bang operator for logical negation.
		return evalBangOperatorExpression(right)
	case "-": // Minus operator for numerical negation.
		return evalMinusPrefixOperatorExpression(right)
	default: // If the operator is unknown.
		return newError("unknown operator: %s%s", operator, right.Type())
	}
}

// evalInfixExpression evaluates infix expressions like `5 + 5` or `true == false`.
// It takes the operator string and the already evaluated left and right operands.
// It dispatches to more specific functions based on the types of the operands.
func evalInfixExpression(
	operator string,
	left, right object.Object,
) object.Object {
	switch {
	// Integer arithmetic and comparisons.
	case left.Type() == object.INTEGER_OBJ && right.Type() == object.INTEGER_OBJ:
		return evalIntegerInfixExpression(operator, left, right)
	// String concatenation.
	case left.Type() == object.STRING_OBJ && right.Type() == object.STRING_OBJ:
		return evalStringInfixExpression(operator, left, right)
	// Pointer comparison for booleans (TRUE and FALSE are singletons).
	// This handles `true == true`, `false == false`, `true != false`, etc.
	// For other types, `==` and `!=` might need different handling if they are not singletons
	// or if value equality is desired over reference equality.
	// In Monkey's current design, `TRUE` and `FALSE` are singletons, so pointer comparison works.
	// `NULL` is also a singleton. For integers and strings, `evalIntegerInfixExpression` handles `==` and `!=`.
	case operator == "==":
		return nativeBoolToBooleanObject(left == right) // Works for TRUE, FALSE, NULL due to singleton nature.
	case operator == "!=":
		return nativeBoolToBooleanObject(left != right) // Works for TRUE, FALSE, NULL.
	// Type mismatch error if operators other than '==' or '!=' are used with different types.
	case left.Type() != right.Type():
		return newError("type mismatch: %s %s %s",
			left.Type(), operator, right.Type())
	// Default error for unknown operators between types that were not handled above.
	default:
		return newError("unknown operator: %s %s %s",
			left.Type(), operator, right.Type())
	}
}

// evalBangOperatorExpression handles the logical NOT (`!`) operation.
// - `!true` is `false`.
// - `!false` is `true`.
// - `!null` is `true` (null is considered falsy).
// - `! <any other object (e.g. integer 0, 5, string "">` is `false` (all other objects are truthy).
func evalBangOperatorExpression(right object.Object) object.Object {
	switch right {
	case TRUE:
		return FALSE
	case FALSE:
		return TRUE
	case NULL: // In Monkey, null is falsy, so !null is true.
		return TRUE
	default: // All other objects (integers, strings, functions, arrays, hashes) are truthy.
		return FALSE
	}
}

// evalMinusPrefixOperatorExpression handles the numerical negation (`-`) operation.
// It expects an Integer operand.
func evalMinusPrefixOperatorExpression(right object.Object) object.Object {
	// Check if the operand is an integer.
	if right.Type() != object.INTEGER_OBJ {
		return newError("unknown operator: -%s", right.Type())
	}
	// Perform the negation.
	value := right.(*object.Integer).Value
	return &object.Integer{Value: -value}
}

// evalIntegerInfixExpression handles infix operations between two Integer objects.
// Supports arithmetic (+, -, *, /) and comparison (<, >, ==, !=) operators.
func evalIntegerInfixExpression(
	operator string,
	left, right object.Object,
) object.Object {
	leftVal := left.(*object.Integer).Value
	rightVal := right.(*object.Integer).Value

	switch operator {
	case "+":
		return &object.Integer{Value: leftVal + rightVal}
	case "-":
		return &object.Integer{Value: leftVal - rightVal}
	case "*":
		return &object.Integer{Value: leftVal * rightVal}
	case "/":
		// Note: Division by zero is not explicitly handled here, will panic if rightVal is 0.
		// A production interpreter would add a check for division by zero.
		return &object.Integer{Value: leftVal / rightVal}
	case "<":
		return nativeBoolToBooleanObject(leftVal < rightVal)
	case ">":
		return nativeBoolToBooleanObject(leftVal > rightVal)
	case "==":
		return nativeBoolToBooleanObject(leftVal == rightVal)
	case "!=":
		return nativeBoolToBooleanObject(leftVal != rightVal)
	default: // If the operator is not supported for integers.
		return newError("unknown operator: %s %s %s",
			left.Type(), operator, right.Type())
	}
}

// evalStringInfixExpression handles infix operations between two String objects.
// Currently, only concatenation (`+`) is supported.
func evalStringInfixExpression(
	operator string,
	left, right object.Object,
) object.Object {
	// Only '+' is supported for strings.
	if operator != "+" {
		return newError("unknown operator: %s %s %s",
			left.Type(), operator, right.Type())
	}
	// Perform string concatenation.
	leftVal := left.(*object.String).Value
	rightVal := right.(*object.String).Value
	return &object.String{Value: leftVal + rightVal}
}

// evalIfExpression evaluates an if-expression.
// 1. It evaluates the `Condition`.
// 2. If the condition is truthy (see `isTruthy`), it evaluates the `Consequence` block.
// 3. Otherwise, if an `Alternative` block exists, it evaluates the `Alternative` block.
// 4. If the condition is falsy and no `Alternative` exists, it returns `NULL`.
func evalIfExpression(
	ie *ast.IfExpression,
	env *object.Environment,
) object.Object {
	condition := Eval(ie.Condition, env) // Evaluate the condition.
	if isError(condition) {              // Propagate errors from condition evaluation.
		return condition
	}

	if isTruthy(condition) { // Check if the condition is truthy.
		return Eval(ie.Consequence, env) // Evaluate the consequence if truthy.
	} else if ie.Alternative != nil { // Otherwise, if an alternative block exists,
		return Eval(ie.Alternative, env) // evaluate it.
	} else { // If condition is falsy and no alternative, the if-expression evaluates to NULL.
		return NULL
	}
}

// evalIdentifier evaluates an identifier (variable name).
// 1. It first attempts to retrieve the value from the current `env` (and its outer scopes).
// 2. If not found in the environment, it checks if the identifier matches a built-in function name.
// 3. If still not found, it returns an "identifier not found" error.
func evalIdentifier(
	node *ast.Identifier,
	env *object.Environment,
) object.Object {
	// Look up in the current environment chain.
	if val, ok := env.Get(node.Value); ok {
		return val
	}
	// If not in environment, check if it's a built-in function.
	if builtin, ok := builtins[node.Value]; ok {
		return builtin // Return the built-in function object.
	}
	// If not found anywhere, it's an error.
	return newError("%s", "identifier not found: "+node.Value)
}

// isTruthy determines the truthiness of a Monkey object.
//   - `NULL` is falsy.
//   - `FALSE` (the boolean object) is falsy.
//   - `TRUE` (the boolean object) is truthy.
//   - All other objects (integers including 0, strings including "", functions, arrays, hashes)
//     are considered truthy in Monkey's current definition.
func isTruthy(obj object.Object) bool {
	switch obj {
	case NULL:
		return false
	case TRUE:
		return true
	case FALSE:
		return false
	default: // Any other object is truthy.
		return true
	}
}

// newError is a helper function to create a new `object.Error` with a formatted message.
// This centralizes error object creation.
func newError(format string, a ...interface{}) *object.Error {
	return &object.Error{Message: fmt.Sprintf(format, a...)}
}

// isError is a helper function to check if a given `object.Object` is an `object.Error`.
// It safely handles `nil` objects (which are not errors).
func isError(obj object.Object) bool {
	if obj != nil {
		return obj.Type() == object.ERROR_OBJ
	}
	return false
}

// evalExpressions evaluates a slice of AST expressions (`exps`) within a given environment (`env`).
// It returns a slice of evaluated Monkey objects.
// If any expression evaluation results in an error, `evalExpressions` immediately returns
// a slice containing only that error object. This ensures errors halt further argument/element evaluation.
func evalExpressions(
	exps []ast.Expression,
	env *object.Environment,
) []object.Object {
	var result []object.Object

	for _, e := range exps {
		evaluated := Eval(e, env) // Evaluate each expression.
		if isError(evaluated) {   // If an error occurs,
			// return a slice containing only the error, signaling to the caller.
			return []object.Object{evaluated}
		}
		result = append(result, evaluated) // Add successful evaluation to the result.
	}

	return result // Return all evaluated objects.
}

// applyFunction handles the invocation of both user-defined functions (`object.Function`)
// and built-in functions (`object.Builtin`).
// - `fn`: The function object to apply (must be either *object.Function or *object.Builtin).
// - `args`: A slice of already evaluated argument objects.
func applyFunction(fn object.Object, args []object.Object) object.Object {
	switch fn := fn.(type) {
	// Case 1: User-defined function.
	case *object.Function:
		// Create a new, enclosed environment for the function call.
		// This environment is enclosed by the function's definition environment (fn.Env),
		// which is the essence of lexical scoping (closures).
		extendedEnv := extendFunctionEnv(fn, args)
		// Evaluate the function's body within this new, extended environment.
		evaluated := Eval(fn.Body, extendedEnv)
		// If the evaluation of the body resulted in an object.ReturnValue (from a `return` statement),
		// unwrap it to get the actual value to be returned from the function call.
		// If it was not a ReturnValue (e.g. implicit return of the last statement's value, or an error),
		// `unwrapReturnValue` will return it as is.
		return unwrapReturnValue(evaluated)

	// Case 2: Built-in function.
	case *object.Builtin:
		// Directly call the Go function (fn.Fn) associated with the built-in,
		// passing the evaluated arguments.
		return fn.Fn(args...)

	// Case 3: Not a function.
	// If the object `fn` is not a Function or a Builtin, it's an error.
	default:
		return newError("not a function: %s", fn.Type())
	}
}

// extendFunctionEnv creates a new, enclosed environment specifically for a function call.
// This new environment is where the function's parameters will be bound to the argument values.
//   - `fn`: The `object.Function` being called. Its `Env` field (the environment where it was defined)
//     will become the outer environment for this new scope.
//   - `args`: The slice of evaluated argument objects passed to the function.
//
// The function's parameters (from `fn.Parameters`) are bound to the corresponding `args`
// in the new environment's local store.
func extendFunctionEnv(
	fn *object.Function,
	args []object.Object,
) *object.Environment {
	// Create a new environment that is enclosed by the function's definition environment (fn.Env).
	// This establishes the lexical scope for the function body.
	env := object.NewEnclosedEnvironment(fn.Env)

	// Iterate over the function's formal parameters and bind them to the provided arguments.
	// It's assumed that `len(args)` matches `len(fn.Parameters)`.
	// Argument count checking should ideally happen before calling `extendFunctionEnv`.
	for paramIdx, param := range fn.Parameters {
		env.Set(param.Value, args[paramIdx]) // param.Value is the string name of the parameter.
	}

	return env // Return the newly created and populated environment.
}

// unwrapReturnValue checks if an object is an `object.ReturnValue` wrapper.
// If it is, it extracts and returns the actual `Value` contained within.
// Otherwise, it returns the object as is. This is used after evaluating a function body
// to get the true return value if an explicit `return` statement was used.
func unwrapReturnValue(obj object.Object) object.Object {
	if returnValue, ok := obj.(*object.ReturnValue); ok {
		return returnValue.Value // Extract the wrapped value.
	}
	return obj // Not a ReturnValue, return the object itself.
}

// evalIndexExpression handles evaluation of index expressions (e.g., `array[0]`, `hash["key"]`).
// It dispatches to specific handlers based on the type of the object being indexed (`left`).
func evalIndexExpression(left, index object.Object) object.Object {
	switch {
	// If indexing an Array with an Integer.
	case left.Type() == object.ARRAY_OBJ && index.Type() == object.INTEGER_OBJ:
		return evalArrayIndexExpression(left, index)
	// If indexing a Hash. The index type will be checked by evalHashIndexExpression.
	case left.Type() == object.HASH_OBJ:
		return evalHashIndexExpression(left, index)
	// Any other combination is an unsupported index operation.
	default:
		return newError("index operator not supported: %s", left.Type())
	}
}

// evalArrayIndexExpression handles indexing into an Array object.
// - `array`: The `object.Array` being indexed.
// - `index`: The `object.Integer` representing the index.
// It performs bounds checking: if the index is out of bounds, it returns `NULL`.
// Otherwise, it returns the element at the specified index.
func evalArrayIndexExpression(array, index object.Object) object.Object {
	arrayObject := array.(*object.Array)
	idx := index.(*object.Integer).Value
	max := int64(len(arrayObject.Elements) - 1)

	// Check for out-of-bounds access.
	if idx < 0 || idx > max {
		return NULL // Return NULL for out-of-bounds, as per common dynamic language behavior.
	}

	return arrayObject.Elements[idx] // Return the element.
}

// evalHashLiteral evaluates an AST HashLiteral node to create an `object.Hash`.
// It iterates through the key-value pairs in the AST:
//  1. Evaluates the AST key expression.
//  2. Checks if the evaluated key is `Hashable`. If not, returns an error.
//  3. Evaluates the AST value expression.
//  4. If any evaluation causes an error, it's propagated.
//  5. Stores the evaluated key (via its `HashKey()`) and the evaluated value as an `object.HashPair`
//     in the `object.Hash`'s internal map.
func evalHashLiteral(
	node *ast.HashLiteral,
	env *object.Environment,
) object.Object {
	pairs := make(map[object.HashKey]object.HashPair)

	// Iterate over the key-value pairs from the AST.
	for keyNode, valueNode := range node.Pairs {
		key := Eval(keyNode, env) // Evaluate the key expression.
		if isError(key) {
			return key
		}

		// Check if the evaluated key implements the Hashable interface.
		hashKey, ok := key.(object.Hashable)
		if !ok {
			return newError("unusable as hash key: %s", key.Type())
		}

		value := Eval(valueNode, env) // Evaluate the value expression.
		if isError(value) {
			return value
		}

		hashed := hashKey.HashKey()                             // Get the HashKey from the hashable key object.
		pairs[hashed] = object.HashPair{Key: key, Value: value} // Store the pair.
	}

	return &object.Hash{Pairs: pairs} // Return the new Hash object.
}

// evalHashIndexExpression handles indexing into a Hash object.
// - `hash`: The `object.Hash` being indexed.
// - `index`: The key used for lookup; must be a `Hashable` type (Integer, Boolean, or String).
// If the `index` object is not hashable, an error is returned.
// If the key is not found in the hash, `NULL` is returned.
// Otherwise, the value associated with the key is returned.
func evalHashIndexExpression(hash, index object.Object) object.Object {
	hashObject := hash.(*object.Hash)

	// The index (key for the hash) must be a hashable type.
	key, ok := index.(object.Hashable)
	if !ok {
		return newError("unusable as hash key: %s", index.Type())
	}

	// Look up the HashPair in the hash's internal map using the key's HashKey.
	pair, ok := hashObject.Pairs[key.HashKey()]
	if !ok {
		return NULL // Key not found, return NULL.
	}

	return pair.Value // Key found, return the associated value.
}
