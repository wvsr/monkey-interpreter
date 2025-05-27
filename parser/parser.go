package parser

import (
	"fmt"
	"monkey/ast"
	"monkey/lexer"
	"monkey/token"
	"strconv"
)

// Precedence constants define the order of operations for parsing expressions.
// Higher values indicate higher precedence.
const (
	_ int = iota // Assigns 0 to _, then increments for subsequent constants.
	// LOWEST is the lowest precedence, used for expressions not handled by other precedences.
	LOWEST
	// EQUALS represents the precedence of equality operators (==, !=).
	EQUALS
	// LESSGREATER represents the precedence of comparison operators (<, >).
	LESSGREATER
	// SUM represents the precedence of addition (+) and subtraction (-).
	SUM
	// PRODUCT represents the precedence of multiplication (*) and division (/).
	PRODUCT
	// PREFIX represents the precedence of prefix operators (e.g., -X, !X).
	PREFIX
	// CALL represents the precedence of function call expressions (e.g., myFunction(X)).
	CALL
	// INDEX represents the precedence of array indexing operations (e.g., array[index]).
	INDEX
)

// precedences maps token types to their corresponding precedence levels.
// This map is crucial for the Pratt parsing algorithm to correctly handle operator precedence.
var precedences = map[token.TokenType]int{
	token.EQ:       EQUALS,      // '==' has EQUALS precedence.
	token.NOT_EQ:   EQUALS,      // '!=' has EQUALS precedence.
	token.LT:       LESSGREATER, // '<' has LESSGREATER precedence.
	token.GT:       LESSGREATER, // '>' has LESSGREATER precedence.
	token.PLUS:     SUM,         // '+' has SUM precedence.
	token.MINUS:    SUM,         // '-' has SUM precedence.
	token.SLASH:    PRODUCT,     // '/' has PRODUCT precedence.
	token.ASTERISK: PRODUCT,     // '*' has PRODUCT precedence.
	token.LPAREN:   CALL,        // '(' (for function calls) has CALL precedence.
	token.LBRACKET: INDEX,       // '[' (for array indexing) has INDEX precedence.
}

// prefixParseFn is a function type for parsing prefix expressions.
// It takes no arguments and returns an ast.Expression.
// Example: For a token like '!', the corresponding prefixParseFn would handle '!true'.
type prefixParseFn func() ast.Expression

// infixParseFn is a function type for parsing infix expressions.
// It takes the left-hand side of the infix expression (ast.Expression) as an argument
// and returns the resulting ast.Expression.
// Example: For a token like '+', the infixParseFn would take the expression to the left of '+'
// and parse the complete addition expression like 'left + right'.
type infixParseFn func(ast.Expression) ast.Expression

// Parser struct holds the state required for parsing Monkey source code.
type Parser struct {
	l      *lexer.Lexer // Pointer to the lexer instance, providing a stream of tokens.
	errors []string     // A slice of strings to collect parsing errors encountered.

	curToken  token.Token // The current token being processed by the parser.
	peekToken token.Token // The next token after curToken, used to "look ahead".

	// prefixParseFns maps token types to functions that parse expressions where the token is in a prefix position (e.g., '!' in '!true', '-' in '-5').
	prefixParseFns map[token.TokenType]prefixParseFn
	// infixParseFns maps token types to functions that parse expressions where the token is in an infix position (e.g., '+' in '2 + 3').
	infixParseFns map[token.TokenType]infixParseFn
}

// New creates and returns a new Parser instance.
// It initializes the parser with a lexer, sets up the parsing function maps,
// and advances the tokens twice to populate both curToken and peekToken.
func New(l *lexer.Lexer) *Parser {
	p := &Parser{
		l:      l,
		errors: []string{}, // Initialize with an empty error slice.
	}

	// Initialize maps for prefix and infix parsing functions.
	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	// Register parsing functions for prefix operators and literals.
	p.registerPrefix(token.IDENT, p.parseIdentifier)         // e.g., variableName
	p.registerPrefix(token.INT, p.parseIntegerLiteral)       // e.g., 5
	p.registerPrefix(token.STRING, p.parseStringLiteral)     // e.g., "hello"
	p.registerPrefix(token.BANG, p.parsePrefixExpression)    // e.g., !true
	p.registerPrefix(token.MINUS, p.parsePrefixExpression)   // e.g., -10
	p.registerPrefix(token.TRUE, p.parseBoolean)             // true
	p.registerPrefix(token.FALSE, p.parseBoolean)            // false
	p.registerPrefix(token.LPAREN, p.parseGroupedExpression) // e.g., (5 + 5)
	p.registerPrefix(token.IF, p.parseIfExpression)          // if (condition) { ... }
	p.registerPrefix(token.FUNCTION, p.parseFunctionLiteral) // fn(x, y) { ... }
	p.registerPrefix(token.LBRACKET, p.parseArrayLiteral)    // [1, 2, 3]
	p.registerPrefix(token.LBRACE, p.parseHashLiteral)       // {"key": "value"}

	p.infixParseFns = make(map[token.TokenType]infixParseFn)
	// Register parsing functions for infix operators.
	p.registerInfix(token.PLUS, p.parseInfixExpression)     // e.g., left + right
	p.registerInfix(token.MINUS, p.parseInfixExpression)    // e.g., left - right
	p.registerInfix(token.SLASH, p.parseInfixExpression)    // e.g., left / right
	p.registerInfix(token.ASTERISK, p.parseInfixExpression) // e.g., left * right
	p.registerInfix(token.EQ, p.parseInfixExpression)       // e.g., left == right
	p.registerInfix(token.NOT_EQ, p.parseInfixExpression)   // e.g., left != right
	p.registerInfix(token.LT, p.parseInfixExpression)       // e.g., left < right
	p.registerInfix(token.GT, p.parseInfixExpression)       // e.g., left > right

	p.registerInfix(token.LPAREN, p.parseCallExpression)       // For function calls, e.g., funcName(arg1, arg2)
	p.registerInfix(token.LBRACKET, p.parseIndexExpression) // For array/hash indexing, e.g., myArray[0]

	// Read two tokens, so curToken and peekToken are both set.
	// This initialization is crucial for the parser's lookahead mechanism.
	p.nextToken()
	p.nextToken()

	return p
}

// nextToken advances the parser's tokens.
// The current peekToken becomes the new curToken.
// A new token is read from the lexer to become the new peekToken.
// This provides a one-token lookahead.
func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

// curTokenIs checks if the current token's type matches the given TokenType.
func (p *Parser) curTokenIs(t token.TokenType) bool {
	return p.curToken.Type == t
}

// peekTokenIs checks if the peek token's type matches the given TokenType.
func (p *Parser) peekTokenIs(t token.TokenType) bool {
	return p.peekToken.Type == t
}

// expectPeek checks if the peek token is of the expected type.
// If it is, it advances the tokens by calling nextToken and returns true.
// If not, it records a peekError and returns false.
// This is a common utility for ensuring the sequence of tokens matches grammar rules.
func (p *Parser) expectPeek(t token.TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	} else {
		p.peekError(t) // Record an error if the expectation is not met.
		return false
	}
}

// Errors returns the slice of error messages collected during parsing.
func (p *Parser) Errors() []string {
	return p.errors
}

// peekError adds an error message to the p.errors slice when an unexpected token
// is found in the peek position.
func (p *Parser) peekError(t token.TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead",
		t, p.peekToken.Type)
	p.errors = append(p.errors, msg)
}

// noPrefixParseFnError adds an error message when no prefix parsing function
// is found for the current token type. This usually indicates a syntax error
// where an expression was expected but something else was found.
func (p *Parser) noPrefixParseFnError(t token.TokenType) {
	msg := fmt.Sprintf("no prefix parse function for %s found", t)
	p.errors = append(p.errors, msg)
}

// ParseProgram is the main entry point for the parser.
// It creates an ast.Program node and then iteratively calls parseStatement
// to parse each statement in the input until an EOF token is encountered.
// All successfully parsed statements are appended to the Program's Statements slice.
func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{} // Create the root AST node.
	program.Statements = []ast.Statement{}

	// Loop until the End-Of-File token is reached.
	for !p.curTokenIs(token.EOF) {
		stmt := p.parseStatement() // Parse a single statement.
		if stmt != nil {           // If parsing was successful (stmt is not nil),
			program.Statements = append(program.Statements, stmt) // add it to the program.
		}
		p.nextToken() // Advance to the next token to process the next statement.
	}

	return program
}

// parseStatement determines the type of statement to parse based on the current token.
// It uses a switch statement on p.curToken.Type to delegate to more specific
// parsing functions like parseLetStatement, parseReturnStatement, or parseExpressionStatement.
func (p *Parser) parseStatement() ast.Statement {
	switch p.curToken.Type {
	case token.LET:
		return p.parseLetStatement() // Delegate to parseLetStatement if current token is LET.
	case token.RETURN:
		return p.parseReturnStatement() // Delegate to parseReturnStatement if current token is RETURN.
	default:
		// If it's not a LET or RETURN statement, it's treated as an expression statement.
		return p.parseExpressionStatement()
	}
}

// parseLetStatement parses a 'let' statement (e.g., `let x = 5;`).
// It expects the format: LET Identifier ASSIGN Expression (optional SEMICOLON).
// - Creates an ast.LetStatement node.
// - Validates the presence of token.IDENT for the variable name.
// - Validates the presence of token.ASSIGN.
// - Recursively calls parseExpression to parse the value being assigned.
// - Handles an optional semicolon at the end of the statement.
func (p *Parser) parseLetStatement() *ast.LetStatement {
	stmt := &ast.LetStatement{Token: p.curToken} // Current token is LET.

	// Expect an identifier (variable name) next.
	if !p.expectPeek(token.IDENT) {
		return nil // Error already recorded by expectPeek.
	}
	stmt.Name = &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}

	// Expect an assignment operator '=' next.
	if !p.expectPeek(token.ASSIGN) {
		return nil // Error already recorded by expectPeek.
	}

	p.nextToken() // Consume the '=' token.

	// Parse the expression on the right-hand side of the assignment.
	stmt.Value = p.parseExpression(LOWEST)

	// Check for an optional semicolon at the end of the statement.
	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken() // Consume the semicolon.
	}

	return stmt
}

// parseReturnStatement parses a 'return' statement (e.g., `return x + 5;`).
// It expects the format: RETURN Expression (optional SEMICOLON).
// - Creates an ast.ReturnStatement node.
// - Advances past the RETURN token.
// - Recursively calls parseExpression to parse the value being returned.
// - Handles an optional semicolon at the end of the statement.
func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: p.curToken} // Current token is RETURN.

	p.nextToken() // Consume the RETURN token.

	// Parse the expression to be returned.
	stmt.ReturnValue = p.parseExpression(LOWEST)

	// Check for an optional semicolon.
	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken() // Consume the semicolon.
	}

	return stmt
}

// parseExpressionStatement parses a statement that consists of a single expression
// (e.g., `x + 5;` or `myFunction();`).
// - Creates an ast.ExpressionStatement node.
// - Calls parseExpression to parse the expression itself.
// - Handles an optional semicolon at the end.
func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	// The current token is the first token of the expression.
	stmt := &ast.ExpressionStatement{Token: p.curToken}

	// Parse the expression using the Pratt parsing algorithm.
	// LOWEST precedence is passed as an argument because at the start of an expression statement,
	// any operator precedence is allowed.
	stmt.Expression = p.parseExpression(LOWEST)

	// Check for an optional semicolon. If present, it signifies the end of the statement.
	// In Monkey, expression statements can be terminated by a semicolon, but it's often optional.
	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken() // Consume the semicolon.
	}

	return stmt
}

// parseExpression is the core of the Pratt parsing algorithm (also known as Top-Down Operator Precedence parsing).
// It takes a `precedence` argument which indicates the current precedence level.
// 1. It finds a `prefixParseFn` for the current token (`p.curToken.Type`). This handles literals, identifiers, prefix operators, and grouped expressions.
//    If no prefix function is found, it's a syntax error.
// 2. It calls this `prefixParseFn` to get the `leftExp` (the left-hand side of a potential infix expression).
// 3. It then enters a loop that continues as long as the next token is not a semicolon (which would end the expression statement)
//    and the `precedence` argument is less than the precedence of the `peekToken`. This condition ensures that operators are parsed in the correct order.
//    - Inside the loop:
//        - It looks for an `infixParseFn` associated with the `peekToken.Type`.
//        - If no infix function is found (e.g., `5 (`, where `(` is not an infix operator here), it means the current `leftExp` is complete for this precedence level, so it's returned.
//        - If an infix function is found (e.g., `+` in `5 +`), it advances tokens (`p.nextToken()`) so `curToken` is now the infix operator.
//        - It calls the `infixParseFn`, passing the current `leftExp`. This function will parse the right-hand side of the infix operator and return a new `ast.Expression` that incorporates `leftExp`, the operator, and the right-hand expression. This new expression becomes the new `leftExp`.
// 4. The loop continues, potentially consuming more infix operators if their precedence is higher or equal (for left-associativity handled by the precedence argument).
func (p *Parser) parseExpression(precedence int) ast.Expression {
	// Find a parsing function for the current token in a prefix position.
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type) // Error if no prefix parser is found.
		return nil
	}
	leftExp := prefix() // Parse the prefix part (e.g., a number, identifier, or prefix op like '!')

	// Loop for handling infix operations, governed by precedence.
	// Continues as long as the next token is not a semicolon (ending the expression)
	// and the current `precedence` is less than the precedence of the `peekToken`.
	for !p.peekTokenIs(token.SEMICOLON) && precedence < p.peekPrecedence() {
		// Find an infix parsing function for the peekToken.
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			// No infix operator found that can operate on leftExp at this precedence,
			// or the operator has lower precedence.
			return leftExp
		}

		p.nextToken() // Consume the infix operator, it becomes curToken.

		// Call the infix parsing function, passing the left expression.
		// This function will parse the right side and combine it with the left.
		leftExp = infix(leftExp)
	}

	return leftExp // Return the fully parsed expression.
}

// peekPrecedence returns the precedence of the peekToken.
// If the peekToken is not in the precedences map, it returns LOWEST.
func (p *Parser) peekPrecedence() int {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}
	return LOWEST
}

// curPrecedence returns the precedence of the curToken.
// If the curToken is not in the precedences map, it returns LOWEST.
func (p *Parser) curPrecedence() int {
	if p, ok := precedences[p.curToken.Type]; ok {
		return p
	}
	return LOWEST
}

// parseIdentifier is a prefix parsing function for identifiers.
// It simply creates an ast.Identifier node with the current token and its literal value.
func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
}

// parseIntegerLiteral is a prefix parsing function for integer literals.
// It attempts to parse the token's literal value as an int64.
// If parsing fails, it records an error. Otherwise, it returns an ast.IntegerLiteral node.
func (p *Parser) parseIntegerLiteral() ast.Expression {
	lit := &ast.IntegerLiteral{Token: p.curToken}

	// Convert the string literal of the token to an int64.
	// Base 0 means strconv.ParseInt will infer the base from the prefix (e.g., 0x for hex).
	// 64 specifies that the result must fit in 64 bits.
	value, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer", p.curToken.Literal)
		p.errors = append(p.errors, msg) // Record error if parsing fails.
		return nil
	}

	lit.Value = value
	return lit
}

// parseStringLiteral is a prefix parsing function for string literals.
// It returns an ast.StringLiteral node, using the token's literal value as the string's content.
// Note: In Monkey, the token's literal for a string already contains the content without the quotes.
func (p *Parser) parseStringLiteral() ast.Expression {
	return &ast.StringLiteral{Token: p.curToken, Value: p.curToken.Literal}
}

// parsePrefixExpression is a prefix parsing function for prefix operators like '!' or '-'.
// It creates an ast.PrefixExpression node.
// - The `Token` and `Operator` fields are set from the current token (e.g., '!').
// - It then advances the token (`p.nextToken()`).
// - Recursively calls `parseExpression` with the `PREFIX` precedence to parse the expression
//   that follows the prefix operator (the right-hand side).
func (p *Parser) parsePrefixExpression() ast.Expression {
	expression := &ast.PrefixExpression{
		Token:    p.curToken,        // The prefix token itself (e.g., "!", "-").
		Operator: p.curToken.Literal, // The operator as a string.
	}

	p.nextToken() // Consume the prefix operator token.

	// Parse the expression that follows the prefix operator.
	// The PREFIX precedence is passed to correctly handle expressions like "--5" or "!!true".
	expression.Right = p.parseExpression(PREFIX)

	return expression
}

// parseInfixExpression is an infix parsing function for operators like '+', '-', '*', '/', '==', etc.
// It's called when an infix operator is encountered after a left-hand side expression (`left`) has already been parsed.
// - It takes the already parsed `left` expression as an argument.
// - Creates an ast.InfixExpression node, setting its `Left` field to the passed `left` expression,
//   and `Token` and `Operator` fields from the current token (the infix operator).
// - It captures the precedence of the current infix operator.
// - It advances the token (`p.nextToken()`).
// - Recursively calls `parseExpression` with the captured operator's precedence to parse the
//   right-hand side of the infix expression. This handles associativity correctly (e.g., for left-associative
//   operators like `a - b - c`, `(a - b)` is parsed first, then `- c` is applied to that result).
func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	expression := &ast.InfixExpression{
		Token:    p.curToken,        // The infix operator token (e.g., "+", "==").
		Operator: p.curToken.Literal, // The operator as a string.
		Left:     left,              // The expression on the left side of the operator.
	}

	precedence := p.curPrecedence() // Get the precedence of the current infix operator.
	p.nextToken()                   // Consume the infix operator token.

	// Parse the expression on the right side of the operator.
	// Passing `precedence` here ensures correct associativity. For left-associative operators,
	// parsing `a + b + c` as `(a + b) + c`, the right-hand side of the first `+` (i.e. `b`)
	// will be parsed with the precedence of `+`. If the next operator also has the same or lower precedence,
	// it won't be consumed by this recursive call, allowing the outer loop in `parseExpression` to handle it.
	expression.Right = p.parseExpression(precedence)

	return expression
}

// parseBoolean is a prefix parsing function for boolean literals (`true` or `false`).
// It returns an ast.Boolean node, setting its `Value` field based on whether
// the current token is token.TRUE.
func (p *Parser) parseBoolean() ast.Expression {
	return &ast.Boolean{Token: p.curToken, Value: p.curTokenIs(token.TRUE)}
}

// parseGroupedExpression is a prefix parsing function for expressions enclosed in parentheses.
// Example: `(5 + 5)`
// - It consumes the opening parenthesis `(`.
// - Recursively calls `parseExpression` with `LOWEST` precedence to parse the expression
//   inside the parentheses. This allows any expression to be nested.
// - It then expects and consumes a closing parenthesis `)`. If not found, an error is recorded.
func (p *Parser) parseGroupedExpression() ast.Expression {
	p.nextToken() // Consume the '('.

	// Parse the expression inside the parentheses. LOWEST precedence is used
	// because the parentheses define a new, isolated expression context.
	exp := p.parseExpression(LOWEST)

	// Expect a closing parenthesis ')'.
	if !p.expectPeek(token.RPAREN) {
		return nil // Error already recorded by expectPeek.
	}

	return exp // Return the parsed inner expression.
}

// parseIfExpression is a prefix parsing function for 'if' expressions.
// It expects the format: IF LPAREN Condition RPAREN LBRACE Consequence RBRACE [ELSE LBRACE Alternative RBRACE]
// - Parses the condition expression enclosed in parentheses.
// - Parses the consequence block statement enclosed in curly braces.
// - Optionally parses an 'else' branch with an alternative block statement.
func (p *Parser) parseIfExpression() ast.Expression {
	expression := &ast.IfExpression{Token: p.curToken} // Current token is 'if'.

	// Expect an opening parenthesis '(' for the condition.
	if !p.expectPeek(token.LPAREN) {
		return nil
	}

	p.nextToken() // Consume '('.
	// Parse the condition expression.
	expression.Condition = p.parseExpression(LOWEST)

	// Expect a closing parenthesis ')' after the condition.
	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	// Expect an opening curly brace '{' for the consequence block.
	if !p.expectPeek(token.LBRACE) {
		return nil
	}
	expression.Consequence = p.parseBlockStatement()

	// Check for an optional 'else' branch.
	if p.peekTokenIs(token.ELSE) {
		p.nextToken() // Consume 'else'.

		// Expect an opening curly brace '{' for the alternative block.
		if !p.expectPeek(token.LBRACE) {
			return nil
		}
		expression.Alternative = p.parseBlockStatement()
	}

	return expression
}

// parseBlockStatement parses a block of statements enclosed in curly braces `{ ... }`.
// It's used for the bodies of functions, and the consequence/alternative branches of if-expressions.
// - Creates an ast.BlockStatement node.
// - Consumes the opening LBRACE.
// - It iteratively calls `parseStatement` to parse each statement within the block
//   until an RBRACE token or EOF is encountered.
// - Consumes the closing RBRACE.
func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Token: p.curToken} // Current token is '{'.
	block.Statements = []ast.Statement{}

	p.nextToken() // Consume '{'.

	// Parse statements until a '}' or EOF is encountered.
	for !p.curTokenIs(token.RBRACE) && !p.curTokenIs(token.EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		p.nextToken() // Advance to the next statement or '}'.
	}
	// Note: The closing '}' is consumed by the caller or the main ParseProgram loop
	// if this block is the last part of an if/fn. If called from parseIfExpression,
	// expectPeek(RBRACE) is typically not needed here as the loop condition handles it.
	// The current token after this loop will be RBRACE or EOF.
	return block
}

// parseFunctionLiteral is a prefix parsing function for function literals (anonymous functions).
// It expects the format: FN LPAREN Parameters RPAREN LBRACE Body RBRACE
// - Parses the parameter list using `parseFunctionParameters`.
// - Parses the function body (a block statement) using `parseBlockStatement`.
func (p *Parser) parseFunctionLiteral() ast.Expression {
	lit := &ast.FunctionLiteral{Token: p.curToken} // Current token is 'fn'.

	// Expect an opening parenthesis '(' for parameters.
	if !p.expectPeek(token.LPAREN) {
		return nil
	}
	lit.Parameters = p.parseFunctionParameters()

	// Expect an opening curly brace '{' for the function body.
	if !p.expectPeek(token.LBRACE) {
		return nil
	}
	lit.Body = p.parseBlockStatement()

	return lit
}

// parseFunctionParameters parses the list of parameters for a function literal.
// Expects a comma-separated list of identifiers enclosed in parentheses `()`.
// - Handles an empty parameter list (just `()`).
// - Parses one or more identifiers, separated by commas.
// - Expects a closing parenthesis `)`.
func (p *Parser) parseFunctionParameters() []*ast.Identifier {
	identifiers := []*ast.Identifier{}

	// Check for an empty parameter list: next token is ')'.
	if p.peekTokenIs(token.RPAREN) {
		p.nextToken() // Consume ')'.
		return identifiers
	}

	p.nextToken() // Consume '('. Now curToken is the first parameter.

	// Parse the first parameter.
	ident := &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
	identifiers = append(identifiers, ident)

	// Parse subsequent parameters, separated by commas.
	for p.peekTokenIs(token.COMMA) {
		p.nextToken() // Consume ','.
		p.nextToken() // Consume the identifier token.
		ident := &ast.Identifier{Token: p.curToken, Value: p.curToken.Literal}
		identifiers = append(identifiers, ident)
	}

	// Expect a closing parenthesis ')' after parameters.
	if !p.expectPeek(token.RPAREN) {
		return nil
	}

	return identifiers
}

// parseCallExpression is an infix parsing function for function calls.
// It's triggered when an LPAREN `(` is encountered after an expression that represents the function.
// - `function` is the ast.Expression that was parsed to the left of `(`. This could be an Identifier (function name) or a FunctionLiteral itself.
// - It creates an ast.CallExpression node.
// - It calls `parseExpressionList` (or a similar helper like `parseCallArguments`) to parse the
//   comma-separated arguments passed to the function, expecting a closing RPAREN `)`.
func (p *Parser) parseCallExpression(function ast.Expression) ast.Expression {
	// `function` is the expression that evaluates to the function being called (e.g., identifier, function literal).
	// `p.curToken` is '('.
	exp := &ast.CallExpression{Token: p.curToken, Function: function}
	// `parseExpressionList` handles parsing a list of expressions until a given `end` token.
	exp.Arguments = p.parseExpressionList(token.RPAREN)
	return exp
}

// parseExpressionList parses a list of comma-separated expressions,
// continuing until an `end` token (e.g., RPAREN for function arguments, RBRACKET for array elements) is encountered.
// - Handles an empty list (e.g., `()` or `[]`).
// - Parses one or more expressions, separated by commas.
// - Expects the `end` token to terminate the list.
func (p *Parser) parseExpressionList(end token.TokenType) []ast.Expression {
	list := []ast.Expression{}

	// Check for an empty list (e.g., if peekToken is ')' for call arguments or ']' for array elements).
	if p.peekTokenIs(end) {
		p.nextToken() // Consume the `end` token (e.g., ')', ']').
		return list
	}

	p.nextToken() // Consume the token that starts the list (e.g., first argument, first element).
	// Parse the first expression in the list.
	list = append(list, p.parseExpression(LOWEST))

	// Parse subsequent expressions, separated by commas.
	for p.peekTokenIs(token.COMMA) {
		p.nextToken() // Consume ','.
		p.nextToken() // Consume the token that starts the next expression.
		list = append(list, p.parseExpression(LOWEST))
	}

	// Expect the `end` token (e.g., ')', ']') to terminate the list.
	if !p.expectPeek(end) {
		return nil // Error already recorded by expectPeek.
	}

	return list
}

// parseArrayLiteral is a prefix parsing function for array literals.
// Example: `[1, "two", 3 + 3]`
// - It creates an ast.ArrayLiteral node.
// - It calls `parseExpressionList` with `token.RBRACKET` as the end token
//   to parse the comma-separated elements of the array.
func (p *Parser) parseArrayLiteral() ast.Expression {
	array := &ast.ArrayLiteral{Token: p.curToken} // Current token is '['.

	// `parseExpressionList` handles parsing elements until a closing ']'.
	array.Elements = p.parseExpressionList(token.RBRACKET)

	return array
}

// parseIndexExpression is an infix parsing function for array/hash indexing operations.
// It's triggered when an LBRACKET `[` is encountered after an expression representing the collection.
// - `left` is the ast.Expression that was parsed to the left of `[` (the array or hash being indexed).
// - It creates an ast.IndexExpression node.
// - It advances past the `[` token.
// - Recursively calls `parseExpression` to parse the index expression itself (inside the brackets).
// - Expects and validates a closing RBRACKET `]`.
func (p *Parser) parseIndexExpression(left ast.Expression) ast.Expression {
	// `left` is the expression for the array or hash.
	// `p.curToken` is '['.
	exp := &ast.IndexExpression{Token: p.curToken, Left: left}

	p.nextToken() // Consume '['.
	// Parse the index expression.
	exp.Index = p.parseExpression(LOWEST)

	// Expect a closing ']' for the index operation.
	if !p.expectPeek(token.RBRACKET) {
		return nil
	}

	return exp
}

// parseHashLiteral is a prefix parsing function for hash literals (also known as maps or dictionaries).
// Example: `{"one": 1, "two": 2 + 0, "three": fn() { return 3; }}`
// - It expects key-value pairs separated by colons, and pairs separated by commas, all enclosed in curly braces `{}`.
// - It creates an ast.HashLiteral node.
// - It iterates, parsing a key expression, expecting a colon, then parsing a value expression.
// - It continues as long as it finds commas separating pairs.
// - Finally, it expects a closing RBRACE `}`.
func (p *Parser) parseHashLiteral() ast.Expression {
	hash := &ast.HashLiteral{Token: p.curToken} // Current token is '{'.
	hash.Pairs = make(map[ast.Expression]ast.Expression)

	// Loop to parse key-value pairs.
	for !p.peekTokenIs(token.RBRACE) { // Continue as long as the next token is not '}'.
		p.nextToken() // Consume '{' or ',' (for subsequent pairs).
		key := p.parseExpression(LOWEST)

		// Expect a colon ':' separating key and value.
		if !p.expectPeek(token.COLON) {
			return nil
		}

		p.nextToken() // Consume ':'.
		value := p.parseExpression(LOWEST)

		hash.Pairs[key] = value

		// If the next token is not '}', it must be a comma (if there are more pairs).
		if !p.peekTokenIs(token.RBRACE) && !p.expectPeek(token.COMMA) {
			return nil // Expected '}' or ','.
		}
	}

	// Expect a closing '}' for the hash literal.
	if !p.expectPeek(token.RBRACE) {
		return nil
	}

	return hash
}

// registerPrefix associates a tokenType with a prefix parsing function.
// This function will be called when `tokenType` is encountered in a prefix position.
// This is a core part of the Pratt parsing setup.
func (p *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

// registerInfix associates a tokenType with an infix parsing function.
// This function will be called when `tokenType` is encountered in an infix position.
// This is a core part of the Pratt parsing setup.
func (p *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}
