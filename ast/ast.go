package ast

import (
	"bytes"
	"monkey/token"
	"strings"
)

// Node is the base interface for all nodes in the Abstract Syntax Tree (AST).
// It requires all nodes to provide a `TokenLiteral()` method, which returns the literal
// value of the token associated with the node. This is primarily used for debugging and testing.
// The `String()` method provides a human-readable string representation of the AST node,
// also useful for debugging and testing.
type Node interface {
	TokenLiteral() string // Returns the literal value of the token this node is associated with.
	String() string       // Returns a string representation of the node.
}

// Statement is an interface that all statement nodes (e.g., LetStatement, ReturnStatement) must implement.
// It embeds the Node interface, so all statements are also Nodes.
// The `statementNode()` method is a placeholder; it doesn't perform any action but is used to
// distinguish Statement nodes from Expression nodes at the type system level. This technique is
// sometimes called "marker interface" or "embedding for type discrimination".
type Statement interface {
	Node
	statementNode() // Placeholder method for type discrimination.
}

// Expression is an interface that all expression nodes (e.g., Identifier, IntegerLiteral, InfixExpression) must implement.
// It embeds the Node interface, so all expressions are also Nodes.
// The `expressionNode()` method is a placeholder, similar to `statementNode()`, used to
// distinguish Expression nodes from Statement nodes within Go's type system.
type Expression interface {
	Node
	expressionNode() // Placeholder method for type discrimination.
}

// Program is the root node of every AST that our parser produces.
// It holds a slice of Statements, representing the sequence of statements in the source code.
type Program struct {
	Statements []Statement // A slice of statements that make up the program.
}

// TokenLiteral for a Program node returns the token literal of the first statement if it exists,
// otherwise an empty string. This is a convention, as a program itself doesn't have a single token.
func (p *Program) TokenLiteral() string {
	if len(p.Statements) > 0 {
		return p.Statements[0].TokenLiteral()
	} else {
		return ""
	}
}

// String for a Program node concatenates the string representations of all its statements.
// This provides a string representation of the entire program's AST.
func (p *Program) String() string {
	var out bytes.Buffer

	for _, s := range p.Statements {
		out.WriteString(s.String())
	}

	return out.String()
}

// Statements

// LetStatement represents a 'let' statement, used for variable binding.
// Example: `let x = 5;`
type LetStatement struct {
	Token token.Token   // The token.LET token.
	Name  *Identifier   // The identifier (variable name) being bound.
	Value Expression    // The expression producing the value to be bound to the identifier.
}

func (ls *LetStatement) statementNode()       {} // Implements the Statement interface.
// TokenLiteral returns the literal "let".
func (ls *LetStatement) TokenLiteral() string { return ls.Token.Literal }
// String returns a string representation like "let <Name> = <Value>;".
func (ls *LetStatement) String() string {
	var out bytes.Buffer

	out.WriteString(ls.TokenLiteral() + " ")
	out.WriteString(ls.Name.String())
	out.WriteString(" = ")

	if ls.Value != nil {
		out.WriteString(ls.Value.String())
	}

	out.WriteString(";")

	return out.String()
}

// ReturnStatement represents a 'return' statement, used for returning a value from a function.
// Example: `return x + y;`
type ReturnStatement struct {
	Token       token.Token // The 'return' token.
	ReturnValue Expression  // The expression whose value is to be returned.
}

func (rs *ReturnStatement) statementNode()       {} // Implements the Statement interface.
// TokenLiteral returns the literal "return".
func (rs *ReturnStatement) TokenLiteral() string { return rs.Token.Literal }
// String returns a string representation like "return <ReturnValue>;".
func (rs *ReturnStatement) String() string {
	var out bytes.Buffer

	out.WriteString(rs.TokenLiteral() + " ")

	if rs.ReturnValue != nil {
		out.WriteString(rs.ReturnValue.String())
	}

	out.WriteString(";")

	return out.String()
}

// ExpressionStatement represents a statement that consists solely of an expression.
// In Monkey, expressions can be statements (e.g., `x + 10;` or a function call `myFunction(x);`).
// The result of the expression is discarded if it's used as a statement.
type ExpressionStatement struct {
	Token      token.Token // The first token of the expression.
	Expression Expression  // The expression itself.
}

func (es *ExpressionStatement) statementNode()       {} // Implements the Statement interface.
// TokenLiteral returns the token literal of the first token of the expression.
func (es *ExpressionStatement) TokenLiteral() string { return es.Token.Literal }
// String returns the string representation of the underlying expression.
func (es *ExpressionStatement) String() string {
	if es.Expression != nil {
		return es.Expression.String()
	}
	return ""
}

// BlockStatement represents a sequence of statements enclosed in curly braces `{ ... }`.
// It's often used as the body of functions or as the consequence/alternative branches of if-expressions.
type BlockStatement struct {
	Token      token.Token // The '{' token.
	Statements []Statement // The list of statements within the block.
}

func (bs *BlockStatement) statementNode()       {} // Implements the Statement interface.
// TokenLiteral returns the literal "{".
func (bs *BlockStatement) TokenLiteral() string { return bs.Token.Literal }
// String concatenates the string representations of all statements in the block.
func (bs *BlockStatement) String() string {
	var out bytes.Buffer

	for _, s := range bs.Statements {
		out.WriteString(s.String())
	}

	return out.String()
}

// Expressions

// Identifier represents an identifier, such as a variable name or a function name.
// Example: `x`, `myVariable`, `add`
type Identifier struct {
	Token token.Token // The token.IDENT token (e.g., {Type:IDENT, Literal:"myVar"}).
	Value string      // The string value of the identifier (e.g., "myVar").
}

func (i *Identifier) expressionNode()      {} // Implements the Expression interface.
// TokenLiteral returns the literal value of the identifier token (e.g., "myVar").
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }
// String returns the string value of the identifier.
func (i *Identifier) String() string       { return i.Value }

// Boolean represents a boolean literal (`true` or `false`).
// Example: `true`, `false`
type Boolean struct {
	Token token.Token // The token.TRUE or token.FALSE token.
	Value bool        // The actual boolean value (true or false).
}

func (b *Boolean) expressionNode()      {} // Implements the Expression interface.
// TokenLiteral returns "true" or "false".
func (b *Boolean) TokenLiteral() string { return b.Token.Literal }
// String returns "true" or "false".
func (b *Boolean) String() string       { return b.Token.Literal }

// IntegerLiteral represents an integer number.
// Example: `5`, `100`, `0`
type IntegerLiteral struct {
	Token token.Token // The token.INT token (e.g., {Type:INT, Literal:"123"}).
	Value int64       // The actual integer value.
}

func (il *IntegerLiteral) expressionNode()      {} // Implements the Expression interface.
// TokenLiteral returns the string representation of the integer (e.g., "123").
func (il *IntegerLiteral) TokenLiteral() string { return il.Token.Literal }
// String returns the string representation of the integer.
func (il *IntegerLiteral) String() string       { return il.Token.Literal }

// PrefixExpression represents an expression with a prefix operator.
// Example: `!true`, `-15`
type PrefixExpression struct {
	Token    token.Token // The prefix token, e.g., '!' or '-'.
	Operator string      // The operator itself as a string (e.g., "!" or "-").
	Right    Expression  // The expression to the right of the operator.
}

func (pe *PrefixExpression) expressionNode()      {} // Implements the Expression interface.
// TokenLiteral returns the literal of the prefix token (e.g., "!" or "-").
func (pe *PrefixExpression) TokenLiteral() string { return pe.Token.Literal }
// String returns a string representation like "(<Operator><RightExpression>)".
func (pe *PrefixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(pe.Operator)
	out.WriteString(pe.Right.String())
	out.WriteString(")")

	return out.String()
}

// InfixExpression represents an expression with an infix operator between two operands.
// Example: `5 + 5`, `x * y`, `foo == bar`
type InfixExpression struct {
	Token    token.Token // The operator token, e.g., '+', '-', '=='.
	Left     Expression  // The expression to the left of the operator.
	Operator string      // The operator itself as a string (e.g., "+", "==").
	Right    Expression  // The expression to the right of the operator.
}

func (oe *InfixExpression) expressionNode()      {} // Implements the Expression interface.
// TokenLiteral returns the literal of the operator token (e.g., "+").
func (oe *InfixExpression) TokenLiteral() string { return oe.Token.Literal }
// String returns a string representation like "(<LeftExpression> <Operator> <RightExpression>)".
func (oe *InfixExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(oe.Left.String())
	out.WriteString(" " + oe.Operator + " ")
	out.WriteString(oe.Right.String())
	out.WriteString(")")

	return out.String()
}

// IfExpression represents an 'if' expression, which allows for conditional execution.
// It can optionally include an 'else' branch.
// Example: `if (x < y) { return x } else { return y }`
type IfExpression struct {
	Token       token.Token     // The 'if' token.
	Condition   Expression      // The condition to be evaluated.
	Consequence *BlockStatement // The block of statements to execute if the condition is true.
	Alternative *BlockStatement // The block of statements to execute if the condition is false (optional, can be nil).
}

func (ie *IfExpression) expressionNode()      {} // Implements the Expression interface.
// TokenLiteral returns the literal "if".
func (ie *IfExpression) TokenLiteral() string { return ie.Token.Literal }
// String returns a string representation like "if <Condition> <Consequence> else <Alternative>".
func (ie *IfExpression) String() string {
	var out bytes.Buffer

	out.WriteString("if")
	out.WriteString(ie.Condition.String())
	out.WriteString(" ")
	out.WriteString(ie.Consequence.String())

	if ie.Alternative != nil {
		out.WriteString("else ")
		out.WriteString(ie.Alternative.String())
	}

	return out.String()
}

// FunctionLiteral represents the definition of a function.
// Example: `fn(x, y) { x + y; }`
type FunctionLiteral struct {
	Token      token.Token   // The 'fn' token.
	Parameters []*Identifier // A list of identifiers representing the function's parameters.
	Body       *BlockStatement // The block of statements that constitutes the function's body.
}

func (fl *FunctionLiteral) expressionNode()      {} // Implements the Expression interface.
// TokenLiteral returns the literal "fn".
func (fl *FunctionLiteral) TokenLiteral() string { return fl.Token.Literal }
// String returns a string representation like "fn(<Parameters>) <Body>".
func (fl *FunctionLiteral) String() string {
	var out bytes.Buffer

	params := []string{}
	for _, p := range fl.Parameters {
		params = append(params, p.String())
	}

	out.WriteString(fl.TokenLiteral())
	out.WriteString("(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(") ")
	out.WriteString(fl.Body.String())

	return out.String()
}

// CallExpression represents a function call.
// Example: `add(2, 3)`, `myFunction()`
type CallExpression struct {
	Token     token.Token  // The '(' token that signifies the start of the argument list.
	Function  Expression   // The expression that evaluates to a function (typically an Identifier or a FunctionLiteral).
	Arguments []Expression // A list of expressions representing the arguments passed to the function.
}

func (ce *CallExpression) expressionNode()      {} // Implements the Expression interface.
// TokenLiteral returns the literal "(".
func (ce *CallExpression) TokenLiteral() string { return ce.Token.Literal }
// String returns a string representation like "<Function>(<Arguments>)".
func (ce *CallExpression) String() string {
	var out bytes.Buffer

	args := []string{}
	for _, a := range ce.Arguments {
		args = append(args, a.String())
	}

	out.WriteString(ce.Function.String())
	out.WriteString("(")
	out.WriteString(strings.Join(args, ", "))
	out.WriteString(")")

	return out.String()
}

// StringLiteral represents a string data type.
// Example: `"hello world"`, `""`
type StringLiteral struct {
	Token token.Token // The token.STRING token (e.g., {Type:STRING, Literal:"hello"}).
	Value string      // The actual string value (e.g., "hello").
}

func (sl *StringLiteral) expressionNode()      {} // Implements the Expression interface.
// TokenLiteral returns the literal string value itself (e.g., "hello").
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Literal }
// String returns the literal string value, which is often the same as TokenLiteral for strings.
func (sl *StringLiteral) String() string       { return sl.Token.Literal }

// ArrayLiteral represents an array data structure.
// Example: `[1, 2 * 2, "three"]`
type ArrayLiteral struct {
	Token    token.Token  // The '[' token.
	Elements []Expression // A list of expressions, each evaluating to an element of the array.
}

func (al *ArrayLiteral) expressionNode()      {} // Implements the Expression interface.
// TokenLiteral returns the literal "[".
func (al *ArrayLiteral) TokenLiteral() string { return al.Token.Literal }
// String returns a string representation like "[<Element1>, <Element2>, ...]".
func (al *ArrayLiteral) String() string {
	var out bytes.Buffer

	elements := []string{}
	for _, el := range al.Elements {
		elements = append(elements, el.String())
	}

	out.WriteString("[")
	out.WriteString(strings.Join(elements, ", "))
	out.WriteString("]")

	return out.String()
}

// IndexExpression represents accessing an element in an array or hash by its index or key.
// Example: `myArray[0]`, `myHash["key"]`
type IndexExpression struct {
	Token token.Token // The '[' token used for indexing.
	Left  Expression  // The expression that evaluates to the array or hash being indexed (e.g., an Identifier).
	Index Expression  // The expression that evaluates to the index or key.
}

func (ie *IndexExpression) expressionNode()      {} // Implements the Expression interface.
// TokenLiteral returns the literal "[".
func (ie *IndexExpression) TokenLiteral() string { return ie.Token.Literal }
// String returns a string representation like "(<LeftExpression>[<IndexExpression>])".
func (ie *IndexExpression) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(ie.Left.String())
	out.WriteString("[")
	out.WriteString(ie.Index.String())
	out.WriteString("])")

	return out.String()
}

// HashLiteral represents a hash map (also known as a dictionary or object) data structure.
// Keys and values are both expressions.
// Example: `{"one": 1, "two": 2, "three": 3}`
type HashLiteral struct {
	Token token.Token             // The '{' token.
	Pairs map[Expression]Expression // A map where keys and values are both expressions.
}

func (hl *HashLiteral) expressionNode()      {} // Implements the Expression interface.
// TokenLiteral returns the literal "{".
func (hl *HashLiteral) TokenLiteral() string { return hl.Token.Literal }
// String returns a string representation like "{<Key1>:<Value1>, <Key2>:<Value2>, ...}".
func (hl *HashLiteral) String() string {
	var out bytes.Buffer

	pairs := []string{}
	for key, value := range hl.Pairs {
		pairs = append(pairs, key.String()+":"+value.String())
	}

	out.WriteString("{")
	out.WriteString(strings.Join(pairs, ", "))
	out.WriteString("}")

	return out.String()
}
