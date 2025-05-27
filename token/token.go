package token

// TokenType is a string type that represents the type of a token.
// This allows us to use descriptive names for token types, improving code readability.
type TokenType string

// Token struct represents a token in the Monkey language.
// It contains the type of the token and its literal value.
type Token struct {
	Type    TokenType // Type of the token, e.g., IDENT, INT, ASSIGN.
	Literal string    // Literal value of the token, e.g., "x", "5", "=".
}

const (
	// ILLEGAL signifies a token that the lexer couldn't recognize.
	// This is used for characters or sequences of characters that don't match any defined token.
	ILLEGAL = "ILLEGAL"
	// EOF signifies the end of the input file.
	// This token is used to indicate that there are no more tokens to lex.
	EOF = "EOF"

	// Identifiers + literals
	// IDENT represents an identifier, such as a variable name or function name.
	// Examples: add, foobar, x, y, ...
	IDENT = "IDENT"
	// INT represents an integer literal.
	// Examples: 1343456, 0, 42
	INT = "INT"
	// STRING represents a string literal.
	// Examples: "foobar", "hello world"
	STRING = "STRING"

	// Operators
	// ASSIGN represents the assignment operator '='.
	// Used for assigning values to variables.
	ASSIGN = "="
	// PLUS represents the addition operator '+'.
	// Used for arithmetic addition.
	PLUS = "+"
	// MINUS represents the subtraction operator '-'.
	// Used for arithmetic subtraction.
	MINUS = "-"
	// BANG represents the logical negation operator '!'.
	// Used for inverting boolean values.
	BANG = "!"
	// ASTERISK represents the multiplication operator '*'.
	// Used for arithmetic multiplication.
	ASTERISK = "*"
	// SLASH represents the division operator '/'.
	// Used for arithmetic division.
	SLASH = "/"

	// LT represents the less than operator '<'.
	// Used for comparison.
	LT = "<"
	// GT represents the greater than operator '>'.
	// Used for comparison.
	GT = ">"

	// EQ represents the equality operator '=='.
	// Used for comparing two values for equality.
	EQ = "=="
	// NOT_EQ represents the inequality operator '!='.
	// Used for comparing two values for inequality.
	NOT_EQ = "!="

	// Delimiters
	// COMMA represents the comma delimiter ','.
	// Used for separating items in lists, function arguments, etc.
	COMMA = ","
	// SEMICOLON represents the semicolon delimiter ';'.
	// Used for separating statements.
	SEMICOLON = ";"
	// COLON represents the colon delimiter ':'.
	// Used in hash literals for separating keys and values.
	COLON = ":"

	// LPAREN represents the left parenthesis delimiter '('.
	// Used for grouping expressions and in function calls.
	LPAREN = "("
	// RPAREN represents the right parenthesis delimiter ')'.
	// Used for grouping expressions and in function calls.
	RPAREN = ")"
	// LBRACE represents the left brace delimiter '{'.
	// Used for defining code blocks (e.g., in function bodies, if statements).
	LBRACE = "{"
	// RBRACE represents the right brace delimiter '}'.
	// Used for defining code blocks.
	RBRACE = "}"
	// LBRACKET represents the left bracket delimiter '['.
	// Used for defining arrays and accessing array elements.
	LBRACKET = "["
	// RBRACKET represents the right bracket delimiter ']'.
	// Used for defining arrays and accessing array elements.
	RBRACKET = "]"

	// Keywords
	// FUNCTION represents the 'fn' keyword.
	// Used for defining functions.
	FUNCTION = "FUNCTION"
	// LET represents the 'let' keyword.
	// Used for declaring variables.
	LET = "LET"
	// TRUE represents the 'true' keyword.
	// Represents the boolean true value.
	TRUE = "TRUE"
	// FALSE represents the 'false' keyword.
	// Represents the boolean false value.
	FALSE = "FALSE"
	// IF represents the 'if' keyword.
	// Used for conditional execution.
	IF = "IF"
	// ELSE represents the 'else' keyword.
	// Used for alternative execution path in if statements.
	ELSE = "ELSE"
	// RETURN represents the 'return' keyword.
	// Used for returning values from functions.
	RETURN = "RETURN"
)

// keywords is a map that stores the mapping between keyword strings and their corresponding TokenType.
// This map is used by LookupIdent to determine if an identifier is a keyword.
var keywords = map[string]TokenType{
	"fn":     FUNCTION,
	"let":    LET,
	"true":   TRUE,
	"false":  FALSE,
	"if":     IF,
	"else":   ELSE,
	"return": RETURN,
}

// LookupIdent checks the keywords map to see if the given identifier is a keyword.
// If it is a keyword, it returns the keyword's TokenType.
// Otherwise, it returns the IDENT TokenType, indicating it's a user-defined identifier.
func LookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return IDENT
}
