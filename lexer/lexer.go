package lexer

import "monkey/token"

// Lexer struct represents the lexical analyzer for the Monkey language.
// It takes a string input and produces a stream of tokens.
type Lexer struct {
	input        string // The input string to be tokenized.
	position     int    // current position in input (points to current char).
	readPosition int    // current reading position in input (after current char).
	ch           byte   // current char under examination.
}

// New creates and returns a new Lexer instance.
// It initializes the lexer with the input string and reads the first character.
func New(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar() // Initialize l.ch, l.position, and l.readPosition.
	return l
}

// NextToken is the core method of the Lexer. It reads the input and returns the next token.
// It performs the following steps:
// 1. Skips any whitespace characters (spaces, tabs, newlines, carriage returns) using `skipWhitespace`.
// 2. Uses a `switch` statement on the current character (`l.ch`) to determine the token type.
//    - Handles single-character tokens like operators (+, -, *, /) and delimiters (,, ;, (, ), {, }).
//    - Handles multi-character tokens like `==` (EQ) and `!=` (NOT_EQ) by checking the next character using `peekChar`.
//    - If the character is a letter, it reads an identifier using `readIdentifier`. It then uses `token.LookupIdent`
//      to check if the identifier is a keyword (like `fn`, `let`, `if`) or a user-defined identifier.
//    - If the character is a digit, it reads a number (integer in Monkey) using `readNumber`.
//    - If the character is a double quote (`"`), it reads a string literal using `readString`.
//    - If the character is 0 (EOF), it returns an EOF token.
//    - If the character is not recognized, it creates an `ILLEGAL` token.
// 3. Advances the lexer to the next character by calling `readChar` before returning the token (unless a return happened earlier, e.g. for identifiers or numbers).
func (l *Lexer) NextToken() token.Token {
	var tok token.Token

	l.skipWhitespace() // Skip any intervening whitespace.

	switch l.ch {
	case '=':
		if l.peekChar() == '=' { // Check for '=='
			ch := l.ch
			l.readChar() // Consume the second '='
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.EQ, Literal: literal}
		} else {
			tok = newToken(token.ASSIGN, l.ch)
		}
	case '+':
		tok = newToken(token.PLUS, l.ch)
	case '-':
		tok = newToken(token.MINUS, l.ch)
	case '!':
		if l.peekChar() == '=' { // Check for '!='
			ch := l.ch
			l.readChar() // Consume the '='
			literal := string(ch) + string(l.ch)
			tok = token.Token{Type: token.NOT_EQ, Literal: literal}
		} else {
			tok = newToken(token.BANG, l.ch)
		}
	case '/':
		tok = newToken(token.SLASH, l.ch)
	case '*':
		tok = newToken(token.ASTERISK, l.ch)
	case '<':
		tok = newToken(token.LT, l.ch)
	case '>':
		tok = newToken(token.GT, l.ch)
	case ';':
		tok = newToken(token.SEMICOLON, l.ch)
	case ':':
		tok = newToken(token.COLON, l.ch)
	case ',':
		tok = newToken(token.COMMA, l.ch)
	case '{':
		tok = newToken(token.LBRACE, l.ch)
	case '}':
		tok = newToken(token.RBRACE, l.ch)
	case '(':
		tok = newToken(token.LPAREN, l.ch)
	case ')':
		tok = newToken(token.RPAREN, l.ch)
	case '"':
		tok.Type = token.STRING
		tok.Literal = l.readString() // Read the entire string literal.
	case '[':
		tok = newToken(token.LBRACKET, l.ch)
	case ']':
		tok = newToken(token.RBRACKET, l.ch)
	case 0: // Null byte signifies EOF (End Of File).
		tok.Literal = ""
		tok.Type = token.EOF
	default:
		if isLetter(l.ch) { // Check if it's a letter (start of an identifier or keyword).
			tok.Literal = l.readIdentifier()         // Read the full identifier.
			tok.Type = token.LookupIdent(tok.Literal) // Determine if it's a keyword or user-defined identifier.
			return tok                                // Return early as readIdentifier advances readChar.
		} else if isDigit(l.ch) { // Check if it's a digit (start of a number).
			tok.Type = token.INT
			tok.Literal = l.readNumber() // Read the full number.
			return tok                   // Return early as readNumber advances readChar.
		} else { // Character is not recognized.
			tok = newToken(token.ILLEGAL, l.ch)
		}
	}

	l.readChar() // Advance to the next character for the next call to NextToken().
	return tok
}

// skipWhitespace advances the lexer past any whitespace characters
// (space, tab, newline, carriage return).
func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		l.readChar()
	}
}

// readChar reads the next character from the input string and advances the lexer's position.
// It updates `l.ch` with the character at `l.readPosition`.
// If `l.readPosition` is beyond the end of the input, `l.ch` is set to 0 (ASCII NUL), signifying EOF.
// `l.position` is updated to the position of the character just read, and `l.readPosition` is incremented.
func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0 // NUL character, signifies EOF or "nothing read yet"
	} else {
		l.ch = l.input[l.readPosition]
	}
	l.position = l.readPosition
	l.readPosition += 1
}

// peekChar returns the character at the current `l.readPosition` without advancing the lexer.
// This is used to "look ahead" one character, for example, to distinguish between `=` (ASSIGN) and `==` (EQ),
// or `!` (BANG) and `!=` (NOT_EQ).
// If `l.readPosition` is beyond the end of the input, it returns 0.
func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	} else {
		return l.input[l.readPosition]
	}
}

// readIdentifier reads a sequence of characters that form an identifier.
// It continues reading as long as `isLetter` returns true for the current character.
// The method returns the substring from the input that constitutes the identifier.
// It advances the lexer's position past the identifier.
func (l *Lexer) readIdentifier() string {
	position := l.position
	for isLetter(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position] // l.position is now one past the end of the identifier.
}

// readNumber reads a sequence of characters that form a number (integer in Monkey).
// It continues reading as long as `isDigit` returns true for the current character.
// The method returns the substring from the input that constitutes the number.
// It advances the lexer's position past the number.
func (l *Lexer) readNumber() string {
	position := l.position
	for isDigit(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position] // l.position is now one past the end of the number.
}

// readString reads a string literal enclosed in double quotes.
// It starts reading from the character after the opening double quote.
// It continues reading until it encounters a closing double quote or the end of the input (EOF).
// The method returns the content of the string (without the enclosing quotes).
// It advances the lexer's position past the closing double quote.
func (l *Lexer) readString() string {
	position := l.position + 1 // Start after the opening "
	for {
		l.readChar()
		if l.ch == '"' || l.ch == 0 { // End of string or EOF
			break
		}
	}
	return l.input[position:l.position] // l.position is at the closing " or EOF
}

// isLetter is a helper function that checks if a given character is a letter (a-z, A-Z) or an underscore.
// Underscore is often allowed in identifiers.
func isLetter(ch byte) bool {
	return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

// isDigit is a helper function that checks if a given character is a digit (0-9).
func isDigit(ch byte) bool {
	return '0' <= ch && ch <= '9'
}

// newToken is a helper function to create a new token.Token.
// It takes the token type and the character that forms the token's literal value.
func newToken(tokenType token.TokenType, ch byte) token.Token {
	return token.Token{Type: tokenType, Literal: string(ch)}
}
