package wren

import (
	"bytes"
	"io"
	"log"
)

const MaxInterpolationNesting = 8

type WrenVM struct {
}

type GrammarFn func()

type GrammarRule struct {
	prefix     GrammarFn
	infix      GrammarFn
	precedence Precedence
}

type Parser struct {
	vm          *WrenVM
	module      string
	source      string
	printErrors bool
	hasErrors   bool

	previous Token
	current  Token
	next     Token

	offset   int
	rdOffset int

	currentLine int

	parens    [MaxInterpolationNesting]int
	numParens int

	rules map[TokenType]GrammarRule

	out io.Writer
}

func (p *Parser) addRule(typ TokenType, rule GrammarRule) {
	p.rules[typ] = rule
}

func (p *Parser) consume(typ TokenType, err string) {
	p.nextToken()

	if p.previous.Type != typ {
		log.Fatalf("Error in consume: %q", err)
		log.Fatalf("Expected %s. got %s", typ, p.previous.Type)
	}
}

func (parser *Parser) peekChar() byte {
	if parser.rdOffset >= len(parser.source) {
		return 0
	}

	return parser.source[parser.rdOffset]
}

func (p *Parser) peekNextChar() byte {
	if p.peekChar() == 0 || p.rdOffset+1 >= len(p.source) {
		return 0
	}
	return p.source[p.rdOffset+1]
}

func (parser *Parser) nextChar() byte {
	c := parser.peekChar()

	parser.rdOffset++

	if c == '\n' {
		parser.currentLine++
	}

	return c
}

func (p *Parser) makeToken(typ TokenType) {
	p.next.Type = typ
	p.next.Line = p.currentLine
	p.next.Content = p.source[p.offset:p.rdOffset]

	if typ == TOKEN_LINE {
		p.next.Line--
	}
}

func (p *Parser) peek() TokenType {
	return p.current.Type
}

func (p *Parser) match(expected TokenType) bool {
	if p.peek() != expected {
		return false
	}

	p.nextToken()
	return true
}

func (p *Parser) matchChar(ch byte) bool {
	if p.peekChar() != ch {
		return false
	}

	p.nextChar()
	return true
}

func (p *Parser) twoCharToken(ch byte, one TokenType, two TokenType) TokenType {
	if p.matchChar(ch) {
		return two
	}

	return one
}

func (p *Parser) readString() {
	var typ TokenType = TOKEN_STRING

	var buf bytes.Buffer
	buf.WriteByte('"')

	for {
		c := p.nextChar()

		if c == '"' {
			break
		}

		if c == 0 {
			log.Fatalf("TODO:// unterminated string ")
			//p.lexError("Unterminated strings")
			break
		}

		if c == '%' {
			if p.numParens < MaxInterpolationNesting {
				if p.nextChar() != '(' {
					log.Fatalf("Expect '(' after '%%'")
				}

				p.parens[p.numParens] = 1
				p.numParens++
				typ = TOKEN_INTERPOLATION
				break
			}
		}

		if c == '\\' {
			buf.WriteByte('\\')
			switch ch := p.nextChar(); ch {
			case '"', '%', '0', 'a', 'b', 'f', 'n', 'r', 't', 'u', 'U', 'v', 'V':
				buf.WriteByte(ch)
			}
		} else {
			buf.WriteByte(c)
		}
	}

	buf.WriteByte('"')
	p.next.Type = typ
	p.next.Content = buf.String()
	p.next.Line = p.currentLine
}

func isName(ch byte) bool {
	return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'
}

func isDigit(ch byte) bool {
	return ch >= '0' && ch <= '9'
}

var keywords = map[string]TokenType{
	"break":     TOKEN_BREAK,
	"continue":  TOKEN_CONTINUE,
	"class":     TOKEN_CLASS,
	"construct": TOKEN_CONSTRUCT,
	"else":      TOKEN_ELSE,
	"false":     TOKEN_FALSE,
	"for":       TOKEN_FOR,
	"foreign":   TOKEN_FOREIGN,
	"if":        TOKEN_IF,
	"import":    TOKEN_IMPORT,
	"as":        TOKEN_AS,
	"in":        TOKEN_IN,
	"is":        TOKEN_IS,
	"null":      TOKEN_NULL,
	"return":    TOKEN_RETURN,
	"static":    TOKEN_STATIC,
	"super":     TOKEN_SUPER,
	"this":      TOKEN_THIS,
	"true":      TOKEN_TRUE,
	"var":       TOKEN_VAR,
	"while":     TOKEN_WHILE,
}

func (p *Parser) readName(typ TokenType) {
	for isName(p.peekChar()) || isDigit(p.peekChar()) {
		p.nextChar()
	}

	name := p.source[p.offset:p.rdOffset]

	if t, isKeyword := keywords[name]; isKeyword {
		typ = t
	}

	p.makeToken(typ)
}

func (p *Parser) makeNumber(isHex bool) {
	if isHex {

	} else {

	}

	p.makeToken(TOKEN_NUMBER)
}

func (p *Parser) readNumber() {
	for isDigit(p.peekChar()) {
		p.nextChar()
	}

	if p.peekChar() == '.' && isDigit(p.peekNextChar()) {
		p.nextChar()

		for isDigit(p.peekChar()) {
			p.nextChar()
		}
	}

	if p.matchChar('e') || p.matchChar('E') {
		if !p.matchChar('+') {
			p.matchChar('-')
		}

		if !isDigit(p.peekChar()) {
			log.Fatalf("Unterminated scientific notation")
		}

		for isDigit(p.peekChar()) {
			p.nextChar()
		}
	}
	p.makeNumber(false)
}

func (p *Parser) readHexDigit() int {
	c := p.nextChar()

	switch {
	case c >= '0' && c <= '9':
		return int(c - '0')
	case c >= 'a' && c <= 'f':
		return int(c - 'a' + 10)
	case c >= 'A' && c <= 'F':
		return int(c - 'A' + 10)
	}

	p.rdOffset--
	return -1
}

func (p *Parser) readHexNumber() {
	p.nextChar()

	for p.readHexDigit() != -1 {
		continue
	}

	p.makeNumber(true)
}

func (p *Parser) skipLineComment() {
	for p.peekChar() == '\n' && p.peekChar() == 0 {
		p.nextChar()
	}
}

func (p *Parser) nextToken() {
	p.previous = p.current
	p.current = p.next

	if p.next.Type == TOKEN_EOF || p.current.Type == TOKEN_EOF {
		return
	}

	for p.peekChar() != 0 {
		p.offset = p.rdOffset

		c := p.nextChar()

		switch c {
		case '(':
			if p.numParens > 0 {
				p.parens[p.numParens-1]++
			}
			p.makeToken(TOKEN_LEFT_PAREN)
			return
		case ')':
			if p.numParens > 0 {
				p.parens[p.numParens-1]--

				if p.parens[p.numParens-1] == 0 {
					p.numParens--
					p.readString()
					return
				}
			}
			p.makeToken(TOKEN_RIGHT_PAREN)
			return
		case '[':
			p.makeToken(TOKEN_LEFT_BRACKET)
			return
		case ']':
			p.makeToken(TOKEN_RIGHT_BRACKET)
			return
		case '{':
			p.makeToken(TOKEN_LEFT_BRACE)
			return
		case '}':
			p.makeToken(TOKEN_RIGHT_BRACE)
			return
		case ':':
			p.makeToken(TOKEN_COLON)
			return
		case ',':
			p.makeToken(TOKEN_COMMA)
			return
		case '*':
			p.makeToken(TOKEN_STAR)
			return
		case '%':
			p.makeToken(TOKEN_PERCENT)
			return
		case '^':
			p.makeToken(TOKEN_CARET)
			return
		case '+':
			p.makeToken(TOKEN_PLUS)
			return
		case '-':
			p.makeToken(TOKEN_MINUS)
			return
		case '~':
			p.makeToken(TOKEN_TILDE)
			return
		case '?':
			p.makeToken(TOKEN_QUESTION)
			return
		case '|':
			p.makeToken(p.twoCharToken('|', TOKEN_PIPE, TOKEN_PIPE_PIPE))
			return
		case '&':
			p.makeToken(p.twoCharToken('&', TOKEN_AMP, TOKEN_AMPAMP))
			return
		case '=':
			p.makeToken(p.twoCharToken('=', TOKEN_EQ, TOKEN_EQEQ))
			return
		case '!':
			p.makeToken(p.twoCharToken('=', TOKEN_BANG, TOKEN_BANGEQ))
			return
		case '.':
			if p.matchChar('.') {
				p.makeToken(p.twoCharToken('.', TOKEN_DOTDOT, TOKEN_DOTDOTDOT))
				return
			}

			p.makeToken(TOKEN_DOT)
			return
		case '/':
			p.makeToken(TOKEN_SLASH)
			return
		case '<':
			if p.matchChar('<') {
				p.makeToken(TOKEN_LTLT)
			} else {
				p.makeToken(p.twoCharToken('=', TOKEN_LT, TOKEN_LTEQ))
			}
			return
		case '>':
			if p.matchChar('>') {
				p.makeToken(TOKEN_GTGT)
			} else {
				p.makeToken(p.twoCharToken('=', TOKEN_GT, TOKEN_GTEQ))
			}
			return
		case '\n':
			p.makeToken(TOKEN_LINE)
			return
		case ' ':
		case '\r':
		case '\t':

			for p.peekChar() == ' ' ||
				p.peekChar() == '\r' ||
				p.peekChar() == '\t' {
				p.nextChar()
			}
			break
		case '"':
			p.readString()
			return
		case '0':
			if p.peekChar() == 'x' {
				p.readHexNumber()
				return
			}
			p.readNumber()
			return
		default:
			if p.currentLine == 1 && c == '#' && p.peekChar() == '!' {
				p.skipLineComment()
				break
			}
			if isName(c) {
				p.readName(TOKEN_NAME)
			} else if isDigit(c) {
				p.readNumber()
			}
			return
		}
	}

	p.next.Type = TOKEN_EOF
	p.next.Content = ""
}

type Precedence int

const (
	PREC_NONE Precedence = iota
	PREC_LOWEST
	PREC_EQUALITY
	PREC_IS
	PREC_COMPARISION
	PREC_BITWISE_OR
	PREC_BITWISE_XOR
	PREC_BITWISE_AND
	PREC_BITWISE_SHIFT
	PREC_RANGE
	PREC_TERM
	PREC_FACTOR
	PREC_UNARY
	PREC_CALL
)

func (p *Parser) literal() {
	switch p.previous.Type {
	case TOKEN_NUMBER:
		p.out.Write([]byte("\"number"))
	case TOKEN_STRING:
		p.out.Write([]byte("\"string"))
	}

	p.out.Write([]byte("  "))
	p.out.Write([]byte(p.previous.Content))
	p.out.Write([]byte("\""))
}

func (p *Parser) name() {
	p.out.Write([]byte("\""))
	p.out.Write([]byte(p.previous.Content))
	p.out.Write([]byte("\""))
}

func (p *Parser) list() {
	p.out.Write([]byte("\"list  [\"\t"))

	defer func() {
		p.out.Write([]byte("\"]\""))
	}()

	if p.match(TOKEN_RIGHT_BRACKET) {
		return
	}

	p.expression()
	for p.match(TOKEN_COMMA) {
		p.out.Write([]byte("\t"))

		if p.peek() == TOKEN_RIGHT_BRACKET {
			break
		}
		p.expression()

	}

	p.consume(TOKEN_RIGHT_BRACKET, "Expect ']' after list elements.")

	p.out.Write([]byte("\t"))

}

func (p *Parser) finishArgumentList() {

	if p.peek() == TOKEN_RIGHT_BRACKET {
		return
	}

	p.expression()

	for p.match(TOKEN_COMMA) {
		if p.peek() == TOKEN_RIGHT_BRACKET {
			break
		}
		p.expression()
	}

}

func (p *Parser) subscript() {
	p.out.Write([]byte("\t\"subscript  [\"\t"))

	defer func() {
		p.out.Write([]byte("\t"))
		p.out.Write([]byte("\"]\""))
	}()

	p.finishArgumentList()

	p.consume(TOKEN_RIGHT_BRACKET, "Expect ']' after arguments")

}


func (p *Parser) mapp() {
	p.out.Write([]byte("\"map  {\""))

	defer func() {
		p.out.Write([]byte("\t\"}\""))
	}()

	if p.match(TOKEN_RIGHT_BRACE) {
		return
	}

	p.out.Write([]byte("\t"))

	p.parsePrecedence(PREC_UNARY)

	p.out.Write([]byte("\t\":\"\t"))
	p.consume(TOKEN_COLON, "Expect ':' after map key")

	p.expression()

	for p.match(TOKEN_COMMA) {
		p.out.Write([]byte("\t\",\"\t"))

		if p.peek() == TOKEN_RIGHT_BRACE {
			break
		}

		p.parsePrecedence(PREC_UNARY)
		p.out.Write([]byte("\t\":\"\t"))

		p.consume(TOKEN_COLON, "Expect ':' after map key")

		p.expression()
	}

	p.consume(TOKEN_RIGHT_BRACE, "Expect '}' after map entries.")
}

func (p *Parser) boolean() {
	p.out.Write([]byte("\"" + p.previous.Content + "\""))
}

func (p *Parser) grouping() {
	p.out.Write([]byte("\"grouping"))
	p.out.Write([]byte("  (\"\t"))

	defer func() {
		p.out.Write([]byte("\t)"))
	}()

	p.expression()
	p.consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression")
}

func (p *Parser) unaryOp() {
	//rule := p.rules[p.previous.Type]
	p.out.Write([]byte("\"" + p.previous.Content + "\"\t"))

	p.parsePrecedence(PREC_UNARY + 1)
}

func (p *Parser) infixOp() {
	p.out.Write([]byte("\t\"" + p.previous.Content + "\"\t"))

	rule := p.rules[p.previous.Type]

	p.parsePrecedence(rule.precedence + 1)
}

func (p *Parser) parsePrecedence(prec Precedence) {
	p.nextToken()

	prefix := p.rules[p.previous.Type].prefix

	if prefix == nil {
		log.Fatalf("Expected Expression %s", p.previous.Type)
	}

	prefix()
	for prec <= p.rules[p.current.Type].precedence {
		p.nextToken()
		infix := p.rules[p.previous.Type].infix
		infix()
	}
	//  prefix = parser.prefix[p.current.Type]
	//  if prefix == NULL {
	//    		log.Fatalf("No prefix rule for Token %s", p.current.Type)
	// }
	// for
	//
	//
	//
}

func (p *Parser) expression() {
	p.parsePrecedence(PREC_LOWEST)

}

func (vm *WrenVM) wrenCompile(module string, source string, isExpr bool, printErrors bool) {
	var parser Parser

	parser.vm = vm
	parser.module = module
	parser.source = source

	parser.rdOffset = 0
	parser.offset = 0

	parser.printErrors = printErrors
	parser.hasErrors = false

	parser.nextToken()
}

func (vm *WrenVM) compileSource(module string, source string, isExpr bool, printErrors bool) {
	vm.wrenCompile(module, source, isExpr, printErrors)
}

func (vm *WrenVM) Interpret(module string, source string) {
	vm.compileSource(module, source, false, true)
}

type WrenInterpretResult int

const (
	WrenResultSuccess WrenInterpretResult = iota
	WrenResultCompileError
	WrenResultRuntimeError
)

type WrenErrorType int

const (
	WREN_ERROR_COMPILER WrenErrorType = iota
	WREN_ERROR_RUNTIME
)

type wrenResolveModuleFn func(vm *WrenVM, importer string, name string)

type loadModuleFn func(vm *WrenVM, name string)

type writeFn func(vm *WrenVM, message string)

type errorFn func(vm *WrenVM, errorType WrenErrorType, module string, line int, message string)

type WrenConfiguration struct {
	WrenResolveModuleFn wrenResolveModuleFn
	WrenLoadModuleFn    loadModuleFn
	WrenWriteFn         writeFn
	WrenErrorFn         errorFn
}

func WrenInitConfiguration(config *WrenConfiguration) {
}

func WrenFreeVM(vm *WrenVM) {

}

func WrenNewVM(config *WrenConfiguration) *WrenVM {

	if config != nil {

	}

	return &WrenVM{}
}
