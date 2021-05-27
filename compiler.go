package wren

import "log"

type Compiler struct {
	parser       *Parser
	constants    []Value
	instructions Instructions

	rules map[TokenType]GrammarRule
}

func NewCompiler(parser *Parser) *Compiler {
	c := &Compiler{
		parser:       parser,
		constants:    []Value{},
		rules:        make(map[TokenType]GrammarRule),
		instructions: Instructions{},
	}
	c.rules[TOKEN_NUMBER] = GrammarRule{c.literal, nil, PREC_NONE}
	c.rules[TOKEN_STRING] = GrammarRule{c.literal, nil, PREC_NONE}
	c.rules[TOKEN_TRUE] = GrammarRule{c.boolean, nil, PREC_NONE}
	c.rules[TOKEN_FALSE] = GrammarRule{c.boolean, nil, PREC_NONE}
	c.rules[TOKEN_NULL] = GrammarRule{c.null, nil, PREC_NONE}

	return c
}

func (c *Compiler) consume(typ TokenType, err string) {
	c.parser.nextToken()

	if c.parser.previous.Type != typ {
		log.Fatalf("Error in consume: %q %s", err, c.parser.previous.Type)
		log.Fatalf("Expected %s. got %s", typ, c.parser.previous.Type)
	}
}

func (c *Compiler) literal() {
	switch c.parser.previous.Type {
	case TOKEN_NUMBER:
		c.parser.out.Write([]byte("\"number"))
	case TOKEN_STRING:
		c.parser.out.Write([]byte("\"string"))
	}

	c.parser.out.Write([]byte("  "))
	c.parser.out.Write([]byte(c.parser.previous.Content))
	c.parser.out.Write([]byte("\""))

	c.emitConstant(c.parser.previous.value)
}

func (c *Compiler) boolean() {
	c.parser.out.Write([]byte("\"" + c.parser.previous.Content + "\""))

	if c.parser.previous.Type == TOKEN_FALSE {
		c.emitOp(CODE_FALSE)
	} else {
		c.emitOp(CODE_TRUE)
	}
}

func (c *Compiler) null() {
	c.emitOp(CODE_NULL)
}

func (c *Compiler) name() {
	c.parser.out.Write([]byte("\""))
	c.parser.out.Write([]byte(c.parser.previous.Content))
	c.parser.out.Write([]byte("\""))
}

func (c *Compiler) grouping() {
	c.parser.out.Write([]byte("\"grouping"))
	c.parser.out.Write([]byte("  (\"\t"))

	defer func() {
		c.parser.out.Write([]byte("\t)"))
	}()

	c.expression()
	c.consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression")
}

func (c *Compiler) unaryOp() {
	c.parser.out.Write([]byte("\"" + c.parser.previous.Content + "\"\t"))

	c.parsePrecedence(PREC_UNARY + 1)
}

func (c *Compiler) infixOp() {
	c.parser.out.Write([]byte("\t\"" + c.parser.previous.Content + "\"\t"))

	//rule := c.parser.rules[c.parser.previous.Type]
	rule := c.rules[c.parser.previous.Type]

	c.parsePrecedence(rule.precedence + 1)
}

func (c *Compiler) list() {
	c.parser.out.Write([]byte("\"list  [\"\t"))

	defer func() {
		c.parser.out.Write([]byte("\"]\""))
	}()

	if c.parser.match(TOKEN_RIGHT_BRACKET) {
		return
	}

	c.expression()
	for c.parser.match(TOKEN_COMMA) {
		c.parser.out.Write([]byte("\t"))

		if c.parser.peek() == TOKEN_RIGHT_BRACKET {
			break
		}
		c.expression()

	}

	c.consume(TOKEN_RIGHT_BRACKET, "Expect ']' after list elements.")

	c.parser.out.Write([]byte("\t"))
}

func (c *Compiler) finishArgumentList() {
	if c.parser.peek() == TOKEN_RIGHT_BRACKET {
		return
	}

	c.expression()

	for c.parser.match(TOKEN_COMMA) {
		if c.parser.peek() == TOKEN_RIGHT_BRACKET {
			return
		}

		c.expression()
	}
}

func (c *Compiler) subscript() {
	c.parser.out.Write([]byte("\t\"subscript  [\"\t"))

	defer func() {
		c.parser.out.Write([]byte("\t"))
		c.parser.out.Write([]byte("\"]\""))
	}()

	c.finishArgumentList()

	c.consume(TOKEN_RIGHT_BRACKET, "Expect ']' after arguments")
}

func (c *Compiler) mapp() {
	c.parser.out.Write([]byte("\"map  {\""))

	defer func() {
		c.parser.out.Write([]byte("\t\"}\""))
	}()

	if c.parser.match(TOKEN_RIGHT_BRACE) {
		return
	}

	c.parser.out.Write([]byte("\t"))

	c.parsePrecedence(PREC_UNARY)

	c.parser.out.Write([]byte("\t\":\"\t"))
	c.consume(TOKEN_COLON, "Expect ':' after map key")

	c.expression()

	for c.parser.match(TOKEN_COMMA) {
		c.parser.out.Write([]byte("\t\",\"\t"))

		if c.parser.peek() == TOKEN_RIGHT_BRACE {
			break
		}

		c.parsePrecedence(PREC_UNARY)
		c.parser.out.Write([]byte("\t\":\"\t"))

		c.consume(TOKEN_COLON, "Expect ':' after map key")

		c.expression()
	}

	c.consume(TOKEN_RIGHT_BRACE, "Expect '}' after map entries.")
}

func (c *Compiler) parsePrecedence(prec Precedence) {
	c.parser.nextToken()

	//prefix := c.parser.rules[c.parser.previous.Type].prefix
	prefix := c.rules[c.parser.previous.Type].prefix

	if prefix == nil {
		log.Fatalf("Expected Expression %s", c.parser.previous.Type)
	}

	prefix()

	//for prec <= c.parser.rules[c.parser.current.Type].precedence {
	for prec <= c.rules[c.parser.current.Type].precedence {
		c.parser.nextToken()

		//infix := c.parser.rules[c.parser.previous.Type].infix
		//infix := c.parser.rules[c.parser.previous.Type].infix
		infix := c.rules[c.parser.previous.Type].infix

		infix()
	}
}

func (c *Compiler) expression() {
	c.parsePrecedence(PREC_LOWEST)
}

func (c *Compiler) addConstant(constant Value) int {
	c.constants = append(c.constants, constant)
	return len(c.constants) - 1
}

func (c *Compiler) emitByte(byt int) int {
	length := len(c.instructions)

	c.instructions = append(c.instructions, byt)

	return length - 1
}

func (c *Compiler) emitOp(instruction Code) {
	c.emitByte(int(instruction))
}

func (c *Compiler) emitShort(arg int) {
	c.emitByte(arg >> 8 & 0xff)
	c.emitByte(arg & 0xff)
}

func (c *Compiler) emitShortArg(instruction Code, arg int) {
	c.emitOp(instruction)
	c.emitShort(arg)
}

func (c *Compiler) emitConstant(value Value) {
	constant := c.addConstant(value)
	c.emitShortArg(CODE_CONSTANT, constant)
}
