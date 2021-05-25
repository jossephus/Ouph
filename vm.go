package wren

import (
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
