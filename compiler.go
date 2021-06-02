package wren

import (
	"bytes"
	"log"
)

type Local struct {
	name string

	depth int

	isUpvalue bool
}

type ClassInfo struct {
	name ObjectValue

	fields *SymbolTable

	methods       []int
	staticMethods []int

	isForeign bool
	inStatic  bool

	signature *Signature
}

type Compiler struct {
	parser *Parser
	//constants    []Value
	//instructions Instructions
	parent *Compiler

	rules map[TokenType]GrammarRule
	fn    *ObjFn

	scopeDepth int

	loop *Loop

	numLocals int
	numSlots  int
	locals    []Local

	enclosingClass *ClassInfo
}

const MAX_PARAMETERS = 16
const MAX_VARIABLE_NAME = 64
const MAX_METHOD_NAME = 64
const MAX_LOCALS = 256

type SigType uint

const (
	SIG_METHOD SigType = iota
	SIG_SUBSCRIPT
	SIG_GETTER
	SIG_INITIALIZER
	SIG_SUBSCRIPT_SETTER
	SIG_SETTER
)

type Signature struct {
	Name   string
	Arity  int
	Length int
	Type   SigType
}

type SignatureFn func(signature *Signature)

type Scope uint

const (
	SCOPE_MODULE Scope = iota
	SCOPE_LOCAL
	SCOPE_UPVALUE
)

type Variable struct {
	index int

	scope Scope
}

func NewCompiler(parser *Parser, isMethod bool) *Compiler {
	c := &Compiler{
		parser: parser,
		//constants:    []Value{},
		rules: make(map[TokenType]GrammarRule),
		//instructions: Instructions{},
		fn: &ObjFn{
			module: parser.module,
		},

		loop:      nil,
		numLocals: 0,
		locals:    make([]Local, MAX_LOCALS),

		scopeDepth: -1,
	}

	c.numLocals = 1
	c.parser.vm.compiler = c

	if isMethod {
		c.locals[0].name = "this"
	} else {
		c.locals[0].name = ""
	}

	c.locals[0].depth = -1
	c.locals[0].isUpvalue = false

	c.rules[TOKEN_NUMBER] = GrammarRule{c.literal, nil, PREC_NONE}
	c.rules[TOKEN_STRING] = GrammarRule{c.literal, nil, PREC_NONE}
	c.rules[TOKEN_TRUE] = GrammarRule{c.boolean, nil, PREC_NONE}
	c.rules[TOKEN_FALSE] = GrammarRule{c.boolean, nil, PREC_NONE}
	c.rules[TOKEN_NULL] = GrammarRule{c.null, nil, PREC_NONE}
	c.rules[TOKEN_MINUS] = GrammarRule{c.unaryOp, c.infixOp, PREC_TERM}
	c.rules[TOKEN_PLUS] = GrammarRule{nil, c.infixOp, PREC_TERM}
	c.rules[TOKEN_SLASH] = GrammarRule{nil, c.infixOp, PREC_FACTOR}
	c.rules[TOKEN_STAR] = GrammarRule{nil, c.infixOp, PREC_FACTOR}

	c.rules[TOKEN_LT] = GrammarRule{nil, c.infixOp, PREC_COMPARISION}
	c.rules[TOKEN_GT] = GrammarRule{nil, c.infixOp, PREC_COMPARISION}
	c.rules[TOKEN_LTEQ] = GrammarRule{nil, c.infixOp, PREC_COMPARISION}
	c.rules[TOKEN_GTEQ] = GrammarRule{nil, c.infixOp, PREC_COMPARISION}

	c.rules[TOKEN_EQEQ] = GrammarRule{nil, c.infixOp, PREC_EQUALITY}
	c.rules[TOKEN_BANGEQ] = GrammarRule{nil, c.infixOp, PREC_EQUALITY}

	c.rules[TOKEN_AMP] = GrammarRule{nil, c.infixOp, PREC_BITWISE_AND}

	c.rules[TOKEN_LTLT] = GrammarRule{nil, c.infixOp, PREC_BITWISE_SHIFT}
	c.rules[TOKEN_GTGT] = GrammarRule{nil, c.infixOp, PREC_BITWISE_SHIFT}
	c.rules[TOKEN_PIPE] = GrammarRule{nil, c.infixOp, PREC_BITWISE_OR}
	c.rules[TOKEN_CARET] = GrammarRule{nil, c.infixOp, PREC_BITWISE_XOR}

	c.rules[TOKEN_PERCENT] = GrammarRule{nil, c.infixOp, PREC_FACTOR}
	c.rules[TOKEN_TILDE] = GrammarRule{c.unaryOp, nil, PREC_NONE}

	c.rules[TOKEN_DOT] = GrammarRule{nil, c.call, PREC_CALL}
	c.rules[TOKEN_DOTDOT] = GrammarRule{nil, c.infixOp, PREC_RANGE}
	c.rules[TOKEN_DOTDOTDOT] = GrammarRule{nil, c.infixOp, PREC_RANGE}

	c.rules[TOKEN_NAME] = GrammarRule{c.name, nil, PREC_NONE}
	c.rules[TOKEN_BANG] = GrammarRule{c.unaryOp, nil, PREC_NONE}

	c.rules[TOKEN_LEFT_PAREN] = GrammarRule{c.grouping, nil, PREC_NONE}
	c.rules[TOKEN_LEFT_BRACKET] = GrammarRule{c.list, c.subscript, PREC_CALL}

	return c
}

func (c *Compiler) consume(typ TokenType, err string) {
	c.parser.nextToken()

	if c.parser.previous.Type != typ {
		log.Fatalf("Error in consume: %q %s", err, c.parser.previous.Type)
		log.Fatalf("Expected %s. got %s", typ, c.parser.previous.Type)
	}
}

func (c *Compiler) consumeLine(err string) {
	c.consume(TOKEN_LINE, err)
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

func (c *Compiler) signatureFromToken(typ SigType) *Signature {
	var signature = &Signature{}

	token := c.parser.previous
	signature.Name = token.Content
	signature.Type = typ
	signature.Arity = 0

	if len(signature.Name) > MAX_METHOD_NAME {
		log.Fatalf("Method names cannot be longer than %d characters.", MAX_METHOD_NAME)
	}

	return signature
}

func (c *Compiler) methodCall(instruction Code, signature *Signature) {
	called := Signature{
		Name:  signature.Name,
		Type:  SIG_GETTER,
		Arity: 0,
	}

	if c.match(TOKEN_LEFT_PAREN) {
		called.Type = SIG_METHOD

		if c.peek() != TOKEN_RIGHT_PAREN {
			c.finishArgumentList(&called)
		}
		c.consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments")
	}

	if c.match(TOKEN_LEFT_BRACE) {
		log.Fatalf("c.methodCall, treating it like a function ")
	}

	c.callSignature(instruction, &called)
}

func (c *Compiler) namedCall(instruction Code) {
	signature := c.signatureFromToken(SIG_GETTER)

	if c.match(TOKEN_EQ) {
		log.Fatalf("c.namedCall. setter")
	} else {
		c.methodCall(instruction, signature)
	}
}

func (c *Compiler) call() {
	c.parser.out.Write([]byte("\t\"call  "))

	c.consume(TOKEN_NAME, "Expect method name after '.'.")

	//name := c.parser.previous.Content

	c.namedCall(CODE_CALL_0)
	/*
		c.callMethod(0, name, len(name))

		c.parser.out.Write([]byte(c.parser.previous.Content + "\""))
	*/
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

func (c *Compiler) resolveLocal(name string) int {
	for i := c.numLocals - 1; i >= 0; i-- {
		if c.locals[i].name == name {
			return i
		}
	}
	return -1
}

func (c *Compiler) addUpvalue(isLocal bool, index int) int {
	log.Fatalf("In c.addUpvalue ")
	return 0
}

func (c *Compiler) findUpValue(name string) int {
	if c.parent == nil {
		return -1
	}

	local := c.parent.resolveLocal(name)

	if local != -1 {
		c.parent.locals[local].isUpvalue = true
		return c.addUpvalue(true, local)
	}

	upvalue := c.parent.findUpValue(name)

	if upvalue != -1 {
		return c.addUpvalue(false, upvalue)
	}

	return -1
}

func (c *Compiler) resolveNonmodule(name string) Variable {
	var variable Variable

	variable.scope = SCOPE_LOCAL
	variable.index = c.resolveLocal(name)

	if variable.index != -1 {
		return variable
	}

	variable.scope = SCOPE_UPVALUE
	variable.index = c.findUpValue(name)

	return variable
}

func (c *Compiler) loadLocal(slot int) {
	if slot <= 8 {
		c.emitOp(Code(int(CODE_LOAD_LOCAL_0) + slot))
		return
	}
	c.emitByteArg(CODE_LOAD_LOCAL, slot)
}

func (c *Compiler) loadVariable(variable Variable) {
	switch variable.scope {
	case SCOPE_MODULE:
		c.emitShortArg(CODE_LOAD_MODULE_VAR, variable.index)
	case SCOPE_LOCAL:
		c.loadLocal(variable.index)
	default:
		log.Fatalf("c.loadVariable unhandled scope")
	}
}

func (c *Compiler) bareName(variable Variable) {
	if c.match(TOKEN_EQ) {
		c.expression()

		switch variable.scope {
		case SCOPE_LOCAL:
			c.emitByteArg(CODE_STORE_LOCAL, variable.index)
		case SCOPE_MODULE:
			c.emitShortArg(CODE_STORE_MODULE_VAR, variable.index)
		default:
			log.Fatalf("c.bareName unhandled scope ")
		}
		return
	}
	c.loadVariable(variable)
}

func (c *Compiler) name() {
	c.parser.out.Write([]byte("\""))
	c.parser.out.Write([]byte(c.parser.previous.Content))
	c.parser.out.Write([]byte("\""))

	token := c.parser.previous

	variable := c.resolveNonmodule(token.Content)

	if variable.index != -1 {
		c.bareName(variable)
		return
	}

	variable.scope = SCOPE_MODULE

	value, ok := c.parser.module.VariableNames[token.Content]

	if !ok {
		variable.index = c.parser.vm.declareVariable(c.parser.module, token.Content, token.Line)

		if variable.index == -2 {
			log.Fatalf("c.name too many module variables defined.")
		}
	} else {
		variable.index = value.index
	}

	c.bareName(variable)
}

func (c *Compiler) grouping() {
	c.parser.out.Write([]byte("\"grouping"))
	c.parser.out.Write([]byte("  (\"\t"))
	c.expression()

	defer func() {
		c.parser.out.Write([]byte("\t)"))
	}()

	c.consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression")
}

func (c *Compiler) methodSymbol(name string, length int) int {
	//existing, ok := c.parser.vm.methodNames[name]
	sym, ok := c.parser.vm.methodNames.Resolve(name)

	if ok {
		return sym.Index
	}
	c.parser.vm.functionNames = append(c.parser.vm.functionNames, name)

	sym = c.parser.vm.methodNames.Define(name)
	return sym.Index
}

func (c *Compiler) callMethod(numArgs int, name string, length int) {
	symbol := c.methodSymbol(name, length)
	c.emitShortArg(Code(int(CODE_CALL_0)+numArgs), symbol)
}

func (c *Compiler) unaryOp() {
	c.parser.out.Write([]byte("\"" + c.parser.previous.Content + "\"\t"))

	name := c.parser.previous.Content

	c.parsePrecedence(PREC_UNARY + 1)

	c.callMethod(0, name, 1)
}

func (c *Compiler) signatureParameterList(buf *bytes.Buffer, name string, numParams int, left, right byte) {
	buf.WriteByte(left)

	for i := 0; i < numParams && i < MAX_PARAMETERS; i++ {
		if i > 0 {
			buf.WriteByte(',')
		}
		buf.WriteByte('_')
	}

	buf.WriteByte(right)
}

func (c *Compiler) signatureToString(signature *Signature) string {
	var buf bytes.Buffer

	buf.WriteString(signature.Name)

	switch signature.Type {
	case SIG_METHOD:
		buf.WriteByte('(')
		for i := 0; i < signature.Arity && i < MAX_PARAMETERS; i++ {
			if i > 0 {
				buf.WriteByte(',')
			}
			buf.WriteByte('_')
		}
		buf.WriteByte(')')
	case SIG_SUBSCRIPT:
		c.signatureParameterList(&buf, signature.Name, signature.Arity, '[', ']')
	case SIG_GETTER:
	default:
		log.Fatalf("compiler.signatureToString with type unhandled %t", signature.Type == SIG_GETTER)
	}

	return buf.String()

}

func (c *Compiler) signatureSymbol(signature *Signature) int {
	name := c.signatureToString(signature)

	return c.methodSymbol(name, len(name))
}

func (c *Compiler) callSignature(instruction Code, signature *Signature) {
	symbol := c.signatureSymbol(signature)

	c.emitShortArg((Code(int(instruction) + signature.Arity)), symbol)

	if instruction == CODE_SUPER_0 {
		log.Fatalf("compiler.callSignature instruction is CODE_SUPER_0")
	}
}

func (c *Compiler) infixOp() {
	c.parser.out.Write([]byte("\t\"" + c.parser.previous.Content + "\"\t"))

	name := c.parser.previous.Content
	//rule := c.parser.rules[c.parser.previous.Type]
	rule := c.rules[c.parser.previous.Type]

	c.parsePrecedence(rule.precedence + 1)

	signature := &Signature{
		Name:   name,
		Length: len(name),
		Type:   SIG_METHOD,
		Arity:  1,
	}

	c.callSignature(CODE_CALL_0, signature)
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

func (c *Compiler) validateNumParameters(numArgs int) {
	if numArgs == MAX_PARAMETERS+1 {
		log.Fatalf("exceeded from maximum number of arguments ")
	}
}

func (c *Compiler) finishArgumentList(sig *Signature) {

	sig.Arity++
	c.validateNumParameters(sig.Arity)
	c.expression()

	for c.parser.match(TOKEN_COMMA) {
		sig.Arity++
		c.validateNumParameters(sig.Arity)

		c.expression()
	}
}

func (c *Compiler) subscript() {
	c.parser.out.Write([]byte("\t\"subscript  [\"\t"))

	defer func() {
		c.parser.out.Write([]byte("\t"))
		c.parser.out.Write([]byte("\"]\""))
	}()

	signature := &Signature{
		Name:   "",
		Arity:  0,
		Length: 0,
		Type:   SIG_SUBSCRIPT,
	}

	c.finishArgumentList(signature)
	c.callSignature(CODE_CALL_0, signature)

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
	c.fn.constants = append(c.fn.constants, constant)
	return len(c.fn.constants) - 1
	//c.constants = append(c.constants, constant)
	//return len(c.constants) - 1
}

func (c *Compiler) emitByte(byt int) int {
	//length := len(c.instructions)
	length := len(c.fn.instructions)
	c.fn.instructions = append(c.fn.instructions, byt)

	return length
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

func (c *Compiler) emitByteArg(instruction Code, arg int) int {
	c.emitOp(instruction)
	return c.emitByte(arg)
}

func (c *Compiler) emitConstant(value Value) {
	constant := c.addConstant(value)
	c.emitShortArg(CODE_CONSTANT, constant)
}

func (c *Compiler) endCompiler(debugName string, debugNameLength int) *ObjFn {
	if c.parser.hasError {
		//log.Fatalf("In compiler.go=>endCompiler")
		return nil

	}

	c.emitOp(CODE_END)

	c.parser.vm.functionBindName(c.fn, debugName)

	if c.parent != nil {
		constant := c.parent.addConstant(ObjectValue{
			Type: VAL_OBJ,
			Obj:  c.fn,
		})

		c.parent.emitShortArg(CODE_CLOSURE, constant)
		/*
			for i := 0; i < c.fn.numUpvalues; i++ {
				log.Fatalf("some numupalues in endCompiler")
			}
		*/
	}

	c.parser.vm.compiler = c.parent

	return c.fn
}

func (c *Compiler) peek() TokenType {
	return c.parser.current.Type
}

func (c *Compiler) match(typ TokenType) bool {
	return c.parser.match(typ)
}
func (c *Compiler) matchLine() bool {
	if !c.parser.match(TOKEN_LINE) {
		return false
	}

	for c.parser.match(TOKEN_LINE) {
	}
	return true
}

func (c *Compiler) addLocal(name string) int {
	/*var local Local

	local.name = name
	local.depth = c.scopeDepth

	c.locals[c.numLocals] = local
	c.numLocals++
	return c.numLocals - 1
	*/

	local := &c.locals[c.numLocals]
	local.name = name
	local.depth = c.scopeDepth

	numLocals := c.numLocals
	c.numLocals++
	return numLocals

}

func (c *Compiler) declareVariable(token *Token) int {
	if token == nil {
		token = &c.parser.previous
	}
	if len(token.Content) > MAX_VARIABLE_NAME {
		log.Fatalf("Max Variable name")
	}

	if c.scopeDepth == -1 {
		line := -1

		symbol := c.parser.vm.defineVariable(c.parser.module, token.Content, NULL_VAL, &line)

		switch symbol {
		case -1:
			log.Fatalf("Module variable is already defined.")
		case -2:
			log.Fatalf("Too many module variables defined")
		case -3:
			log.Fatalf("Variable %s refernced before this definition ( first use at line %d). ", token.Content, line)
		}

		return symbol
	}

	for i := c.numLocals - 1; i >= 0; i-- {
		local := c.locals[i]

		if local.depth < c.scopeDepth {
			break
		}

		if local.name == token.Content {
			log.Fatalf("Variable is already defined in this scope ")
			return i
		}
	}

	if c.numLocals == MAX_LOCALS {
		log.Fatalf("Cannot declare more than %d local variables", MAX_LOCALS)
	}

	return c.addLocal(token.Content)

	return 0
}

func (c *Compiler) defineVariable(symbol int) {
	if c.scopeDepth >= 0 {
		return
	}

	c.emitShortArg(CODE_STORE_MODULE_VAR, symbol)
	c.emitOp(CODE_POP)
}

func (c *Compiler) variableDefinition() {
	c.consume(TOKEN_NAME, "Expect variable name")
	nameToken := c.parser.previous

	if c.match(TOKEN_EQ) {
		c.expression()
	} else {
		c.null()
	}

	symbol := c.declareVariable(&nameToken)
	c.defineVariable(symbol)
}

func (c *Compiler) emitJump(instruction Code) int {
	c.emitOp(instruction)
	ind := len(c.fn.instructions)

	c.emitByte(0xff)
	c.emitByte(0xff)

	return ind
}

func (c *Compiler) patchJump(offset int) {
	jump := len(c.fn.instructions) - offset - 2

	c.fn.instructions[offset] = (jump >> 8) & 0xff
	c.fn.instructions[offset+1] = jump & 0xff
}

func (c *Compiler) ifStatement() {
	c.consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.")
	c.expression()
	c.consume(TOKEN_RIGHT_PAREN, "Expect ')' after if condition")

	ifJump := c.emitJump(CODE_JUMP_IF)

	c.statement()

	if c.match(TOKEN_ELSE) {
		elseJump := c.emitJump(CODE_JUMP)

		c.patchJump(ifJump)

		c.statement()

		c.patchJump(elseJump)
	} else {
		c.patchJump(ifJump)
	}
}

type Loop struct {
	start int

	exitJump int

	body int

	scopeDepth int

	enclosing *Loop
}

func (c *Compiler) startLoop(loop *Loop) {
	loop.enclosing = c.loop
	loop.start = len(c.fn.instructions) - 1
	loop.scopeDepth = c.scopeDepth
	c.loop = loop
}

func (c *Compiler) testExitLoop() {
	c.loop.exitJump = c.emitJump(CODE_JUMP_IF)
}

func (c *Compiler) loopBody() {
	c.loop.body = len(c.fn.instructions)
	c.statement()
}

func getByteCountForArguments(instruction Instructions, constants []Value, ip int) int {
	code := instruction[ip]

	switch Code(code) {
	case CODE_NULL, CODE_FALSE, CODE_TRUE, CODE_POP, CODE_RETURN, CODE_END:
		return 0
	case CODE_LOAD_MODULE_VAR, CODE_STORE_MODULE_VAR, CODE_LOOP, CODE_CONSTANT,
		CODE_CALL_0, CODE_CALL_1, CODE_JUMP_IF:
		return 2
	default:
		log.Fatalf("we do not know how much u take %d", code)
	}

	return 0
}

func (c *Compiler) endLoop() {
	loopOffset := len(c.fn.instructions) - c.loop.start + 2
	c.emitShortArg(CODE_LOOP, loopOffset)

	c.patchJump(c.loop.exitJump)

	i := c.loop.body

	for i < len(c.fn.instructions)-1 {
		if Code(c.fn.instructions[i]) == CODE_END {
			c.fn.instructions[i] = int(CODE_JUMP)
			c.patchJump(i + 1)
			i += 3
		} else {
			i += 1 + getByteCountForArguments(c.fn.instructions, c.fn.constants, i)
		}
	}

	c.loop = c.loop.enclosing
}

func (c *Compiler) whileStatement() {
	var loop Loop
	c.startLoop(&loop)

	c.consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.")
	c.expression()
	c.consume(TOKEN_RIGHT_PAREN, "Expect ')' after while condition")

	c.testExitLoop()
	c.loopBody()
	c.endLoop()
}

func (c *Compiler) pushScope() {
	c.scopeDepth++
}

func (c *Compiler) finishBlock() bool {
	if c.match(TOKEN_RIGHT_BRACE) {
		return false
	}

	if !c.matchLine() {
		c.expression()
		c.consume(TOKEN_RIGHT_BRACE, "Expect '}' at end of block")
		return true
	}

	if c.match(TOKEN_RIGHT_BRACE) {
		return false
	}

	for c.peek() != TOKEN_RIGHT_BRACE && c.peek() != TOKEN_EOF {
		c.definition()
		c.consumeLine("Expect newline after statement")
	}

	c.consume(TOKEN_RIGHT_BRACE, "Expect '}' at end of block")
	return false
}

func (c *Compiler) discardLocals(depth int) int {
	local := c.numLocals - 1

	for local >= 0 && c.locals[local].depth >= depth {
		c.emitByte(int(CODE_POP))
		local--
	}

	return c.numLocals - local - 1
}

func (c *Compiler) popScope() {
	popped := c.discardLocals(c.scopeDepth)
	c.numLocals -= popped
	c.numSlots -= popped
	c.scopeDepth--
}

func (c *Compiler) statement() {
	if c.match(TOKEN_BREAK) {
		if c.loop == nil {
			log.Fatalf("Cannot use 'break' outside of a loop")
			return
		}

		c.discardLocals(c.loop.scopeDepth + 1)

		c.emitJump(CODE_END)

	} else if c.match(TOKEN_CONTINUE) {

		if c.loop == nil {
			log.Fatalf("Cannot use 'conttinue' outside of a loop")
			return
		}

		c.discardLocals(c.loop.scopeDepth + 1)

		loopOffset := len(c.fn.instructions) - c.loop.start + 2

		c.emitShortArg(CODE_LOOP, loopOffset)
	} else if c.match(TOKEN_IF) {
		c.ifStatement()
	} else if c.match(TOKEN_WHILE) {
		c.whileStatement()
	} else if c.match(TOKEN_LEFT_BRACE) {
		c.pushScope()

		if c.finishBlock() {
			c.emitOp(CODE_POP)
		}

		c.popScope()
	} else {
		c.expression()
		c.emitOp(CODE_POP)
	}
}

func (c *Compiler) declareNamedVariable() int {
	c.consume(TOKEN_NAME, "Expect variable name ")
	return c.declareVariable(nil)
}

func (c *Compiler) loadCoreVariable(name string) {
	symbol, _ := c.parser.module.VariableNames[name]
	c.emitShortArg(CODE_LOAD_MODULE_VAR, symbol.index)
}

func (c *Compiler) declareMethod(signature *Signature, name string) int {
	symbol := c.signatureSymbol(signature)

	classInfo := c.enclosingClass

	var methods []int

	if classInfo.inStatic {
		methods = classInfo.staticMethods
	} else {
		methods = classInfo.methods
	}

	for i := 0; i < len(methods); i++ {
		if methods[i] == symbol {
			log.Fatalf("c.declareMethod")
		}
	}

	classInfo.methods = append(classInfo.methods, symbol)
	return symbol
}

func (c *Compiler) defineMethod(classVar Variable, isStatic bool, methodSymbol int) {

	c.loadVariable(classVar)

	var instruction Code
	if isStatic {
		instruction = CODE_METHOD_STATIC
	} else {
		instruction = CODE_METHOD_INSTANCE
	}

	c.emitShortArg(instruction, methodSymbol)
}

func (c *Compiler) finishBody(isInitializer bool) {
	isExpr := c.finishBlock()

	if isInitializer {
		if isExpr {
			c.emitOp(CODE_POP)
		}
		c.emitOp(CODE_LOAD_LOCAL_0)
	} else if !isExpr {
		c.emitOp(CODE_NULL)
	}
	c.emitOp(CODE_RETURN)
}

func (c *Compiler) finishParameterList(sig *Signature) {
	sig.Arity++
	c.validateNumParameters(sig.Arity)
	c.declareNamedVariable()

	for c.parser.match(TOKEN_COMMA) {
		sig.Arity++
		c.validateNumParameters(sig.Arity)

		c.declareNamedVariable()
	}
}

func (c *Compiler) mayBeSetter(signature *Signature) bool {
	if !c.match(TOKEN_EQ) {
		return false
	}

	if signature.Type == SIG_SUBSCRIPT {
		signature.Type = SIG_SUBSCRIPT_SETTER
	} else {
		signature.Type = SIG_SETTER
	}

	c.consume(TOKEN_LEFT_PAREN, "Expect '(' after '='.")
	c.declareNamedVariable()
	c.consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameter name")

	signature.Arity++
	return true
}

func (c *Compiler) parameterList(signature *Signature) {
	if !c.match(TOKEN_LEFT_PAREN) {
		return
	}
	signature.Type = SIG_METHOD

	if c.match(TOKEN_RIGHT_PAREN) {
		return
	}

	c.finishParameterList(signature)
	c.consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters")
}

func (c *Compiler) namedSignature(signature *Signature) {
	signature.Type = SIG_GETTER

	if c.mayBeSetter(signature) {
		return
	}

	c.parameterList(signature)
}

func (c *Compiler) method(classVar Variable) bool {
	isForeign := c.match(TOKEN_FOREIGN)
	isStatic := c.match(TOKEN_STATIC)

	c.enclosingClass.inStatic = isStatic

	var signatureFn SignatureFn
	signatureFn = c.namedSignature

	c.parser.nextToken()

	if signatureFn == nil {
		log.Fatalf("Expected method definition")
	}

	signature := c.signatureFromToken(SIG_GETTER)

	c.enclosingClass.signature = signature

	var methodCompiler = NewCompiler(c.parser, false)
	methodCompiler.parent = c

	signatureFn(signature)

	fullSignature := c.signatureToString(signature)

	methodSymbol := c.declareMethod(signature, fullSignature)

	if isForeign {
		log.Fatalf("Hello c.method isForeign")
	} else {
		c.consume(TOKEN_LEFT_BRACE, "Expect '{' to begin method body.")

		methodCompiler.finishBody(signature.Type == SIG_INITIALIZER)
		methodCompiler.endCompiler(fullSignature, len(fullSignature))
		c.emitOp(CODE_END)
	}
	c.defineMethod(classVar, isStatic, methodSymbol)

	if signature.Type == SIG_INITIALIZER {
		log.Fatalf("hello world")
	}

	return true
}

func (c *Compiler) defineObjectClass() {
	log.Printf("Let's add Object to our variable names in classDefinition.")
	var line int
	c.emitConstant(ObjectValue{
		Type: VAL_OBJ,
		Obj: &ObjClass{
			name:    "Object",
			methods: make(map[string]*Method),
		},
	})
	c.emitShortArg(CODE_STORE_MODULE_VAR, 0)
	c.parser.vm.defineVariable(c.parser.module, "Object", NULL_VAL, &line)
}

func (c *Compiler) classDefinition(isForeign bool) {
	var line int
	c.parser.vm.defineVariable(c.parser.module, "Object", NULL_VAL, &line)

	var classVar Variable

	if c.scopeDepth == -1 {
		classVar.scope = SCOPE_MODULE
	} else {
		classVar.scope = SCOPE_LOCAL
	}

	classVar.index = c.declareNamedVariable()

	className := ObjectValue{
		Type: VAL_OBJ,
		Obj: StringObject{
			Type:  OBJ_STRING,
			Value: c.parser.previous.Content,
		},
	}

	c.emitConstant(className)

	if c.match(TOKEN_IS) {
		log.Fatalf("There is an is in class Definition")
	} else {
		c.loadCoreVariable("Object")
	}

	numFieldsInstruction := -1

	if isForeign {
		log.Fatalf("class is foreign")
	} else {
		numFieldsInstruction = c.emitByteArg(CODE_CLASS, 255)
	}

	c.defineVariable(classVar.index)

	c.pushScope()

	var classInfo ClassInfo

	classInfo.isForeign = isForeign
	classInfo.name = className

	classInfo.fields = NewSymbolTable()

	classInfo.methods = []int{}
	classInfo.staticMethods = []int{}

	c.enclosingClass = &classInfo

	c.consume(TOKEN_LEFT_BRACE, "Expect '{' after class declaration")
	c.matchLine()

	for !c.match(TOKEN_RIGHT_BRACE) {
		if !c.method(classVar) {
			break
		}

		if c.match(TOKEN_RIGHT_BRACE) {
			break
		}

		c.consumeLine("Expect newline after definition in class")
	}
	//	c.emitOp(CODE_END)
	//	log.Fatalf(c.fn.instructions.String(c.fn))

	if !isForeign {
		c.fn.instructions[numFieldsInstruction] = classInfo.fields.numDefinitions
	}

	c.enclosingClass = nil
	c.popScope()
}

func (c *Compiler) definition() {
	if c.match(TOKEN_CLASS) {
		c.classDefinition(false)
	} else if c.match(TOKEN_VAR) {
		c.variableDefinition()
	} else {
		c.statement()
	}
}
