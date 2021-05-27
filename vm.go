package wren

import (
	"bytes"
	"log"
)

const MaxInterpolationNesting = 8

var FALSE_VAL = BoolValue{Type: VAL_FALSE, value: false}
var TRUE_VAL = BoolValue{Type: VAL_TRUE, value: true}
var NULL_VAL = NullValue{Type: VAL_NULL}

type WrenVM struct {
	stack []Value
	sp    int
}

func NewWrenVM() *WrenVM {
	return &WrenVM{
		stack: []Value{},
		sp:    0,
	}
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

func (vm *WrenVM) wrenCompile(module string, source string, isExpr bool, printErrors bool) *Compiler {
	var parser Parser

	parser.vm = vm
	parser.module = module
	parser.source = source

	parser.rdOffset = 0
	parser.offset = 0

	parser.printErrors = printErrors
	parser.hasErrors = false

	parser.nextToken()
	parser.nextToken()

	var out = new(bytes.Buffer)
	parser.out = out

	compiler := NewCompiler(&parser)

	if isExpr {
		compiler.expression()
		compiler.consume(TOKEN_EOF, "Expect End of Expression got %s")
	}

	compiler.emitOp(CODE_RETURN)
	compiler.emitOp(CODE_END)

	return compiler

}

func (vm *WrenVM) compileSource(module string, source string, isExpr bool, printErrors bool) *Compiler {
	return vm.wrenCompile(module, source, isExpr, printErrors)
}

func (vm *WrenVM) runInterpreter(compiler *Compiler) {
	for ip := 0; ip < len(compiler.instructions); ip++ {
		code := Code(compiler.instructions[ip])

		switch code {
		case CODE_CONSTANT:
			ip += 2
			index := uint8(compiler.instructions[ip-2] <<8 | compiler.instructions[ip-1])
			vm.push(compiler.constants[index])
		case CODE_TRUE:
			vm.push(TRUE_VAL)
		case CODE_FALSE:
			vm.push(FALSE_VAL)
		case CODE_NULL:
			vm.push(NULL_VAL)
		case CODE_RETURN:
			vm.pop()
		case CODE_END:
		}
	}
}

func (vm *WrenVM) Interpret(module string, source string) {
	compiler := vm.compileSource(module, source, true, true)

	vm.runInterpreter(compiler)
}

func (vm *WrenVM) push(value Value) {
	vm.stack = append(vm.stack, value)
	vm.sp++
}

func (vm *WrenVM) pop() Value {
	vm.sp--
	return vm.stack[vm.sp]
}

func (vm *WrenVM) StackTop() Value {
	return vm.stack[vm.sp]
	if vm.sp == 0 {
		log.Fatalf("No stack top")
	}
	return vm.stack[vm.sp-1]
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
