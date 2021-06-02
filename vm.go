package wren

import (
	"bytes"
	"fmt"
	"log"
	"math"
	"strings"
)

const MaxInterpolationNesting = 8

const MAX_MODULE_VARS = 65536

var FALSE_VAL = BoolValue{Type: VAL_FALSE, value: false}
var TRUE_VAL = BoolValue{Type: VAL_TRUE, value: true}
var NULL_VAL = NullValue{Type: VAL_NULL}

type WrenVM struct {
	stack []Value
	sp    int

	modules map[Value]Value

	methodNames   *SymbolTable
	functionNames []string

	fiber *ObjFiber

	compiler *Compiler

	boolClass   *ObjClass
	nullClass   *ObjClass
	numClass    *ObjClass
	stringClass *ObjClass
	objectClass *ObjClass
}

func NewWrenVM() *WrenVM {
	return &WrenVM{
		stack:         []Value{},
		sp:            0,
		modules:       make(map[Value]Value),
		methodNames:   NewSymbolTable(),
		functionNames: []string{},
		nullClass: &ObjClass{
			methods: map[string]*Method{
				"!": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						vm.push(TRUE_VAL)

						return true
					},
				},
			},
		},
		objectClass: &ObjClass{
			methods: map[string]*Method{
				"name": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						vm.push(
							ObjectValue{
								Type: VAL_OBJ,
								Obj: StringObject{
									Type: OBJ_STRING,
									//Value: args[0].(ObjectValue).Obj.(StringObject).Value + args[1].(ObjectValue).Obj.(StringObject).Value,
									Value: args[0].(ObjectValue).Obj.(*ObjClass).name,
								},
							},
						)

						return true
					},
				},
			},
		},
		boolClass: &ObjClass{
			methods: map[string]*Method{
				"==(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						val := args[0].(BoolValue).value

						val2 := args[1].(BoolValue).value

						if val == val2 {
							vm.push(TRUE_VAL)
						} else {
							vm.push(FALSE_VAL)
						}
						return true

					},
				},
				"!": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						val := args[0].(BoolValue).value

						if val {
							vm.push(FALSE_VAL)
						} else {
							vm.push(TRUE_VAL)
						}

						return true
					},
				},
			},
		},
		stringClass: &ObjClass{
			methods: map[string]*Method{
				"+(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						switch args[1].ValueType() {
						case VAL_OBJ:
							_, ok := args[1].(ObjectValue).Obj.(StringObject)

							if !ok {
								log.Fatalf("trying to add string to another object")
							}
						default:
							log.Fatalf("trying to add  string to something else ")
							return false
						}

						vm.push(
							ObjectValue{
								Type: VAL_OBJ,
								Obj: StringObject{
									Type:  OBJ_STRING,
									Value: args[0].(ObjectValue).Obj.(StringObject).Value + args[1].(ObjectValue).Obj.(StringObject).Value,
								},
							},
						)
						return true
					},
				},
				"[_]": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						if args[1].ValueType() != VAL_NUM {
							log.Fatalf("using non number to index string ")
						}

						vm.push(
							ObjectValue{
								Type: VAL_OBJ,
								Obj: StringObject{
									Type:  OBJ_STRING,
									Value: fmt.Sprintf("%c", args[0].(ObjectValue).Obj.(StringObject).Value[int(args[1].(NumValue).Number)]),
								},
							},
						)
						return true

					},
				},
				"contains(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						if args[1].ValueType() != VAL_OBJ {
							log.Fatalf("strings contains non integer number")
						}
						finder := args[0].(ObjectValue).Obj.(StringObject).Value
						val := args[1].(ObjectValue).Obj.(StringObject).Value

						if strings.Contains(finder, val) {
							vm.push(TRUE_VAL)
						} else {
							vm.push(FALSE_VAL)
						}

						return true
					},
				},
				"print": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1
						fmt.Printf("%v", args[0].Print())
						return true
					},
				},
			},
		},
		numClass: &ObjClass{
			methods: map[string]*Method{
				"-": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1
						switch args[0].ValueType() {
						case VAL_NUM:
							vm.push(
								NumValue{
									Type:   VAL_NUM,
									Number: -(args[0].(NumValue).Number),
								},
							)
						default:
							log.Fatalf("It is not a number ")

						}
						return true
					},
				},
				"~": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						switch args[0].ValueType() {
						case VAL_NUM:
							vm.push(
								NumValue{
									Type:   VAL_NUM,
									Number: float64(^(uint32((int(args[0].(NumValue).Number))))),
								},
							)
						default:
							log.Fatalf("~ing non number")
							return false
						}

						return true
					},
				},
				"+(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						switch args[1].ValueType() {
						case VAL_NUM:
						default:
							log.Fatalf("trying to add integer to something else ")
							return false
						}

						vm.push(
							NumValue{
								Type:   VAL_NUM,
								Number: args[0].(NumValue).Number + args[1].(NumValue).Number,
							},
						)

						return true
					},
				},
				"-(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						switch args[1].ValueType() {
						case VAL_NUM:
						default:
							log.Fatalf("trying to sub integer to something else ")
							return false
						}

						vm.push(
							NumValue{
								Type:   VAL_NUM,
								Number: args[0].(NumValue).Number - args[1].(NumValue).Number,
							},
						)

						return true
					},
				},
				"*(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						switch args[1].ValueType() {
						case VAL_NUM:
						default:
							log.Fatalf("trying to mul number to something else ")
							return false
						}

						vm.push(
							NumValue{
								Type:   VAL_NUM,
								Number: args[0].(NumValue).Number * args[1].(NumValue).Number,
							},
						)
						return true
					},
				},

				"/(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {

						vm.fiber.stackTop -= len(args) - 1

						switch args[1].ValueType() {
						case VAL_NUM:
						default:
							log.Fatalf("trying to divide number to something else ")
							return false
						}

						vm.push(
							NumValue{
								Type:   VAL_NUM,
								Number: args[0].(NumValue).Number / args[1].(NumValue).Number,
							},
						)

						return true
					},
				},
				"<(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						switch args[1].ValueType() {
						case VAL_NUM:
						default:
							log.Fatalf("comparing integer to non integer ")
							return false
						}

						if args[0].(NumValue).Number < args[1].(NumValue).Number {
							vm.push(TRUE_VAL)
						} else {
							vm.push(FALSE_VAL)
						}

						return true
					},
				},
				">(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						switch args[1].ValueType() {
						case VAL_NUM:
						default:
							log.Fatalf("comparing integer to non integer ")
							return false
						}

						if args[0].(NumValue).Number > args[1].(NumValue).Number {
							vm.push(TRUE_VAL)
						} else {
							vm.push(FALSE_VAL)
						}

						return true
					},
				},
				">=(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						switch args[1].ValueType() {
						case VAL_NUM:
						default:
							log.Fatalf("comparing integer to non integer ")
							return false
						}

						if args[0].(NumValue).Number >= args[1].(NumValue).Number {
							vm.push(TRUE_VAL)
						} else {
							vm.push(FALSE_VAL)
						}

						return true
					},
				},
				"<=(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						switch args[1].ValueType() {
						case VAL_NUM:
						default:
							log.Fatalf("comparing integer to non integer ")
							return false
						}

						if args[0].(NumValue).Number <= args[1].(NumValue).Number {
							vm.push(TRUE_VAL)
						} else {
							vm.push(FALSE_VAL)
						}

						return true
					},
				},
				"==(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						switch args[1].ValueType() {
						case VAL_NUM:
						default:
							log.Fatalf("comparing == integet to non intege ")
							return false
						}

						if args[0].(NumValue).Number == args[1].(NumValue).Number {
							vm.push(TRUE_VAL)
						} else {
							vm.push(FALSE_VAL)
						}

						return true
					},
				},
				"!=(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						switch args[1].ValueType() {
						case VAL_NUM:
						default:
							log.Fatalf("comparing == integet to non intege ")
							return false
						}

						if args[0].(NumValue).Number != args[1].(NumValue).Number {
							vm.push(TRUE_VAL)
						} else {
							vm.push(FALSE_VAL)
						}

						return true
					},
				},
				"&(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						switch args[1].ValueType() {
						case VAL_NUM:
						default:
							log.Fatalf("&ing integer to non integer ")
							return false
						}

						vm.push(
							NumValue{
								Type:   VAL_NUM,
								Number: float64(int(args[0].(NumValue).Number) & int(args[1].(NumValue).Number)),
							},
						)

						return true
					},
				},
				"^(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						switch args[1].ValueType() {
						case VAL_NUM:
						default:
							log.Fatalf("&ing integer to non integer ")
							return false
						}

						vm.push(
							NumValue{
								Type:   VAL_NUM,
								Number: float64(int(args[0].(NumValue).Number) ^ int(args[1].(NumValue).Number)),
							},
						)

						return true
					},
				},
				"|(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						switch args[1].ValueType() {
						case VAL_NUM:
						default:
							log.Fatalf("&ing integer to non integer ")
							return false
						}

						vm.push(
							NumValue{
								Type:   VAL_NUM,
								Number: float64(int(args[0].(NumValue).Number) | int(args[1].(NumValue).Number)),
							},
						)

						return true
					},
				},
				"<<(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						switch args[1].ValueType() {
						case VAL_NUM:
						default:
							log.Fatalf("&ing integer to non integer ")
							return false
						}

						vm.push(
							NumValue{
								Type:   VAL_NUM,
								Number: float64(int(args[0].(NumValue).Number) << int(args[1].(NumValue).Number)),
							},
						)

						return true
					},
				},
				">>(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						switch args[1].ValueType() {
						case VAL_NUM:
						default:
							log.Fatalf("&ing integer to non integer ")
							return false
						}

						vm.push(
							NumValue{
								Type:   VAL_NUM,
								Number: float64(int(args[0].(NumValue).Number) >> int(args[1].(NumValue).Number)),
							},
						)

						return true
					},
				},
				"%(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						switch args[1].ValueType() {
						case VAL_NUM:
						default:
							log.Fatalf("&ing integer to non integer ")
							return false
						}

						vm.push(
							NumValue{
								Type:   VAL_NUM,
								Number: float64(int(args[0].(NumValue).Number) % int(args[1].(NumValue).Number)),
								//Number: args[0].(NumValue).Number % args[1].(NumValue).Number,
							},
						)

						return true
					},
				},
				"..(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						switch args[1].ValueType() {
						case VAL_NUM:
						default:
							log.Fatalf("&ing integer to non integer ")
							return false
						}

						from := args[0].(NumValue).Number
						to := args[1].(NumValue).Number

						vm.push(
							ObjectValue{
								Type: VAL_OBJ,
								Obj:  NewRange(vm, from, to, true),
							},
						)

						return true
					},
				},
				"...(_)": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						switch args[1].ValueType() {
						case VAL_NUM:
						default:
							log.Fatalf("&ing integer to non integer ")
							return false
						}

						from := args[0].(NumValue).Number
						to := args[1].(NumValue).Number

						vm.push(
							ObjectValue{
								Type: VAL_OBJ,
								Obj:  NewRange(vm, from, to, false),
							},
						)

						return true
					},
				},
				"floor": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						vm.push(
							NumValue{
								Type:   VAL_NUM,
								Number: math.Floor(args[0].(NumValue).Number),
								//Number: float64(^(uint32((int(args[0].(NumValue).Number))))),
							},
						)

						return true

					},
				},
				"ceil": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1

						vm.push(
							NumValue{
								Type:   VAL_NUM,
								Number: math.Ceil(args[0].(NumValue).Number),
							},
						)

						return true
					},
				},
				"print": &Method{
					Type: METHOD_PRIMITIVE,
					primitive: func(vm *WrenVM, args []Value) bool {
						vm.fiber.stackTop -= len(args) - 1
						fmt.Printf("%v\n", args[0].Print())
						return true
					},
				},
			},
		},
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

func (vm *WrenVM) wrenCompile(module *ObjModule, source string, isExpr bool, printErrors bool) *ObjFn {
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

	//	numExistingVariables := len(module.VariableNames)
	compiler := NewCompiler(&parser, false)

	if isExpr {
		compiler.expression()
		compiler.consume(TOKEN_EOF, "Expect End of Expression got %s")
	} else {

		for !compiler.match(TOKEN_EOF) {
			compiler.definition()

			if !compiler.matchLine() {
				compiler.consume(TOKEN_EOF, "Expect end of file ")
				break
			}
		}
	}

	compiler.emitOp(CODE_RETURN)

	//TODO:

	for name, demo := range parser.module.VariableNames {
		if demo.value.ValueType() == VAL_NUM {
			parser.previous.Type = TOKEN_NAME
			parser.previous.Content = name
			parser.previous.Line = int(demo.value.(NumValue).Number)
			log.Fatalf("Variable is used but not defined")
			parser.hasError = true
		}
	}

	return compiler.endCompiler("(script)", 0)
}

func (vm *WrenVM) newClosure(fn *ObjFn) *ObjClosure {
	closure := &ObjClosure{}

	closure.Type = OBJ_CLOSURE

	closure.fn = fn

	for i := 0; i < fn.numUpValues; i++ {
		closure.upvalues[i] = nil
	}

	return closure
}

/*
func (vm *WrenVM) getModule(name Value) *ObjModule {
	moduleValue, ok := vm.modules[name]

	if !ok {
		return nil
	}

	objValue, ok := name.(ObjectValue)

	if !ok {
		return nil
	}

	module, ok := objValue.Obj.(*ObjModule)

	//module, ok := moduleValue.ObjectValue.Obj.(*ObjModule)

	if !ok {
		return nil
	}

	return module
}
*/

func (vm *WrenVM) getModule(name Value) *ObjModule {
	val, ok := vm.modules[name]

	if !ok {
		return nil
	}

	if val.ValueType() != VAL_OBJ {
		return nil
	}

	module := val.(ObjectValue).Obj

	if module.ObjType() == OBJ_MODULE {
		return module.(*ObjModule)
	}

	return nil
}

func (vm *WrenVM) compileInModule(name Value, src string, isExpr bool, printErrors bool) *ObjClosure {
	module := vm.getModule(name)

	if module == nil {
		module = &ObjModule{
			Type: OBJ_MODULE,
			//VariableNames: make(map[StringObject]Value),
			VariableNames: make(map[string]Demo),
			//variables: []Value{},
			variables: make([]Value, 65535),
			Name:      name,
		}

		vm.modules[name] = ObjectValue{Type: VAL_OBJ, Obj: module}

		/*	coreModule := vm.getModule(NULL_VAL)

			for name, value := range coreModule.VariableNames {

			}
		*/
		//TODO://coreModule
		/*
			coreModule := vm.modules[NULL_VAL]

			for name, value := range coreModule.VariableNames {
				log.Fatalf("in vm.go compileInModule ")
				//vm.wrenDefineVariable(module, name, i
			}
		*/
	}
	fn := vm.wrenCompile(module, src, isExpr, printErrors)

	if fn == nil {
		return nil
	}

	closure := vm.newClosure(fn)

	return closure
}
func (vm *WrenVM) compileSource(module string, source string, isExpr bool, printErrors bool) *ObjClosure {
	var nameValue Value
	nameValue = NULL_VAL

	if module != "" {
		nameValue = ObjectValue{
			Type: VAL_OBJ,
			Obj: StringObject{
				Type:  OBJ_STRING,
				Value: module,
			},
		}
	}

	closure := vm.compileInModule(nameValue, source, isExpr, printErrors)

	return closure
	//return vm.wrenCompile(module, source, isExpr, printErrors)
}

func (vm *WrenVM) getClass(value Value) *ObjClass {
	if value.ValueType() == VAL_OBJ {
		return value.(ObjectValue).classObj
	}

	switch value.ValueType() {
	case VAL_NUM:
		return vm.numClass
	case VAL_OBJ:
		return value.(ObjectValue).classObj
	case VAL_FALSE:
		return vm.boolClass
	case VAL_TRUE:
		return vm.boolClass
	case VAL_NULL:
		return vm.nullClass
	default:
		log.Fatalf("vm.go getClass Default ")
	}
	return nil

}

func (vm *WrenVM) runInterpreter(fiber *ObjFiber) WrenInterpretResult {
	vm.fiber = fiber

	/*frame := fiber.frames[fiber.numFrames-1]
	stackStart := frame.stackStart
	ip := frame.ip
	fn := frame.closure.fn
	*/
	//log.Fatalf("%s", fn.instructions.String(fn))

	var frame *CallFrame
	var fn *ObjFn
	var ip int
	var stackStart int
	var code Code

	emittedEnds := 0

	for {
		//:for ip < len(fn.instructions) {
		frame = fiber.frames[fiber.numFrames-1]

		stackStart = frame.stackStart
		
		ip = frame.ip
		
		fn = frame.closure.fn

		code = Code(fn.instructions[ip])


		switch code {
		case CODE_CONSTANT:
			frame.ip += 2
			//index := int8(fn.instructions[ip-2]<<8 | fn.instructions[ip-1])
			index := (fn.instructions[frame.ip-1]<<8 | fn.instructions[frame.ip])
			vm.push(fn.constants[index])
		case CODE_RETURN:
			//vm.pop()
		case CODE_TRUE:
			vm.push(TRUE_VAL)
		case CODE_FALSE:
			vm.push(FALSE_VAL)
		case CODE_NULL:
			vm.push(NULL_VAL)
		case CODE_CALL_0, CODE_CALL_1:
			numArgs := fn.instructions[frame.ip] - int(CODE_CALL_0)
			frame.ip += 2
			symbol := fn.instructions[frame.ip-1]<<8 | fn.instructions[frame.ip]
			args := fiber.stack[fiber.stackTop-numArgs-1 : fiber.stackTop]

			classObj := vm.getClass(args[0])

			//if symbol >= len(classObj.methods) {
			//	log.Fatalf("CODE_CALL_0. error ")
			//}
			method, ok := classObj.methods[vm.functionNames[symbol]]

			log.Printf("%v", classObj.methods)
			log.Printf("%#v", vm.functionNames)
			log.Printf("%v", vm.functionNames[symbol])

			if !ok {
				log.Fatalf("CODE_CALL_0. error 2")
			}

			if method.Type == METHOD_NONE {
				log.Fatalf("CODE_CALL_0. error 3")
			}

			switch method.Type {
			case METHOD_PRIMITIVE:
				if method.primitive(vm, args) {

				}
			case METHOD_BLOCK:
				//old := frame.ip	
				fiber.frames[fiber.numFrames] = NewFrame(method.Closure, fiber.stackTop - numArgs)
				fiber.numFrames++
				vm.push(ObjectValue{
					Type: VAL_OBJ,
					Obj: classObj,
				})

			default:
				log.Fatalf("We here CODE_CALL_0. error 4")
			}
		case CODE_STORE_MODULE_VAR:
			frame.ip += 2
			index := (fn.instructions[frame.ip-1]<<8 | fn.instructions[frame.ip])
			fn.module.variables[index] = vm.peek()
		case CODE_LOAD_MODULE_VAR:
			frame.ip += 2
			index := (fn.instructions[frame.ip-1]<<8 | fn.instructions[frame.ip])
			vm.push(fn.module.variables[index])
		case CODE_POP:
			vm.drop()
		case CODE_END:
			if emittedEnds == 0 {
				return WrenResultSuccess
			}
			emittedEnds--
		case CODE_JUMP_IF:
			frame.ip += 2
			offset := fn.instructions[frame.ip-1]<<8 | fn.instructions[frame.ip]

			condition := vm.pop()

			if isFalse(condition) || isFalse(condition) {
				frame.ip += offset
			}
		case CODE_JUMP:
			frame.ip += 2
			offset := fn.instructions[frame.ip-1]<<8 | fn.instructions[frame.ip]

			frame.ip += offset
		case CODE_LOOP:
			frame.ip += 2
			offset := fn.instructions[frame.ip-1]<<8 | fn.instructions[frame.ip]
			frame.ip -= offset
		case CODE_CLASS:
			frame.ip += 1
			numFields := fn.instructions[frame.ip]
			vm.createClass(numFields, nil)
		case CODE_METHOD_STATIC, CODE_METHOD_INSTANCE:
			frame.ip += 2
			offset := fn.instructions[frame.ip-1]<<8 | fn.instructions[frame.ip]
			//log.Fatalf("offset %d", offset)
			//classObj := vm.peek().(ObjectValue).Obj.(*ObjClass)
			method := vm.peek2()

			vm.bindMethod(fn.instructions[frame.ip], code, offset, fn.module, vm.peek(), method)

			vm.drop()
			//vm.drop()
		case CODE_CLOSURE:
			emittedEnds++
			frame.ip += 2
			constant := fn.instructions[frame.ip-1]<<8 | fn.instructions[frame.ip]

			function := fn.constants[constant].(ObjectValue).Obj.(*ObjFn)
			closure := NewClosure(function)

			vm.push(ObjectValue{
				Type: VAL_OBJ,
				Obj:  closure,
			})

			for i := 0; i < function.numUpvalues; i++ {

			}
		case CODE_CONSTRUCT:
			if _, ok := fiber.stack[stackStart].(ObjectValue).Obj.(*ObjClass); !ok {
				log.Fatalf("not an object in top of stack. on CODE_CONSTRUCT")
			}

			fiber.stack[stackStart] = ObjectValue{
				Type: VAL_OBJ,
				Obj: NewInstance(fiber.stack[stackStart].(ObjectValue).Obj.(*ObjClass)),
				Object: Object{
					classObj: fiber.stack[stackStart].(ObjectValue).Obj.(*ObjClass),
				},
			}
		case CODE_LOAD_LOCAL_0:
			log.Fatalf("%#v", fiber.stack[int(code)-int(CODE_LOAD_LOCAL_0)])
		default:
			log.Fatalf("We are here in vm.go runInterpreter %v", code)
		}

		frame.ip++
	}

	return WrenResultRuntimeError
}

func (vm *WrenVM) callFunction(fiber *ObjFiber, closure *ObjClosure, numArgs int) {
	//frame := &fiber.frames[fiber.numFrames]
	//fiber.numFrames++
	//frame.stackStart = fiber.stackTop - numArgs
	//fame.closure = closure
}

func (vm *WrenVM) wrenBindMethod(classObj *ObjClass, symbol int, method *Method) {
	if classObj.methods == nil {
		classObj.methods = make(map[string]*Method)
	}
	classObj.methods[vm.functionNames[symbol]] = method
}

func (vm *WrenVM) bindMethodCode(classObj *ObjClass, fn *ObjFn) {
	ip := 0

	for {
		instruction := Code(fn.instructions[ip])
		switch instruction {
		case CODE_NULL, CODE_RETURN, CODE_LOAD_LOCAL_0, CODE_CALL_0, CODE_CONSTRUCT:
		case CODE_END:
			return
		default:
			log.Fatalf("vm.bindMethodCode instruction %d", instruction)
		}

		ip += 1 + getByteCountForArguments(fn.instructions, fn.constants, ip)
	}

}

func (vm *WrenVM) bindMethod(methodType int, code Code, symbol int, module *ObjModule, value Value, methodValue Value) {
	//className := classObj.name
	var classObj *ObjClass
	if code == CODE_METHOD_STATIC {
		classObj = value.(ObjectValue).classObj
	} else {
		classObj = value.(ObjectValue).Obj.(*ObjClass)
	}
	var method = &Method{}
	objVal, ok := methodValue.(ObjectValue)

	if !ok {
		log.Fatalf("method is not an object")
	}

	_, ok = objVal.Obj.(StringObject)

	if ok {
		log.Fatalf("method is a string. c.bindMethod")
	} else {
		method.Closure = objVal.Obj.(*ObjClosure)
		method.Type = METHOD_BLOCK

		vm.bindMethodCode(classObj, method.Closure.fn)
	}
	vm.wrenBindMethod(classObj, symbol, method)

}

func (vm *WrenVM) createClass(numFields int, module *ObjModule) {
	name := vm.fiber.stack[vm.fiber.stackTop-2]
	superclass := vm.fiber.stack[vm.fiber.stackTop-1]

	vm.fiber.stackTop--

	classObj := NewClass(superclass, numFields, name)

	vm.fiber.stack[vm.fiber.stackTop-1] = ObjectValue{
		Type: VAL_OBJ,
		Obj:  classObj,
		Object: Object{
			classObj: vm.objectClass,
		},
	}
	if numFields == -1 {
		log.Fatalf("vm.createClass, it is a foreign one ")
	}
}

func (vm *WrenVM) Interpret(module string, source string, isExpr bool) WrenInterpretResult {
	closure := vm.compileSource(module, source, isExpr, true)
	//closure := vm.compileSource(module, source, true, true)

	if closure == nil {
		//log.Fatalf("vm.Interpret closure is nil ")
		return WrenResultCompileError
	}

	fiber := NewFiber(vm, closure)

	return vm.runInterpreter(fiber)
}

func (vm *WrenVM) peek() Value {
	return vm.fiber.stack[vm.fiber.stackTop-1]
}

func (vm *WrenVM) peek2() Value {
	return vm.fiber.stack[vm.fiber.stackTop-2]
}

func (vm *WrenVM) push(value Value) {
	vm.fiber.stack[vm.fiber.stackTop] = value
	vm.fiber.stackTop++
}

func (vm *WrenVM) drop() {
	vm.fiber.stackTop--
}

func (vm *WrenVM) pop() Value {
	vm.fiber.stackTop--
	return vm.fiber.stack[vm.fiber.stackTop]
}

func (vm *WrenVM) StackTop() Value {
	return vm.fiber.stack[vm.fiber.stackTop]
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

func (vm *WrenVM) functionBindName(fn *ObjFn, name string) {
	//fn.debugName = name
}

type Symbol struct {
	Name  string
	Index int
}

type SymbolTable struct {
	Name string

	Store map[string]Symbol

	numDefinitions int
}

func NewSymbolTable() *SymbolTable {
	return &SymbolTable{
		Store:          make(map[string]Symbol),
		numDefinitions: 0,
	}
}

func (st *SymbolTable) Define(name string) Symbol {
	sym := Symbol{name, st.numDefinitions}
	st.Store[name] = sym
	st.numDefinitions++
	return sym
}

func (st *SymbolTable) Resolve(name string) (Symbol, bool) {
	sym, ok := st.Store[name]
	return sym, ok
}

func (vm *WrenVM) declareVariable(module *ObjModule, name string, line int) int {
	if len(module.VariableNames) == MAX_MODULE_VARS {
		return -2
	}

	module.VariableNames[name] = Demo{
		NumValue{
			Type:   VAL_NUM,
			Number: float64(line),
		},
		len(module.VariableNames),
	}
	return len(module.VariableNames) - 1
}

func (vm *WrenVM) defineVariable(module *ObjModule, name string, value Value, line *int) int {
	if len(module.VariableNames) == MAX_MODULE_VARS {
		return -2
	}

	_, ok := module.VariableNames[name]

	var symbol int

	// new variable name
	if !ok {
		module.VariableNames[name] = Demo{value, len(module.VariableNames)}
		symbol = len(module.VariableNames) - 1
		module.variables[symbol] = value

		//vm.fiber.fn.modules.variables[symbol] = value
		//module.variables[symbol] = value
		//   vm.fiber.frames[vm.fiber.numFrames - 1].closure.fn.module.variables[symbol] = value

		//	vm.modules.variables[symbol] = value
		//log.Fatalf("%v", symbol)
	} else {
		log.Fatalf("vm.defineVariable unknown output")
	}
	return symbol
}

func isFalse(value Value) bool {
	switch value.ValueType() {
	case VAL_TRUE, VAL_FALSE:
		return value.(BoolValue).value == false
	default:
		return false
	}
}
