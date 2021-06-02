package wren

import (
	"bytes"
	"fmt"
	"log"
)

type Code int

const (
	CODE_RETURN Code = iota
	CODE_CONSTANT
	CODE_TRUE
	CODE_FALSE
	CODE_END

	CODE_STORE_MODULE_VAR
	CODE_LOAD_MODULE_VAR

	CODE_POP

	CODE_CALL_0
	CODE_CALL_1

	CODE_SUPER_0

	CODE_NULL

	CODE_JUMP_IF
	CODE_JUMP

	CODE_STORE_LOCAL

	CODE_LOAD_LOCAL
	CODE_LOAD_LOCAL_0
	CODE_LOAD_LOCAL_1
	CODE_LOAD_LOCAL_2

	CODE_LOOP
	CODE_CLASS

	CODE_METHOD_STATIC
	CODE_METHOD_INSTANCE

	CODE_CLOSURE
)

type Instructions []int

func (ins Instructions) String(compiler *ObjFn) string {

	i := 0

	var buf bytes.Buffer

	lastLine := -1

	for {
		offset := ins.dumpInstruction(&buf, i, lastLine, compiler)

		if offset == -1 {
			break
		}

		i += offset
	}

	return buf.String()
	return "Instruction string"
}

func (ins Instructions) dumpInstruction(buf *bytes.Buffer, i int, lastLine int, compiler *ObjFn) int {
	bytecode := Code(ins[i])
	start := i

	i++

	switch bytecode {
	case CODE_CONSTANT:
		i += 2
		constant := (ins[i-2]<<8 | ins[i-1])
		if constant == 16 {
			log.Fatalf("it is 16")
		}
		fmt.Fprintf(buf, "Constant: %d\t", constant)
		fmt.Fprintf(buf, compiler.constants[constant].Print()+"\n")
	case CODE_RETURN:
		fmt.Fprintf(buf, "Return\n")
	case CODE_END:
		fmt.Fprintf(buf, "End\n")
	case CODE_TRUE:
		fmt.Fprintf(buf, "True\n")
	case CODE_FALSE:
		fmt.Fprintf(buf, "False\n")
	case CODE_NULL:
		fmt.Fprintf(buf, "Null\n")
	case CODE_POP:
		fmt.Fprint(buf, "Pop\n")
	case CODE_CALL_0, CODE_CALL_1:
		numArgs := ins[i-1] - int(CODE_CALL_0)
		i += 2
		symbol := (ins[i-2]<<8 | ins[i-1])
		fmt.Fprintf(buf, "CALL_%d    %d\n", numArgs, symbol)
	case CODE_STORE_MODULE_VAR:
		i += 2
		slot := (ins[i-2]<<8 | ins[i-1])
		fmt.Fprintf(buf, "%-16s %5d\n", "STORE_MODULE_VAR", slot)
	case CODE_LOAD_MODULE_VAR:
		i += 2
		slot := (ins[i-2]<<8 | ins[i-1])
		fmt.Fprintf(buf, "%-16s %5d\n", "LOAD_MODULE_VAR", slot)
	case CODE_SUPER_0:
		log.Fatalf("are we serving this?")
	case CODE_JUMP_IF:
		i += 2
		offset := (ins[i-2]<<8 | ins[i-1])
		fmt.Fprintf(buf, "%-16s %5d to %d\n", "JUMP_IF", offset, i+offset)
	case CODE_LOOP:
		i += 2
		offset := (ins[i-2]<<8 | ins[i-1])
		fmt.Fprintf(buf, "%-16s %5d to %d\n", "LOOP", offset, i-offset)
	case CODE_STORE_LOCAL:
		offs := ins[i]
		i++
		fmt.Fprintf(buf, "%-16s %5d\n", "CODE_STORE_LOCAL", offs)
	case CODE_LOAD_LOCAL:
		offs := ins[i]
		i++
		fmt.Fprintf(buf, "%-16s %5d\n", "LOAD_LOCAL", offs)
	case CODE_CLOSURE:
		i += 2
		constant := (ins[i-2]<<8 | ins[i-1])
		fmt.Fprintf(buf, "%-16s %5d ", "CLOSURE", constant)
		fmt.Fprintf(buf, "%s", compiler.constants[constant].Print())
		fmt.Fprintf(buf, "\n")

		fn := compiler.constants[constant].(ObjectValue).Obj.(*ObjFn)

		for i := 0; i < fn.numUpvalues; i++ {
			log.Fatalf("c.dumpInstruction numUpvalues ")
		}
	case CODE_LOAD_LOCAL_0:
		fmt.Fprintf(buf, "LOAD_LOCAL_0\n")
	case CODE_LOAD_LOCAL_1:
		fmt.Fprintf(buf, "LOAD_LOCAL_1\n")
	case CODE_CLASS:
		numFields := ins[i]
		i++
		fmt.Fprintf(buf, "%-16s %5d fields\n", "CLASS", numFields)
	case CODE_METHOD_INSTANCE:
		i += 2
		symbol := ins[i-2]<<8 | ins[i-1]
		fmt.Fprintf(buf, "%-16s %5d", "METHOD_INSTANCE", symbol)
	default:
		fmt.Fprintf(buf, "Unhandled Opcode in dumpInstruction %d", bytecode)
	}

	if bytecode == CODE_END {
		return -1
	}

	return i - start

}
