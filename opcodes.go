package wren

import (
	"bytes"
	"fmt"
)

type Code int

const (
	CODE_RETURN Code = iota
	CODE_CONSTANT
	CODE_TRUE
	CODE_FALSE
	CODE_END
	CODE_NULL
)

type Instructions []int

func (ins Instructions) String(compiler *Compiler) string {

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

func (ins Instructions) dumpInstruction(buf *bytes.Buffer, i int, lastLine int, compiler *Compiler) int {
	bytecode := Code(ins[i])
	start := i

	i++

	switch bytecode {
	case CODE_CONSTANT:
		i += 2
		constant := (ins[i-2]<<8 | ins[i-1])
		fmt.Fprintf(buf, "Constant: %d\t", constant)
		fmt.Fprintf(buf, compiler.constants[constant].Print() + "\n")
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
	default:
		fmt.Fprintf(buf, "Unhandled Opcode in dumpInstruction ")
	}

	if bytecode == CODE_END {
		return -1
	}

	return i - start

}
