package main

import (
	"fmt"

	"github.com/jossephus/wren"
)

func resolveModule(vm *wren.WrenVM, importer string, module string) {

}

func readModule(vm *wren.WrenVM, module string) {

}

func reportError(vm *wren.WrenVM, typ wren.WrenErrorType, module string, line int, message string) {
	switch typ {
	case wren.WREN_ERROR_COMPILER:
		fmt.Printf("[%s line %d] %s\n", module, line, message)
	case wren.WREN_ERROR_RUNTIME:
		fmt.Printf("[%s]\n", message)
	}
}

func vm_write(vm *wren.WrenVM, message string) {
	fmt.Printf("%s", message)
}
