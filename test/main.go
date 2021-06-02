package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"

	"github.com/jossephus/wren"
)

func handle_args(args []string) int {
	if len(args) < 2 {
		fmt.Printf("This is a wren test runner.\n Usage: wren_test [file]\n")
		return -1
	}

	return 0
}

var vm *wren.WrenVM

func init() {
	initVM(vm)
}

func main() {
	handled := handle_args(os.Args)

	if handled != 0 {
		return
	}

	filename := os.Args[1]

	content, err := readFile(filename)

	if err != nil {
		log.Fatalf("Could not open file %s\n", filename)
	}

	vm := wren.NewWrenVM()

	vm.Interpret("", content, false)

	value := vm.StackTop()

	fmt.Printf("%s",value.Print())

	/*
		handled := handle_args(os.Args)

		if handled != 0 {
			return
		}

		testName := os.Args[1]
		var result wren.WrenInterpretResult = runFile(vm, testName)

		if result == wren.WrenResultCompileError {
		} else if result == wren.WrenResultRuntimeError {

		}

		wren.WrenFreeVM(vm)
	*/
}

func initVM(vm *wren.WrenVM) *wren.WrenVM {
	var config = new(wren.WrenConfiguration)
	wren.WrenInitConfiguration(config)

	config.WrenResolveModuleFn = resolveModule
	config.WrenLoadModuleFn = readModule
	config.WrenWriteFn = vm_write
	config.WrenErrorFn = reportError

	return wren.WrenNewVM(config)
}

func readFile(filename string) (string, error) {
	content, err := ioutil.ReadFile(filename)

	return string(content), err
}
func runFile(vm *wren.WrenVM, filename string) wren.WrenInterpretResult {
	content, err := readFile(filename)

	fmt.Printf("Content of file %s", content)

	if err != nil {
		log.Fatalf("Could not open file %s\n", filename)
	}

	module := NewPath(filename)

	if module.Type() == PATH_TYPE_SIMPLE {
		relative := NewPath(".")
		//relative.Join(module)

		module = relative
	}

	module.removeExtension()

	var result wren.WrenInterpretResult = vm.Interpret(module.String(), content, false)

	return result

}
