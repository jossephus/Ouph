package main

import (
	"bufio"
	"fmt"
	"os"

	"github.com/jossephus/wren"
)

func main() {
	scanner := bufio.NewScanner(os.Stdin)

	fmt.Printf(`\\/"-
 \_/	wren	
 `)
	vm := wren.NewWrenVM()

	for {
		fmt.Printf(">> ")

		scanned := scanner.Scan()

		if !scanned {
			break
		}

		line := scanner.Text()

		result := vm.Interpret("", line, false)

		if result == wren.WrenResultCompileError {
			fmt.Printf("Compiler error\n")
			continue
		}

		value := vm.StackTop()

		fmt.Printf("%s", value.Print())

		fmt.Printf("\n")
	}
}
