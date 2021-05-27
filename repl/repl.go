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

	for {
		fmt.Printf(">> ")

		scanned := scanner.Scan()

		if !scanned {
			break
		}

		line := scanner.Text()

		vm := wren.NewWrenVM()

		vm.Interpret("", line)

		value := vm.StackTop()

		fmt.Printf("%s", value.Print())

		fmt.Printf("\n")
	}
}
