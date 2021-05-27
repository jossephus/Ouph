package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	scanner := bufio.NewScanner(os.Stdin)

	for {
		scanned := scanner.Scan()

		if !scanned {
			break
		}

		line := scanner.Text()

		fmt.Printf("%s", line)
	}
}
