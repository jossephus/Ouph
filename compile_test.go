package wren

import "testing"

func TestCompileLiterals(t *testing.T) {
	for _, tt := range []struct{
		src string
		expected string
	}{
		{"1", `Constant: 0	1.000000
Return
End
`},
		{`"hello"`, `Constant: 0	hello
Return
End
`},
}{
		vm := &WrenVM{}

		compiler := vm.wrenCompile("", tt.src, true, false)

		if compiler.instructions.String(compiler) != tt.expected {
			t.Errorf("Compile[%q]: expected: `%s`; got `%s`", tt.src, tt.expected, compiler.instructions.String(compiler))
		}
	}
}

func TestCompileStrings(t *testing.T) {
	src := `"hello world"`

	vm := &WrenVM{}

	compiler := vm.wrenCompile("", src, true, false)

	expected := `Constant: 0	hello world
Return
End
`
	if compiler.instructions.String(compiler) != expected {
		t.Errorf("Compile[%q]: expected: %q, got %q", src, expected, compiler.instructions.String(compiler))
	}
}

func TestCompileBooleans(t *testing.T) {
	for _, tt := range []struct {
		src      string
		expected string
	}{
		{"true", `True
Return
End
`},
		{"false", `False
Return
End
`},
	} {
		vm := &WrenVM{}

		compiler := vm.wrenCompile("", tt.src, true, false)

		if compiler.instructions.String(compiler) != tt.expected {
			t.Errorf("Compile[%q]: expected: %q, got %q", tt.src, tt.expected, compiler.instructions.String(compiler))
		}
	}
}

func TestCompileNull(t *testing.T) {
	src := "null"

	expected := `Null
Return
End
`

	vm := &WrenVM{}

	compiler := vm.wrenCompile("", src, true, false)

	if compiler.instructions.String(compiler) != expected {
		t.Errorf("Compile[%q]: expected: %q, got %q", src, expected, compiler.instructions.String(compiler))
	}
}
