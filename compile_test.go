package wren

import (
	"testing"
)

func TestCompileLiterals(t *testing.T) {
	for _, tt := range []struct {
		src      string
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
	} {
		vm := &WrenVM{}
		module := &ObjModule{}

		compiler := vm.wrenCompile(module, tt.src, true, false)

		if compiler.instructions.String(compiler) != tt.expected {
			t.Errorf("Compile[%q]: expected: `%s`; got `%s`", tt.src, tt.expected, compiler.instructions.String(compiler))
		}
	}
}

func TestCompileStrings(t *testing.T) {
	src := `"hello world"`

	vm := &WrenVM{}

	module := &ObjModule{}

	compiler := vm.wrenCompile(module, src, true, false)

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
		module := &ObjModule{}

		compiler := vm.wrenCompile(module, tt.src, true, false)

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
	module := &ObjModule{}

	compiler := vm.wrenCompile(module, src, true, false)

	if compiler.instructions.String(compiler) != expected {
		t.Errorf("Compile[%q]: expected: %q, got %q", src, expected, compiler.instructions.String(compiler))
	}
}

func TestCompilePrefixArthimethic(t *testing.T) {
	src := "-1"

	expected := `Constant: 0	1.000000
CALL_0    0
Return
End
`
	vm := NewWrenVM()

	module := &ObjModule{}

	compiler := vm.wrenCompile(module, src, true, false)

	if compiler.instructions.String(compiler) != expected {
		t.Errorf("Compile[%q]: expected: `%s`, got `%s`", src, expected, compiler.instructions.String(compiler))
	}
}

func TestCompileInfixArthimethic(t *testing.T) {
	src := "1 + 1"

	expected := `Constant: 0	1.000000
Constant: 1	1.000000
CALL_1    0
Return
End
`
	vm := NewWrenVM()

	module := &ObjModule{}

	compiler := vm.wrenCompile(module, src, true, false)

	if compiler.instructions.String(compiler) != expected {
		t.Errorf("Compile[%q]: expected: \n`%q`, \n\ngot \n`%q`", src, expected, compiler.instructions.String(compiler))
	}
}

func TestCompileCallExpressions(t *testing.T) {
	src := "1.floor"

	expected := `Constant: 0	1.000000
CALL_0    0
Return
End
`
	vm := NewWrenVM()

	module := &ObjModule{}

	compiler := vm.wrenCompile(module, src, true, false)

	if compiler.instructions.String(compiler) != expected {
		t.Errorf("Compile[%q]: expected: \n`%q`, \n\ngot \n`%q`", src, expected, compiler.instructions.String(compiler))
	}
}

func TestCompileIdentifiers(t *testing.T) {
	src := `var a
a`

	expected := `Null
STORE_MODULE_VAR     0
Pop
LOAD_MODULE_VAR      0
Pop
Return
End
`

	vm := NewWrenVM()

	module := &ObjModule{
		VariableNames: make(map[string]Demo),
		variables:     make([]Value, 65535),
	}

	compiler := vm.wrenCompile(module, src, false, false)

	if compiler.instructions.String(compiler) != expected {
		t.Errorf("Compile[%q]: expected: \n`%q`, \n\ngot \n`%q`", src, expected, compiler.instructions.String(compiler))
	}

}

func TestCompileVariableDefinition(t *testing.T) {
	src := "var a = 1"

	expected := `Constant: 0	1.000000
STORE_MODULE_VAR     0
Pop
Return
End
`

	vm := NewWrenVM()

	module := &ObjModule{
		VariableNames: make(map[string]Demo),
		variables:     make([]Value, 65535),
	}

	compiler := vm.wrenCompile(module, src, false, false)

	if compiler.instructions.String(compiler) != expected {
		t.Errorf("Compile[%q]: expected: \n`%q`, \n\ngot \n`%q`", src, expected, compiler.instructions.String(compiler))
	}
}

func TestCompileGrouping(t *testing.T) {
	src := "!( 1 == 2 )"

	expected := `Constant: 0	1.000000
Constant: 1	2.000000
CALL_1    0
CALL_0    1
Pop
Return
End
`
	vm := NewWrenVM()

	module := &ObjModule{
		VariableNames: make(map[string]Demo),
		variables:     make([]Value, 65535),
	}

	compiler := vm.wrenCompile(module, src, false, false)

	if compiler.instructions.String(compiler) != expected {
		t.Errorf("Compile[%q]: expected: \n`%s`, \n\ngot \n`%s`", src, expected, compiler.instructions.String(compiler))
	}
}

func TestCompileWhileStatements(t *testing.T) {
	src := `var i = 0
while ( i < 10) i = i + 1`

	expected := `Constant: 0	0.000000
STORE_MODULE_VAR     0
Pop
LOAD_MODULE_VAR      0
Constant: 1	10.000000
CALL_1    0
JUMP_IF             16 to 35
LOAD_MODULE_VAR      0
Constant: 2	1.000000
CALL_1    1
STORE_MODULE_VAR     0
Pop
LOOP                24 to 11
Return
End
`

	vm := NewWrenVM()

	module := &ObjModule{
		VariableNames: make(map[string]Demo),
		variables:     make([]Value, 65535),
	}

	compiler := vm.wrenCompile(module, src, false, false)

	if compiler.instructions.String(compiler) != expected {
		t.Errorf("Compile[%q]: expected: \n`%q`, \n\ngot \n`%q`", src, expected, compiler.instructions.String(compiler))
	}
}

func TestCompileBlockStatements(t *testing.T) {
	src :=
		`{
	var i = 10
	i = 9
	var j = i + 10
}`

	expected := `Constant: 0	10.000000
Constant: 1	9.000000
CODE_STORE_LOCAL      1
Pop
LOAD_LOCAL_1
Constant: 2	10.00000
CALL_1	0
Pop
Pop
Return
End`

	vm := NewWrenVM()

	module := &ObjModule{
		VariableNames: make(map[string]Demo),
		variables:     make([]Value, 65535),
	}

	compiler := vm.wrenCompile(module, src, false, false)

	if compiler.instructions.String(compiler) != expected {
		t.Errorf("Compile[%q]: expected: \n`%q`, \n\ngot \n`%q`", src, expected, compiler.instructions.String(compiler))
	}
}

func TestCompileClasses(t *testing.T) {
	src := `class Foo {
		bar() {

		}
	}`

	expected := `

	`
	vm := NewWrenVM()

	module := &ObjModule{
		VariableNames: make(map[string]Demo),
		variables:     make([]Value, 65535),
	}

	compiler := vm.wrenCompile(module, src, false, false)

	if compiler.instructions.String(compiler) != expected {
		t.Errorf("Compile[%q]: expected: \n`%q`, \n\ngot \n`%q`", src, expected, compiler.instructions.String(compiler))
	}
}
