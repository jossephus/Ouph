package wren

import "testing"

func TestInterpretBooleans(t *testing.T) {
	src := "true"

	vm := &WrenVM{
		stack: []Value{},
		sp: 0,
	}

	vm.Interpret("", src)

	value := vm.StackTop()

	if value.ValueType() != VAL_TRUE {
		t.Errorf("value.Type: expected VAL_TRUE: got %v", value.ValueType())
	}
}


func TestInterpretIntegers(t *testing.T) {
	src := "1"

	vm := &WrenVM{
		stack: []Value{},
		sp: 0,
	}

	vm.Interpret("", src)

	value := vm.StackTop()

	if value.ValueType() != VAL_NUM {
		t.Errorf("value.Type: expected VAL_NUM: got %v", value.ValueType())
	}
}

func TestInterpretStrings(t *testing.T) {
	src := `"hello"`

	vm := &WrenVM{
		stack: []Value{},
		sp:0,
	}
	vm.Interpret("", src)

	value := vm.StackTop()

	if value.ValueType() != VAL_OBJ {
		t.Errorf("Value.Type: expected VAL_STRING: got %v", value.ValueType())
	}
}


