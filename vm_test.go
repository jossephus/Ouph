package wren

import "testing"

func TestInterpretBooleans(t *testing.T) {
	for _, tt := range []struct {
		src      string
		expected bool
	}{
		{"true", true},
		//{`"hello world".contains("hello")`, true},
		//{`"hello world".contains("hwor")`, false},
	} {
		vm := NewWrenVM()

		vm.Interpret("", tt.src, false)

		value := vm.StackTop()

		if value.ValueType() != VAL_TRUE && value.ValueType() != VAL_FALSE {
			t.Errorf("value.Type: expected VAL_TRUE or VAL_FALSE. got %v", value.ValueType())
		}

		bool_val := value.(BoolValue).value

		if bool_val != tt.expected {
			t.Errorf("%s: expected %t got %t", tt.src, tt.expected, bool_val)
		}
	}
}

func TestInterpretIntegers(t *testing.T) {
	for _, tt := range []struct {
		src      string
		expected float64
	}{
		{"1", 1.0},
		//{"1.3.floor", 1.0},
		//{"1.3.ceil", 2.0},
	} {
		vm := NewWrenVM()

		vm.Interpret("", tt.src, false)

		value := vm.StackTop()

		if value.ValueType() != VAL_NUM {
			t.Errorf("value.Type: expected VAL_NUM. got %v", value.ValueType())
		}

		number := value.(NumValue).Number

		if number != tt.expected {
			t.Errorf("number expected %f got %f", tt.expected, number)
		}

	}
}

func TestInterpretStrings(t *testing.T) {
	for _, tt := range []struct {
		src      string
		expected string
	}{
		{`"hello"`, "hello"},
	} {
		vm := NewWrenVM()
		vm.Interpret("", tt.src, false)

		value := vm.StackTop()

		if value.ValueType() != VAL_OBJ {
			t.Errorf("Value.Type: expected VAL_STRING: got %v", value.ValueType())
		}

		str, ok := value.(ObjectValue).Obj.(StringObject)

		if !ok {
			t.Errorf("Want string . got something else ")
		}

		if str.Value != tt.expected {
			t.Errorf("want %s, got %s", str.Value, tt.expected)
		}

	}
}

func TestPrefixArthimetic(t *testing.T) {
	t.Run("Prefix Operation on Num", func(t *testing.T) {
		for _, tt := range []struct {
			src      string
			expected float64
		}{
			{"-1", -1.0},
			{"~1", 4294967294.0},
		} {
			vm := NewWrenVM()

			vm.Interpret("", tt.src, false)

			value := vm.StackTop()

			if value.ValueType() != VAL_NUM {
				t.Errorf("value.Type: expected VAL_NUM: got %v", value.ValueType())
			}

			number := value.(NumValue).Number

			if number != tt.expected {
				t.Errorf("number expected %f got %f", tt.expected, number)
			}

		}

	})

	t.Run("Bang operator on booleans", func(t *testing.T) {
		for _, tt := range []struct {
			src      string
			expected bool
		}{
			{"!true", false},
			{"!false", true},
		} {
			vm := NewWrenVM()

			vm.Interpret("", tt.src, false)

			value := vm.StackTop()

			if value.ValueType() != VAL_TRUE && value.ValueType() != VAL_FALSE {
				t.Errorf("value.Type: expected to be a boolean. got %v", value.ValueType())
			}

			val := value.(BoolValue).value

			if val != tt.expected {
				t.Errorf("[%s] => expected to be %t, got %t", tt.src, tt.expected, val)
			}

		}
	})
}

func TestInterpretInfixArthimetic(t *testing.T) {
	t.Run("Simple Arthimetic infix operations", func(t *testing.T) {

		for _, tt := range []struct {
			src      string
			expected float64
		}{
			{"1 + 1", 2.000000},
			{"2 - 1", 1.000000},
			{"2 * 2", 4.000000},
			{"2 / 2", 1.000000},
		} {
			vm := NewWrenVM()

			vm.Interpret("", tt.src, false)

			value := vm.StackTop()

			if value.ValueType() != VAL_NUM {
				t.Errorf("")
			}

			number := value.(NumValue).Number

			if number != tt.expected {
				t.Errorf("number expected %f got %f", tt.expected, number)
			}
		}
	})

	t.Run("Conditional Infix Operations", func(t *testing.T) {
		for _, tt := range []struct {
			src      string
			expected bool
		}{
			{"1 < 2", true},
			{"1 > 2", false},
			{"2 >= 2", true},
			{"2 <= 2", true},
			{"3 <= 2", false},
			{"3 == 2", false},
			{"3 == 3", true},
			{"3 != 2", true},
			{"2 != 2", false},
			{"! ( 1 == 2)", true},
		} {
			vm := NewWrenVM()

			vm.Interpret("", tt.src, false)

			value := vm.StackTop()

			if value.ValueType() != VAL_TRUE && value.ValueType() != VAL_FALSE {
				t.Errorf("Value  Expected to be boolean. got something else %d ", value.ValueType())
			}

			val := value.(BoolValue).value

			if val != tt.expected {
				t.Errorf("[%s] => expected to be %t, got %t", tt.src, tt.expected, val)
			}
		}
	})

	t.Run("Bitwise operations", func(t *testing.T) {
		for _, tt := range []struct {
			src      string
			expected float64
		}{
			{"1 & 2", 0.0},
			{"1 | 2", 3.0},
			{"1 ^ 2", 3.0},
			{"1 << 2", 4.0},
			{"1 >> 2", 0.0},
			{"4 % 2", 0.0},
		} {
			vm := NewWrenVM()

			vm.Interpret("", tt.src, false)

			value := vm.StackTop()

			if value.ValueType() != VAL_NUM {

			}

			val := value.(NumValue).Number

			if val != tt.expected {
				t.Errorf("[%s] => expected to be %f, got %f", tt.src, tt.expected, val)
			}

		}
	})

	t.Run("Range ", func(t *testing.T) {
		for _, tt := range []struct {
			src      string
			expected string
		}{
			{"1..2", "1..2"},
			{"1...2", "1...2"},
		} {
			vm := NewWrenVM()

			vm.Interpret("", tt.src, false)

			value := vm.StackTop()

			if value.Print() != tt.expected {
				t.Errorf("[%s] => expected to be %s, got %s", tt.src, tt.expected, value.Print())
			}
		}
	})

	t.Run("Infix Operation on strings", func(t *testing.T) {
		for _, tt := range []struct {
			src      string
			expected string
		}{
			{"\"hello \" + \"world\"", "hello world"},
			{"\"hello\"[0]", "h"},
		} {
			vm := NewWrenVM()

			vm.Interpret("", tt.src, false)

			value := vm.StackTop()

			if value.ValueType() != VAL_OBJ {
				t.Errorf("Value.Type: expected VAL_STRING: got %v", value.ValueType())
			}

			str, ok := value.(ObjectValue).Obj.(StringObject)

			if !ok {
				t.Errorf("Want string . got something else ")
			}

			if str.Value != tt.expected {
				t.Errorf("want %s, got %s", str.Value, tt.expected)
			}

		}
	})
}

func TestInterpretCallExpressions(t *testing.T) {
	for _, tt := range []struct {
		src      string
		expected float64
	}{
		{"1.floor", 1},
	} {
		vm := NewWrenVM()

		vm.Interpret("", tt.src, false)

		value := vm.StackTop()

		if value.ValueType() != VAL_NUM {
			t.Errorf("Expected a VAL_NUM. got %d", value.ValueType())
		}

		number := value.(NumValue).Number

		if number != tt.expected {
			t.Errorf("number expected %f got %f", tt.expected, number)
		}
	}
}

func TestInterpretVariableDeclarationsAndAssignments(t *testing.T) {
	for _, tt := range []struct {
		src      string
		expected float64
	}{
		{`var a = 3 + 4
a`, 7.0},
		{`var a = 7
a = 8
a`, 8.0},
	} {
		vm := NewWrenVM()

		vm.Interpret("", tt.src, false)

		value := vm.StackTop()

		if value.ValueType() != VAL_NUM {
			t.Errorf("Expected a VAL_NUM. got %d", value.ValueType())
		}

		number := value.(NumValue).Number

		if number != tt.expected {
			t.Errorf("number expected %f got %f", tt.expected, number)
		}

	}
}

func TestInterpretIfStatements(t *testing.T) {
	for _, tt := range []struct {
		src      string
		expected bool
	}{
		{`var oneIsLessThanTwo
if (1 < 2) oneIsLessThanTwo = true else oneIsLessThanTwo = false
oneIsLessThanTwo`, true},
		{`var twoIsLessThanOne
if (2 < 1) twoIsLessThanOne = true else twoIsLessThanOne = false
twoIsLessThanOne`, false},
	} {

		vm := NewWrenVM()

		vm.Interpret("", tt.src, false)

		value := vm.StackTop()

		if value.ValueType() != VAL_TRUE && value.ValueType() != VAL_FALSE {
			t.Errorf("value.Type: expected to be a boolean. got %v", value.ValueType())
		}

		val := value.(BoolValue).value

		if val != tt.expected {
			t.Errorf("[%s] => expected to be %t, got %t", tt.src, tt.expected, val)
		}
	}
}

func TestWhileStatements(t *testing.T) {
	for _, tt := range []struct {
		src      string
		expected float64
	}{
		{`var i = 0
while ( i < 10 ) i = i + 1
i`, 10.0},
	} {
		vm := NewWrenVM()

		vm.Interpret("", tt.src, false)

		value := vm.StackTop()

		if value.ValueType() != VAL_NUM {
			t.Errorf("Expected a VAL_NUM. got %d", value.ValueType())
		}

		number := value.(NumValue).Number

		if number != tt.expected {
			t.Errorf("number expected %f got %f", tt.expected, number)
		}

	}
}

func TestBlockStatements(t *testing.T) {

	for _, tt := range []struct {
		src      string
		expected float64
	}{
		{`var i = 3
{
	i = 3 * 2
}
var b = 10
i`, 6.0},
		// should get an error 'Variable is already define '
		/*{
		`var i = 10
		{
			var i = 3 * 2
			var i = 5
		}
		i`, 6.0,}, */
	} {
		vm := NewWrenVM()

		vm.Interpret("", tt.src, false)

		value := vm.StackTop()

		if value.ValueType() != VAL_NUM {
			t.Errorf("value.Type: expected VAL_NUM: got %v", value.ValueType())
		}

		number := value.(NumValue).Number

		if number != tt.expected {
			t.Errorf("number expected %f got %f", tt.expected, number)
		}

	}
}

func TestBreakAndContinueStatements(t *testing.T) {
	t.Run("Break", func(t *testing.T) {
		for _, tt := range []struct {
			src      string
			expected float64
		}{
			{
				`var i = 0
while( i <= 20) {
	if (i == 12) {
		break
	}
	i = i + 1
}
i`, 12},
		} {
			vm := NewWrenVM()

			vm.Interpret("", tt.src, false)
			value := vm.StackTop()

			if value.ValueType() != VAL_NUM {
				t.Errorf("value.Type: expected VAL_NUM: got %v", value.ValueType())
			}

			number := value.(NumValue).Number

			if number != tt.expected {
				t.Errorf("number expected %f got %f", tt.expected, number)
			}
		}
	})

	t.Run("Continue", func(t *testing.T) {
		for _, tt := range []struct {
			src      string
			expected float64
		}{
			{
				`var j
var i = 0
while (i <= 4) {
	j = i
	i = i + 1
	continue
	i = 2000
}
j`, 4,
			},
		} {
			vm := NewWrenVM()

			vm.Interpret("", tt.src, false)
			value := vm.StackTop()

			if value.ValueType() != VAL_NUM {
				t.Errorf("value.Type: expected VAL_NUM: got %v", value.ValueType())
			}

			number := value.(NumValue).Number

			if number != tt.expected {
				t.Errorf("number expected %f got %f", tt.expected, number)
			}

		}
	})
}

func TestClassStatements(t *testing.T) {
	for _, tt := range []struct {
		src      string
		expected string
	}{
		/*{
			`class Foo {}
var a = Foo.name
a`, "Foo"},*/
		{
			`class Bar {
				bar(a) {}
		}
var b = Bar.name
b`, "Bar"},
	} {
		vm := NewWrenVM()

		vm.Interpret("", tt.src, false)

		value := vm.StackTop()

		if value.ValueType() != VAL_OBJ {
			t.Fatalf("Value.Type: expected VAL_STRING: got %v", value.ValueType())
		}

		str, ok := value.(ObjectValue).Obj.(StringObject)

		if !ok {
			t.Errorf("Want string . got something else ")
		}

		if str.Value != tt.expected {
			t.Errorf("want %s, got %s", tt.expected, str.Value)
		}

	}
}

func TestCreateObjects(t *testing.T) {
	for _, tt := range []struct{
		src string
		expected bool
	}{
		{	
`class Foo {
	construct new() {}
}
var a = Foo.new()
a is Foo`, true,
		},
	}{
		vm := NewWrenVM()

		vm.Interpret("", tt.src, false)

		value := vm.StackTop()

		if value.ValueType() != VAL_TRUE && value.ValueType() != VAL_FALSE {
			t.Errorf("value.Type: expected VAL_TRUE or VAL_FALSE. got %v", value.ValueType())
		}

		bool_val := value.(BoolValue).value

		if bool_val != tt.expected {
			t.Errorf("%s: expected %t got %t", tt.src, tt.expected, bool_val)
		}

	}

}

/*
func TestBasicArthimetic(t *testing.T) {
	src := "1 + 2"

	vm := &WrenVM{
		stack: []Value{},
		sp: 0,
	}

	vm.Interpret("", src)

	value := vm.StackTop()

	if value.ValueType() != VAL_NUM {
		t.Fatalf("Value.Type: expected VAL_NUM: got %v", value.ValueType())
	}

	if value.(NumValue).Number != 3 {
		t.Fatalf("Expected value 3 to be on top of the stack. got %d", value.(NumValue).Number)
	}
}
*/
