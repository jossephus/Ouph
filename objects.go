package wren

func NewFiber(vm *WrenVM, closure *ObjClosure) *ObjFiber {
	frames := make([]*CallFrame, 4)

	fiber := &ObjFiber{
		Type: OBJ_FIBER,
		//TODO: make([]Value, powerOf2(closure->fn->maxSlots + 1))
		//stack:     make([]Value, 6553335),
		stack:     make([]Value, 65535),
		stackTop:  0,
		frames:    frames,
		numFrames: 0,
	}

	if closure != nil {
		fiber.frames[fiber.numFrames] = NewFrame(closure, 0)
		frame := fiber.frames[fiber.numFrames]
		fiber.numFrames++
		//TODO: stackStart is like basePointer for monkey-vm.go
		//frame.stackStart = 0
		frame.stack = append(frame.stack, ObjectValue{
			Type: VAL_OBJ,
			Obj:  closure,
		})
		frame.stackStart++
	}

	return fiber
}

func NewFrame(closure *ObjClosure, stackStart int) *CallFrame {

	return &CallFrame{
		closure:    closure,
		ip:         0,
		stackStart: stackStart,
		stack:      []Value{},
	}
}

func NewRange(vm *WrenVM, from float64, to float64, isInclusive bool) *ObjRange {
	r := &ObjRange{
		Type:        OBJ_RANGE,
		from:        from,
		to:          to,
		isInclusive: isInclusive,
	}

	return r
}

func NewClass(superclass Value, numFields int, name Value) *ObjClass {

	return &ObjClass{
		Type: OBJ_CLASS,
		name: name.(ObjectValue).Obj.(StringObject).Value,
		superclass: nil,

	}
}

func NewClosure(fn *ObjFn) *ObjClosure {
	return &ObjClosure{
		Type: OBJ_CLOSURE,
		fn: fn,
	}
}

func NewInstance(class *ObjClass) *ObjInstance {
	return &ObjInstance{
		Type: OBJ_INSTANCE,
		Object: Object{
			classObj: class,
		},
	}

}
