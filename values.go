package wren

import (
	"fmt"
)

type ObjType uint

const (
	OBJ_STRING ObjType = iota
	OBJ_FIBER
	OBJ_CLOSURE
	OBJ_MODULE
	OBJ_RANGE
	OBJ_CLASS
)

type Object struct {
	classObj *ObjClass
}

type Obj interface {
	ObjType() ObjType
	PrintObject() string
}

type ValueType uint

const (
	VAL_FALSE ValueType = iota
	VAL_NUM
	VAL_TRUE
	VAL_NULL
	VAL_STRING
	VAL_OBJ
)

type Value interface {
	ValueType() ValueType
	Print() string
}

type BoolValue struct {
	Type  ValueType
	value bool
}

func (bl BoolValue) ValueType() ValueType {
	return bl.Type
}

func (bl BoolValue) Print() string {
	return fmt.Sprintf("%t", bl.value)
}

type NullValue struct {
	Type ValueType
}

func (nl NullValue) ValueType() ValueType {
	return nl.Type
}

func (nl NullValue) Print() string {
	return fmt.Sprintf("%s", "null")
}

type NumValue struct {
	Type   ValueType
	Number float64
}

func (nl NumValue) ValueType() ValueType {
	return VAL_NUM
}

func (nl NumValue) Print() string {
	return fmt.Sprintf("%f", nl.Number)
}

type StringValue struct {
	Type  ValueType
	Value string
}

func (sv StringValue) ValueType() ValueType {
	return VAL_STRING
}

func (sv StringValue) Print() string {
	return sv.Value
}

type ObjectValue struct {
	Type ValueType
	Obj  Obj
	Object
}

func (sv ObjectValue) ValueType() ValueType {
	return VAL_OBJ
}

func (sv ObjectValue) Print() string {
	return sv.Obj.PrintObject()
}

type StringObject struct {
	Type  ObjType
	Value string
}

func (so StringObject) ObjType() ObjType {
	return so.Type
}

func (so StringObject) PrintObject() string {
	return so.Value
}

type ObjFn struct {
	numUpvalues int

	Type      ObjType
	constants []Value

	instructions Instructions

	numUpValues int

	module *ObjModule
}

func (of *ObjFn) ObjType() ObjType {
	return of.Type
}

func (of *ObjFn) PrintObject() string {
	return fmt.Sprintf("Fn: [%p]", of)
}

type ObjUpvalue struct {
	value  *Value
	closed Value
}

type ObjClosure struct {
	Type ObjType

	fn *ObjFn

	upvalues []*ObjUpvalue
}

func (oc *ObjClosure) ObjType() ObjType {
	return oc.Type
}

func (oc *ObjClosure) PrintObject() string {
	return fmt.Sprintf("Closure: [%p]", oc)
}

type CallFrame struct {
	ip int

	closure *ObjClosure

	stackStart int

	stack []Value
}

type ObjFiber struct {
	Type ObjType

	stack []Value

	stackTop int

	frames []*CallFrame

	numFrames int
}

type MethodType uint

const (
	METHOD_PRIMITIVE MethodType = iota
	METHOD_BLOCK
	METHOD_NONE
)

type Primitive func(*WrenVM, []Value) bool

type Method struct {
	Type      MethodType
	primitive Primitive
	Closure   *ObjClosure
}

type ObjClass struct {
	Type       ObjType
	name       string
	methods    map[string]*Method
	superclass *ObjClass
}

func (oc *ObjClass) ObjType() ObjType {
	return oc.Type
}

func (oc *ObjClass) PrintObject() string {
	return fmt.Sprintf("Class: %s", oc.name)
}

type Demo struct {
	value Value
	index int
}

type ObjModule struct {
	Type          ObjType
	VariableNames map[string]Demo
	//VariableNames map[string]map[Value]int
	//VariableNames map[string]Value
	variables []Value
	Name      Value
}

func (om *ObjModule) ObjType() ObjType {
	return om.Type
}

func (om *ObjModule) PrintObject() string {
	return fmt.Sprintf("Module: [%p]", om)
}

type ObjRange struct {
	Type        ObjType
	from        float64
	to          float64
	isInclusive bool
}

func (or *ObjRange) ObjType() ObjType {
	return or.Type
}

func (or *ObjRange) PrintObject() string {
	sep := "..."

	if or.isInclusive {
		sep = ".."
	}

	return fmt.Sprintf("%.f%s%.f", or.from, sep, or.to)
}
