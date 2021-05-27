package wren

import (
	"fmt"
)

type ObjType uint

const (
	OBJ_STRING ObjType = iota
)

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
	Obj Obj
}

func (sv ObjectValue) ValueType() ValueType {
	return VAL_OBJ
}

func (sv ObjectValue) Print() string {
	return sv.Obj.PrintObject()
}

type StringObject struct {
	Type ObjType
	Value string
}

func (so StringObject) ObjType() ObjType {
	return so.Type
}

func (so StringObject) PrintObject() string {
	return so.Value
}
