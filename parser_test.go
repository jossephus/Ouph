package wren

import (
	"bytes"
	"testing"
)

func parseExpression(src string, parser *Parser, rules map[TokenType]GrammarRule) *bytes.Buffer {
	parser.source = src

	parser.offset = 0
	parser.rdOffset = 0

	parser.nextToken()
	parser.nextToken()

	parser.rules = rules

	var result *bytes.Buffer = new(bytes.Buffer)
	parser.out = result

	parser.expression()

	return result

}

func appendQuoted(strings []string) string {
	src := ""

	for _, s := range strings {
		src += "\"" + s + "\"" + "\t"
	}

	// remove the last tab
	src = src[:len(src)-1]

	return src
}

func TestParseLiterals(t *testing.T) {

	for _, tt := range []struct {
		source    string
		tokenType TokenType
		expected  string
	}{
		{
			"1",
			TOKEN_NUMBER,
			`"number  1"`,
		},
		{
			`"hello world"`,
			TOKEN_STRING,
			`"string  "hello world""`,
		},
	} {
		var parser Parser

		var rules = map[TokenType]GrammarRule{
			TOKEN_NUMBER: GrammarRule{parser.literal, nil, PREC_NONE},
			TOKEN_STRING: GrammarRule{parser.literal, nil, PREC_NONE},
		}

		result := parseExpression(tt.source, &parser, rules)

		if result.String() != tt.expected {
			t.Errorf("Parse[%q]:= expected %q, got %q", tt.source, tt.expected, result.String())
		}
	}
}

func TestBooleanLiterals(t *testing.T) {
	for _, tt := range []struct {
		source   string
		expected []string
	}{
		{"true", []string{"true"}},
		{"false", []string{"false"}},
	} {
		expected := appendQuoted(tt.expected)

		var parser Parser

		rules := map[TokenType]GrammarRule{
			TOKEN_TRUE:  GrammarRule{parser.boolean, nil, PREC_NONE},
			TOKEN_FALSE: GrammarRule{parser.boolean, nil, PREC_NONE},
		}

		result := parseExpression(tt.source, &parser, rules)

		if result.String() != expected {
			t.Errorf("Parse[%q] := expected %q, got %q", tt.source, expected, result.String())
		}
	}
}

func TestParseNames(t *testing.T) {
	for _, tt := range []struct {
		source   string
		expected []string
	}{
		{"x", []string{"x"}},
	} {
		expected := appendQuoted(tt.expected)

		var parser Parser

		rules := map[TokenType]GrammarRule{
			TOKEN_NAME: GrammarRule{parser.name, nil, PREC_NONE},
		}

		result := parseExpression(tt.source, &parser, rules)

		if result.String() != expected {
			t.Errorf("Parse[%q] := expected %q, got %q", tt.source, expected, result.String())
		}

	}
}

func TestGroupingExpressions(t *testing.T) {
	for _, tt := range []struct {
		source   string
		expected string
	}{
		{"(1)", `"grouping  ("	"number  1"	)`},
	} {
		var parser Parser

		rules := map[TokenType]GrammarRule{
			TOKEN_NUMBER:     GrammarRule{parser.literal, nil, PREC_NONE},
			TOKEN_LEFT_PAREN: GrammarRule{parser.grouping, nil, PREC_NONE},
		}

		result := parseExpression(tt.source, &parser, rules)

		if result.String() != tt.expected {
			t.Errorf("Parse[%q]:= expected ``%s``, got ``%s``", tt.source, tt.expected, result.String())
		}
	}
}

func TestPrefix(t *testing.T) {
	for _, tt := range []struct {
		source   string
		expected []string
	}{
		{"!true", []string{"!", "true"}},
		{"~100", []string{"~", "number  100"}},
		{"-100", []string{"-", "number  100"}},
	} {
		expected := appendQuoted(tt.expected)

		var parser Parser

		rules := map[TokenType]GrammarRule{
			TOKEN_BANG:   GrammarRule{parser.unaryOp, nil, PREC_NONE},
			TOKEN_TILDE:  GrammarRule{parser.unaryOp, nil, PREC_NONE},
			TOKEN_TRUE:   GrammarRule{parser.boolean, nil, PREC_NONE},
			TOKEN_NUMBER: GrammarRule{parser.literal, nil, PREC_NONE},
			TOKEN_MINUS:  GrammarRule{parser.unaryOp, parser.infixOp, PREC_TERM},
		}

		result := parseExpression(tt.source, &parser, rules)

		if expected != result.String() {
			t.Errorf("Parse[%q]:\n\r\texpected\t\t%q, \n\r\tgot\t\t%q", tt.source, expected, result.String())
		}
	}
}

func TestBasicArithmetics(t *testing.T) {
	for _, tt := range []struct {
		source   string
		expected []string
	}{
		{"1 + 2", []string{"number  1", "+", "number  2"}},
		{"3 - 4", []string{"number  3", "-", "number  4"}},
		{"5 * 6", []string{"number  5", "*", "number  6"}},
		{"7 / 8", []string{"number  7", "/", "number  8"}},
		{"14 < 15", []string{"number  14", "<", "number  15"}},
		{"14 > 15", []string{"number  14", ">", "number  15"}},
		{"10 == 11", []string{"number  10", "==", "number  11"}},
		{"12 != 13", []string{"number  12", "!=", "number  13"}},
		{"14 >= 15", []string{"number  14", ">=", "number  15"}},
		{"14 <= 15", []string{"number  14", "<=", "number  15"}},
		{"14 % 15", []string{"number  14", "%", "number  15"}},
		{"1 << 8", []string{"number  1", "<<", "number  8"}},
		{"1 >> 8", []string{"number  1", ">>", "number  8"}},
		{"1 | 0", []string{"number  1", "|", "number  0"}},
		{"1 ^ 0", []string{"number  1", "^", "number  0"}},
		{"1 & 0", []string{"number  1", "&", "number  0"}},
		{"1..2", []string{"number  1", "..", "number  2"}},
		{"1...2", []string{"number  1", "...", "number  2"}},
		{"1 is 1", []string{"number  1", "is", "number  1"}},
	} {
		var parser Parser

		rules := map[TokenType]GrammarRule{
			TOKEN_NUMBER:    GrammarRule{parser.literal, nil, PREC_NONE},
			TOKEN_PLUS:      GrammarRule{nil, parser.infixOp, PREC_TERM},
			TOKEN_MINUS:     GrammarRule{nil, parser.infixOp, PREC_TERM},
			TOKEN_STAR:      GrammarRule{nil, parser.infixOp, PREC_FACTOR},
			TOKEN_SLASH:     GrammarRule{nil, parser.infixOp, PREC_FACTOR},
			TOKEN_EQEQ:      GrammarRule{nil, parser.infixOp, PREC_EQUALITY},
			TOKEN_BANGEQ:    GrammarRule{nil, parser.infixOp, PREC_EQUALITY},
			TOKEN_LT:        GrammarRule{nil, parser.infixOp, PREC_COMPARISION},
			TOKEN_GT:        GrammarRule{nil, parser.infixOp, PREC_COMPARISION},
			TOKEN_LTEQ:      GrammarRule{nil, parser.infixOp, PREC_COMPARISION},
			TOKEN_GTEQ:      GrammarRule{nil, parser.infixOp, PREC_COMPARISION},
			TOKEN_PERCENT:   GrammarRule{nil, parser.infixOp, PREC_FACTOR},
			TOKEN_LTLT:      GrammarRule{nil, parser.infixOp, PREC_BITWISE_SHIFT},
			TOKEN_GTGT:      GrammarRule{nil, parser.infixOp, PREC_BITWISE_SHIFT},
			TOKEN_PIPE:      GrammarRule{nil, parser.infixOp, PREC_BITWISE_OR},
			TOKEN_CARET:     GrammarRule{nil, parser.infixOp, PREC_BITWISE_XOR},
			TOKEN_AMP:       GrammarRule{nil, parser.infixOp, PREC_BITWISE_AND},
			TOKEN_DOTDOT:    GrammarRule{nil, parser.infixOp, PREC_RANGE},
			TOKEN_DOTDOTDOT: GrammarRule{nil, parser.infixOp, PREC_RANGE},
			TOKEN_IS:        GrammarRule{nil, parser.infixOp, PREC_IS},
		}

		result := parseExpression(tt.source, &parser, rules)

		expected := appendQuoted(tt.expected)

		if result.String() != expected {
			t.Errorf("Parse[%q]:\n\r\texpected\t\t%q, \n\r\t\tgot\t\t%q", tt.source, expected, result.String())
		}
	}
}

func TestParseList(t *testing.T) {
	for _, tt := range []struct {
		source   string
		expected []string
	}{
		{"[1,2,3]", []string{"list  [", "number  1", "number  2", "number  3", "]"}},
		{"[1]", []string{"list  [", "number  1", "]"}},
		{"[]", []string{"list  [", "]"}},
	} {

		var parser Parser

		rules := map[TokenType]GrammarRule{
			TOKEN_LEFT_BRACKET: GrammarRule{parser.list, nil, PREC_NONE},
			TOKEN_NUMBER:       GrammarRule{parser.literal, nil, PREC_NONE},
		}

		result := parseExpression(tt.source, &parser, rules)

		expected := appendQuoted(tt.expected)

		if result.String() != expected {
			t.Errorf("Parse[%q]:\n\r\texpected\t\t%q, \n\r\tgot\t\t\t%q", tt.source, expected, result.String())
		}
	}
}

func TestParseSubscript(t *testing.T) {
	for _, tt := range []struct {
		source   string
		expected []string
	}{
		{"list[1]", []string{"list", "subscript  [", "number  1", "]"}},
	} {
		var parser Parser

		rules := map[TokenType]GrammarRule{
			TOKEN_LEFT_BRACKET: GrammarRule{parser.list, parser.subscript, PREC_CALL},
			TOKEN_NUMBER:       GrammarRule{parser.literal, nil, PREC_NONE},
			TOKEN_NAME:         GrammarRule{parser.name, nil, PREC_NONE},
		}

		result := parseExpression(tt.source, &parser, rules)

		expected := appendQuoted(tt.expected)

		if result.String() != expected {
			t.Errorf("Parse[%q]:\n\r\texpected\t\t%q, \n\r\tgot\t\t\t%q", tt.source, expected, result.String())
		}
	}
}

func TestParseMaps(t *testing.T) {
	for _, tt := range []struct {
		source   string
		expected []string
	}{
		{"{1: 2}", []string{"map  {", "number  1", ":", "number  2", "}"}},
		{"{1: 2, 3 : 4}", []string{
			"map  {",
			"number  1",
			":",
			"number  2",
			",",
			"number  3",
			":",
			"number  4",
			"}",
		}},
		{"{}", []string{"map  {", "}"}},
	} {
		var parser Parser

		rules := map[TokenType]GrammarRule{
			TOKEN_LEFT_BRACE: GrammarRule{parser.mapp, nil, PREC_NONE},
			TOKEN_NUMBER:     GrammarRule{parser.literal, nil, PREC_NONE},
		}

		result := parseExpression(tt.source, &parser, rules)

		expected := appendQuoted(tt.expected)

		if result.String() != expected {
			t.Errorf("Parse[%q]:\n\r\texpected\t\t%s, \n\r\tgot\t\t\t%s", tt.source, expected, result.String())
		}

	}
}
