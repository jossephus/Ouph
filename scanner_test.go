package wren

import "testing"

func TestScansOperators(t *testing.T) {
	var parser Parser

	src := "() [] {} : , * % ^ + - ~ ? | || & && = == ! != . .. ... / < << <= > >> >= \n"

	parser.source = src
	parser.rdOffset = 0
	parser.offset = 0

	expected := []struct {
		typ     TokenType
		content string
	}{
		{TOKEN_LEFT_PAREN, "("},
		{TOKEN_RIGHT_PAREN, ")"},
		{TOKEN_LEFT_BRACKET, "["},
		{TOKEN_RIGHT_BRACKET, "]"},
		{TOKEN_LEFT_BRACE, "{"},
		{TOKEN_RIGHT_BRACE, "}"},
		{TOKEN_COLON, ":"},
		{TOKEN_COMMA, ","},
		{TOKEN_STAR, "*"},
		{TOKEN_PERCENT, "%"},
		{TOKEN_CARET, "^"},
		{TOKEN_PLUS, "+"},
		{TOKEN_MINUS, "-"},
		{TOKEN_TILDE, "~"},
		{TOKEN_QUESTION, "?"},
		{TOKEN_PIPE, "|"},
		{TOKEN_PIPE_PIPE, "||"},
		{TOKEN_AMP, "&"},
		{TOKEN_AMPAMP, "&&"},
		{TOKEN_EQ, "="},
		{TOKEN_EQEQ, "=="},
		{TOKEN_BANG, "!"},
		{TOKEN_BANGEQ, "!="},
		{TOKEN_DOT, "."},
		{TOKEN_DOTDOT, ".."},
		{TOKEN_DOTDOTDOT, "..."},
		{TOKEN_SLASH, "/"},
		{TOKEN_LT, "<"},
		{TOKEN_LTLT, "<<"},
		{TOKEN_LTEQ, "<="},
		{TOKEN_GT, ">"},
		{TOKEN_GTGT, ">>"},
		{TOKEN_GTEQ, ">="},
		{TOKEN_LINE, "\n"},
		{TOKEN_EOF, ""},
	}

	// verify that we are starting with a clean state

	for _, v := range expected {
		parser.nextToken()

		if parser.next.Type != v.typ {
			t.Errorf("TokenType: want %q; got %q", v.typ, parser.next.Type)
		}

		if parser.next.Content != v.content {
			t.Errorf("Token Content: want %q; got %q", v.content, parser.next.Content)
		}
	}
}

func TestReadString(t *testing.T) {
	t.Run("reads regular strings", func(t *testing.T) {
		src := `"hello world"`

		var parser Parser
		parser.source = src
		parser.rdOffset = 0
		parser.offset = 0

		parser.nextToken()

		if parser.next.Type != TOKEN_STRING {
			t.Errorf("Expected TOKEN_STRING. got %q", parser.next.Type)
		}

		if parser.next.Content != src {
			t.Errorf("Expected content %q. got %q", src, parser.next.Content)
		}
	})

	t.Run("Reads \\ strings too ", func(t *testing.T) {
		src := `"\n \0 \a \" \b \f \r \t \v"`

		var parser Parser

		parser.source = src
		parser.rdOffset = 0
		parser.offset = 0

		parser.nextToken()

		if parser.next.Type != TOKEN_STRING {
			t.Errorf("Expected TOKEN_STRING. got %q", parser.next.Type)
		}

		if parser.next.Content != src {
			t.Errorf("Expected %q; got %q", src, parser.next.Content)
		}
	})

	t.Run("Read String Interpolations ", func(t *testing.T) {
		src := `"%( %(a) )"`

		var parser Parser

		parser.source = src
		parser.rdOffset = 0
		parser.offset = 0

		parser.nextToken()

		if parser.next.Type != TOKEN_INTERPOLATION {
			t.Errorf("Expected TOKEN_INTERPOLATION. got %q", parser.next.Type)
		}

		if parser.numParens != 1 {
			t.Errorf("Want parser.numParenthesis to be %d, got %d", 1, parser.numParens)
		}
	})

	t.Run("Interpolations and strings", func(t *testing.T) {
		// from wren_compiler.c
		src := `"a %(b) c %(d) e"`

		var parser Parser

		parser.source = src
		parser.offset = 0
		parser.rdOffset = 0

		for i, tt := range []struct {
			expectedType    TokenType
			expectedContent string
		}{
			{TOKEN_INTERPOLATION, `"a "`},
			{TOKEN_NAME, "b"},
			{TOKEN_INTERPOLATION, `" c "`},
			{TOKEN_NAME, "d"},
			{TOKEN_STRING, `" e"`},
		} {
			parser.nextToken()

			if parser.next.Type != tt.expectedType {
				t.Errorf("ExpectedType[%d]: %q, got %q", i, tt.expectedType, parser.next.Type)
			}

			if parser.next.Content != tt.expectedContent {
				t.Errorf("ExpectedContent[%d]: %q, got %q", i, tt.expectedContent, parser.next.Content)
			}
		}
	})

}

func TestReadName(t *testing.T) {
	for _, tt := range []struct {
		src          string
		expectedType TokenType
	}{
		{`a`, TOKEN_NAME},
		{`ident`, TOKEN_NAME},

		{"break", TOKEN_BREAK},
		{"continue", TOKEN_CONTINUE},
		{"class", TOKEN_CLASS},
		{"construct", TOKEN_CONSTRUCT},
		{"else", TOKEN_ELSE},
		{"false", TOKEN_FALSE},
		{"for", TOKEN_FOR},
		{"foreign", TOKEN_FOREIGN},
		{"if", TOKEN_IF},
		{"import", TOKEN_IMPORT},
		{"as", TOKEN_AS},
		{"in", TOKEN_IN},
		{"is", TOKEN_IS},
		{"null", TOKEN_NULL},
		{"return", TOKEN_RETURN},
		{"static", TOKEN_STATIC},
		{"super", TOKEN_SUPER},
		{"this", TOKEN_THIS},
		{"true", TOKEN_TRUE},
		{"var", TOKEN_VAR},
		{"while", TOKEN_WHILE},
	} {
		var parser Parser

		parser.source = tt.src
		parser.rdOffset = 0
		parser.offset = 0

		parser.nextToken()

		if parser.next.Type != tt.expectedType {
			t.Errorf("Expected %q; got %q", tt.expectedType, parser.next.Type)
		}

		if parser.next.Content != tt.src {
			t.Errorf("Expected Content %q, got %q", tt.src, parser.next.Content)
		}
	}
}

func TestReadNumber(t *testing.T) {
	for _, tt := range []struct {
		src          string
		expectedType TokenType
	}{
		{"123", TOKEN_NUMBER},
		{"123.123", TOKEN_NUMBER},
		{"123e10", TOKEN_NUMBER},
		{"0010", TOKEN_NUMBER},
		{"0x010", TOKEN_NUMBER},
	} {
		var parser Parser

		parser.source = tt.src
		parser.rdOffset = 0
		parser.offset = 0

		parser.nextToken()

		if parser.next.Type != tt.expectedType {
			t.Errorf("Expected %q. got %q", tt.expectedType, parser.next.Type)
		}

		if parser.next.Content != tt.src {
			t.Errorf("Expected content %q, got %q", tt.src, parser.next.Content)
		}
	}
}
