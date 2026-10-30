const std = @import("std");
const testing = std.testing;
const token = @import("token.zig");
const TokenType = token.TokenType;
const Lexer = @import("lexer.zig").Lexer;
const mem = std.mem;

pub const Test = struct {
    expectedType: token.TokenType,
    expectedLiteral: []const u8,
};

// zigfmt: Off
pub const booleanOperators = [_]Test{
    .{ .expectedType = .EQ, .expectedLiteral = "==" },
    .{ .expectedType = .NOT_EQ, .expectedLiteral = "!=" },
};
// zigfmt: On

// .expectedType = token.IF, .expectedLiteral = "if" }, .{ .expectedType = token.ELSE, .expectedLiteral = "else" }, .{ .expectedType = token.RETURN, .expectedLiteral = "return" },
pub const variableAssignmentTets = [_]Test{
    .{ .expectedType = .LET, .expectedLiteral = "let" },
    .{ .expectedType = .IDENT, .expectedLiteral = "five" },
    .{ .expectedType = .ASSIGN, .expectedLiteral = "=" },
    .{ .expectedType = .INT, .expectedLiteral = "5" },
    .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
    .{ .expectedType = .LET, .expectedLiteral = "let" },
    .{ .expectedType = .IDENT, .expectedLiteral = "ten" },
    .{ .expectedType = .ASSIGN, .expectedLiteral = "=" },
    .{ .expectedType = .INT, .expectedLiteral = "10" },
    .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
    .{ .expectedType = .LET, .expectedLiteral = "let" },
    .{ .expectedType = .IDENT, .expectedLiteral = "add" },
    .{ .expectedType = .ASSIGN, .expectedLiteral = "=" },
    .{ .expectedType = .FUNCTION, .expectedLiteral = "fn" },
    .{ .expectedType = .LPAREN, .expectedLiteral = "(" },
    .{ .expectedType = .IDENT, .expectedLiteral = "x" },
    .{ .expectedType = .COMMA, .expectedLiteral = "," },
    .{ .expectedType = .IDENT, .expectedLiteral = "y" },
    .{ .expectedType = .RPAREN, .expectedLiteral = ")" },
    .{ .expectedType = .LET, .expectedLiteral = "let" },
    .{ .expectedType = .IDENT, .expectedLiteral = "result" },
    .{ .expectedType = .ASSIGN, .expectedLiteral = "=" },
    .{ .expectedType = .IDENT, .expectedLiteral = "add" },
    .{ .expectedType = .LPAREN, .expectedLiteral = "(" },
    .{ .expectedType = .IDENT, .expectedLiteral = "five" },
    .{ .expectedType = .COMMA, .expectedLiteral = "," },
    .{ .expectedType = .IDENT, .expectedLiteral = "ten" },
    .{ .expectedType = .RPAREN, .expectedLiteral = ")" },
    .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
};

pub const delimiterTests = [_]Test{
    .{ .expectedType = .ASSIGN, .expectedLiteral = "=" },
    .{ .expectedType = .PLUS, .expectedLiteral = "+" },
    .{ .expectedType = .LPAREN, .expectedLiteral = "(" },
    .{ .expectedType = .RPAREN, .expectedLiteral = ")" },
    .{ .expectedType = .LBRACE, .expectedLiteral = "{" },
    .{ .expectedType = .RBRACE, .expectedLiteral = "}" },
    .{ .expectedType = .COMMA, .expectedLiteral = "," },
    .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
};

pub const bitWiseOperators = [_]Test{
    .{ .expectedType = .OR, .expectedLiteral = "|" },
    .{ .expectedType = .AND, .expectedLiteral = "&" },
    .{ .expectedType = .XOR, .expectedLiteral = "^" },
};

pub const arithmeticOperators = [_]Test{
    .{ .expectedType = .LBRACE, .expectedLiteral = "{" },
    .{ .expectedType = .IDENT, .expectedLiteral = "x" },
    .{ .expectedType = .PLUS, .expectedLiteral = "+" },
    .{ .expectedType = .IDENT, .expectedLiteral = "y" },
    .{ .expectedType = .RBRACE, .expectedLiteral = "}" },
    .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
    .{ .expectedType = .BANG, .expectedLiteral = "!" },
    .{ .expectedType = .MINUS, .expectedLiteral = "-" },
    .{ .expectedType = .SLASH, .expectedLiteral = "/" },
    .{ .expectedType = .ASTERISK, .expectedLiteral = "*" },
    .{ .expectedType = .INT, .expectedLiteral = "5" },
    .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
    .{ .expectedType = .INT, .expectedLiteral = "5" },
    .{ .expectedType = .LT, .expectedLiteral = "<" },
    .{ .expectedType = .INT, .expectedLiteral = "10" },
    .{ .expectedType = .GT, .expectedLiteral = ">" },
    .{ .expectedType = .INT, .expectedLiteral = "5" },
};

/////Tests//////////////////

test "Strings" {
    const input = "\"foobar\"";
    var lex = Lexer.init(input);
    const tok = lex.nextToken();
    try testing.expectEqualStrings("foobar", tok.Literal);
}

test "LexerEOF" {
    const input = "";
    var lex = Lexer.init(input);
    const tok = lex.nextToken();
    try testing.expect(TokenType.EOF == tok.Type);
}

test "DelimeterTest" {
    const input = "=+(){},;";
    var lex = Lexer.init(input);

    try run_test(&delimiterTests, &lex);
}

test "BitWiseOperators" {
    const input = "|&^";
    var lex = Lexer.init(input);

    try run_test(&bitWiseOperators, &lex);
}

test "BooleanoperatorTests" {
    const input = "== !=";
    var lex = Lexer.init(input);

    try run_test(&booleanOperators, &lex);
}

test "ArithmeticTests" {
    const input = "{x + y};\n!-/*5;\n5 < 10 > 5";
    var lex = Lexer.init(input);

    try run_test(&arithmeticOperators, &lex);
}

test "VariableAssignmentTest" {
    const input = "let five = 5;\nlet ten = 10;\nlet add = fn(x, y)\nlet result = add(five, ten);";
    var lex = Lexer.init(input);

    try run_test(&variableAssignmentTets, &lex);
}

test "OperatorsWithoutSpaces" {
    const input = "let a=5;a+b-c*d/e<f>g==h!=i";
    var lex = Lexer.init(input);

    try run_test(&[_]Test{
        .{ .expectedType = .LET, .expectedLiteral = "let" },
        .{ .expectedType = .IDENT, .expectedLiteral = "a" },
        .{ .expectedType = .ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = .INT, .expectedLiteral = "5" },
        .{ .expectedType = .SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = .IDENT, .expectedLiteral = "a" },
        .{ .expectedType = .PLUS, .expectedLiteral = "+" },
        .{ .expectedType = .IDENT, .expectedLiteral = "b" },
        .{ .expectedType = .MINUS, .expectedLiteral = "-" },
        .{ .expectedType = .IDENT, .expectedLiteral = "c" },
        .{ .expectedType = .ASTERISK, .expectedLiteral = "*" },
        .{ .expectedType = .IDENT, .expectedLiteral = "d" },
        .{ .expectedType = .SLASH, .expectedLiteral = "/" },
        .{ .expectedType = .IDENT, .expectedLiteral = "e" },
        .{ .expectedType = .LT, .expectedLiteral = "<" },
        .{ .expectedType = .IDENT, .expectedLiteral = "f" },
        .{ .expectedType = .GT, .expectedLiteral = ">" },
        .{ .expectedType = .IDENT, .expectedLiteral = "g" },
        .{ .expectedType = .EQ, .expectedLiteral = "==" },
        .{ .expectedType = .IDENT, .expectedLiteral = "h" },
        .{ .expectedType = .NOT_EQ, .expectedLiteral = "!=" },
        .{ .expectedType = .IDENT, .expectedLiteral = "i" },
        .{ .expectedType = .EOF, .expectedLiteral = "" },
    }, &lex);
}

test "NumbersStopAtNonDigits" {
    const input = "5<10 5!=6 5abc";
    var lex = Lexer.init(input);

    try run_test(&[_]Test{
        .{ .expectedType = .INT, .expectedLiteral = "5" },
        .{ .expectedType = .LT, .expectedLiteral = "<" },
        .{ .expectedType = .INT, .expectedLiteral = "10" },
        .{ .expectedType = .INT, .expectedLiteral = "5" },
        .{ .expectedType = .NOT_EQ, .expectedLiteral = "!=" },
        .{ .expectedType = .INT, .expectedLiteral = "6" },
        .{ .expectedType = .INT, .expectedLiteral = "5" },
        .{ .expectedType = .IDENT, .expectedLiteral = "abc" },
    }, &lex);
}

test "IdentifierCharacters" {
    const input = "my_var x1 _tmp\nnext\tlast";
    var lex = Lexer.init(input);

    try run_test(&[_]Test{
        .{ .expectedType = .IDENT, .expectedLiteral = "my_var" },
        .{ .expectedType = .IDENT, .expectedLiteral = "x1" },
        .{ .expectedType = .IDENT, .expectedLiteral = "_tmp" },
        .{ .expectedType = .IDENT, .expectedLiteral = "next" },
        .{ .expectedType = .IDENT, .expectedLiteral = "last" },
        .{ .expectedType = .EOF, .expectedLiteral = "" },
    }, &lex);
}

test "UnterminatedStringIsIllegal" {
    const input = "\"abc";
    var lex = Lexer.init(input);

    try run_test(&[_]Test{
        .{ .expectedType = .ILLEGAL, .expectedLiteral = "ILLEGAL" },
        .{ .expectedType = .EOF, .expectedLiteral = "" },
    }, &lex);
}

test "StringEdgeCases" {
    // Strings have no escape sequences: a backslash is kept as-is.
    const input = "\"\" \"a\\nb\" \"a b\"";
    var lex = Lexer.init(input);

    try run_test(&[_]Test{
        .{ .expectedType = .STRING, .expectedLiteral = "" },
        .{ .expectedType = .STRING, .expectedLiteral = "a\\nb" },
        .{ .expectedType = .STRING, .expectedLiteral = "a b" },
        .{ .expectedType = .EOF, .expectedLiteral = "" },
    }, &lex);
}

test "IllegalCharactersAndRepeatedEOF" {
    const input = "@ é";
    var lex = Lexer.init(input);

    try run_test(&[_]Test{
        .{ .expectedType = .ILLEGAL, .expectedLiteral = "ILLEGAL" },
        // 'é' is two UTF-8 bytes, each lexed as its own ILLEGAL token.
        .{ .expectedType = .ILLEGAL, .expectedLiteral = "ILLEGAL" },
        .{ .expectedType = .ILLEGAL, .expectedLiteral = "ILLEGAL" },
        .{ .expectedType = .EOF, .expectedLiteral = "" },
        .{ .expectedType = .EOF, .expectedLiteral = "" },
    }, &lex);
}

fn run_test(expectedTokens: []const Test, lex: *Lexer) !void {
    for (expectedTokens) |tc| {
        const tok = lex.nextToken();
        try testing.expectEqual(tc.expectedType, tok.Type);
        try testing.expectEqualStrings(tc.expectedLiteral, tok.Literal);
    }
}
