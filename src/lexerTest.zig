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

test "LexerEOF" {
    const input = "";
    var lex = Lexer.init(input);
    var tok = lex.nextToken();
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

fn run_test(expectedTokens: []const Test, lex: *Lexer) !void {
    for (expectedTokens) |tc| {
        var tok = lex.nextToken();
        try testing.expectEqual(tc.expectedType, tok.Type);
        try testing.expectEqualStrings(tc.expectedLiteral, tok.Literal);
    }
}
