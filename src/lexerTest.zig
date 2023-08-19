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

pub const booleanOps = [_]Test{ .{ .expectedType = .EQ, .expectedLiteral = "==" }, .{ .expectedType = .NOT_EQ, .expectedLiteral = "!=" } };
// .expectedType = token.IF, .expectedLiteral = "if" }, .{ .expectedType = token.ELSE, .expectedLiteral = "else" }, .{ .expectedType = token.RETURN, .expectedLiteral = "return" },
pub const statementTest = [_]Test{
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

pub const arithmeticOps = [_]Test{
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

    for (delimiterTests) |tc| {
        var tok = lex.nextToken();

        try testing.expectEqual(tc.expectedType, tok.Type);
        try testing.expectEqualStrings(tc.expectedLiteral, tok.Literal);
    }
}
test "BooleanoperatorTests" {
    const input = "== !=";

    var lex = Lexer.init(input);

    for (booleanOps) |tc| {
        var tok = lex.nextToken();

        try testing.expectEqual(tc.expectedType, tok.Type);
        try testing.expectEqualStrings(tc.expectedLiteral, tok.Literal);
    }
}

test "ArithmeticTests" {
    const input = "{x + y};\n!-/*5;\n5 < 10 > 5";

    var lex = Lexer.init(input);

    for (arithmeticOps) |tc| {
        var tok = lex.nextToken();
        try testing.expectEqual(tc.expectedType, tok.Type);
        try testing.expectEqualStrings(tc.expectedLiteral, tok.Literal);
    }
}

test "VariableAssignmentTest" {
    const input = "let five = 5;\nlet ten = 10;\nlet add = fn(x, y)\nlet result = add(five, ten);";

    var lex = Lexer.init(input);

    for (statementTest) |tc| {
        var tok = lex.nextToken();

        try testing.expectEqual(tc.expectedType, tok.Type);
        try testing.expectEqualStrings(tc.expectedLiteral, tok.Literal);
    }
}
