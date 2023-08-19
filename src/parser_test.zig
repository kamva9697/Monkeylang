const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const TokenType = token.TokenType;
const testing = std.testing;

test "Parser Error" {
    const input = "let = 5;\nlet y = 10;\nlet foobar = 838383;\n";

    var l = Lexer.init(input);

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var p = Parser.init(l, gpa.allocator());
    defer p.deinit();

    const program = p.parseProgram(gpa.allocator());
    defer program.deinit();

    try testing.expect(p.errors.items.len > 0);
}

test "Parse: Let Statements" {
    const input = "let x = 5;\nlet y = 10;\nlet foobar = 838383;\n";

    var l = Lexer.init(input);

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var p = Parser.init(l, gpa.allocator());
    defer p.deinit();

    const program = p.parseProgram(gpa.allocator());
    defer program.deinit();

    std.debug.assert(program.statements.items.len == 3);

    const tests = [_][]const u8{
        "x",
        "y",
        "foobar",
    };

    for (tests, 0..) |tc, i| {
        var node = program.statements.items[i];
        try testing.expectEqual(@TypeOf(node.statement), ast.Statement);
        try testing.expect(node.statement.tokenType() == .LET);
        try testing.expectEqualStrings(tc, node.statement.letStatement.name.value);
    }
}

test "Parse Return Statements" {
    const tests = [_]struct {
        input: [:0]const u8,
        expectedValue: []const u8,
    }{
        .{ .input = "return 5;", .expectedValue = "5" },
        .{ .input = "return true;", .expectedValue = "true" },
        .{ .input = "return foobar;", .expectedValue = "foobar" },
    };

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    for (tests) |tc| {
        var l = Lexer.init(tc.input);
        var p = Parser.init(l, gpa.allocator());
        defer p.deinit();

        const program = p.parseProgram(gpa.allocator());
        defer program.deinit();

        std.debug.assert(program.statements.items.len == 1);

        var node = program.statements.items[0];

        try testing.expectEqual(TokenType.RETURN, node.statement.returnStatement.token.Type);
    }
}
