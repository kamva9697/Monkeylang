const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const TokenType = token.TokenType;
const testing = std.testing;

test "Parser Error" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;
    var l = Lexer.init(input);

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = std.heap.ArenaAllocator.init(gpa.allocator());
    var p = Parser.init(l, allocator.allocator());
    defer allocator.deinit();

    try testing.expectEqual(p.errors.items.len, 0);
}

test "ParseLetStatements" {
    const input = "let x = 5;\nlet y = 10;\nlet foobar = 838383;";

    var l = Lexer.init(input);

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = std.heap.ArenaAllocator.init(gpa.allocator());
    var p = Parser.init(l, allocator.allocator());
    defer p.deinit();

    const program = p.parseProgram();
    defer program.deinit();

    const tests = [_][]const u8{
        "x",
        "y",
        "foobar",
    };

    try testing.expectEqual(program.statements.items.len, tests.len);

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
    var allocator = std.heap.ArenaAllocator.init(gpa.allocator());
    for (tests) |tc| {
        var l = Lexer.init(tc.input);
        var p = Parser.init(l, allocator.allocator());
        defer p.deinit();

        const program = p.parseProgram();

        std.debug.assert(program.statements.items.len == 1);

        var node = program.statements.items[0];

        try testing.expectEqual(TokenType.RETURN, node.statement.returnStatement.token.Type);
    }
}

test "IdentifierExpression: Nodes" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = std.heap.ArenaAllocator.init(gpa.allocator());
    var input = "foobar;";
    var lex = Lexer.init(input);
    var par = Parser.init(lex, allocator.allocator());
    defer allocator.deinit();

    var program = par.parseProgram();

    try testing.expect(program.statements.items.len == 1);

    const stmt = program.statements.items[0];

    try testing.expectEqual(ast.ExpressionStatement, @TypeOf(stmt.statement.expressionStatement.*));
    try testing.expectEqual(ast.Identifier, @TypeOf(stmt.statement.expressionStatement.*.expression.?.identifier.*));
}

test "IdentifierExpression: token equality" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = std.heap.ArenaAllocator.init(gpa.allocator());
    var input = "foobar;";
    var lex = Lexer.init(input);
    var par = Parser.init(lex, allocator.allocator());
    defer par.deinit();

    var program = par.parseProgram();

    const statementsLen = program.statements.items.len;

    try testing.expect(statementsLen == 1);

    const node = program.statements.items[0];

    try testing.expectEqualStrings("foobar", node.statement.expressionStatement.token.Literal);
}

test "Debug_ParseExpression" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = std.heap.ArenaAllocator.init(gpa.allocator());
    var input = "foobar;";
    var lex = Lexer.init(input);
    var par = Parser.init(lex, allocator.allocator());
    defer allocator.deinit();

    const foobar: []const u8 = "foobar";

    var identPtr = try allocator.allocator().create(ast.Identifier);
    identPtr.* = ast.Identifier{ .token = .{ .Type = .IDENT, .Literal = foobar[0..] }, .value = foobar[0..] };

    const expectedNode = ast.Expression{ .identifier = identPtr };

    const node = par.parseExpression(.LOWEST).?;
    try testing.expectEqual(@TypeOf(expectedNode), @TypeOf(node));
    try testing.expectEqual(@TypeOf(expectedNode.identifier), @TypeOf(node.identifier));
    try testing.expectEqual(expectedNode.identifier.token.Type, node.identifier.token.Type);
}

test "IdentifierExpression_2" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = std.heap.ArenaAllocator.init(gpa.allocator());
    var input = "foobar;";
    var lex = Lexer.init(input);
    var par = Parser.init(lex, allocator.allocator());
    defer allocator.deinit();

    var program = par.parseProgram();

    try testing.expect(1 == program.statements.items.len);

    const node = program.statements.items[0];
    const identLiteral = node.statement.expressionStatement.*.expression.?;

    try testing.expectEqualStrings("foobar", identLiteral.identifier.value);
}

test "IntegerLiteral" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = std.heap.ArenaAllocator.init(gpa.allocator());
    var input = "5;";
    var lex = Lexer.init(input);
    var par = Parser.init(lex, allocator.allocator());
    defer allocator.deinit();

    var program = par.parseProgram();

    try testing.expect(program.statements.items.len == 1);

    const stmt = program.statements.items[0];

    try testing.expectEqual(ast.ExpressionStatement, @TypeOf(stmt.statement.expressionStatement.*));
    try testing.expectEqual(ast.IntegerLiteral, @TypeOf(stmt.statement.expressionStatement.*.expression.?.integerLiteral.*));
}

test "IntegerLiteral-2" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = std.heap.ArenaAllocator.init(gpa.allocator());
    var input = "5;";
    var lex = Lexer.init(input);
    var par = Parser.init(lex, allocator.allocator());
    defer allocator.deinit();

    var program = par.parseProgram();

    try testing.expect(1 == program.statements.items.len);

    const node = program.statements.items[0];

    try testing.expectEqual(@as(u64, 5), node.statement.expressionStatement.expression.?.integerLiteral.value);

    try testing.expectEqualStrings("5", node.statement.expressionStatement.expression.?.integerLiteral.token.Literal);
}

test "ParsingPrefixExpression" {
    const PrefixTests = struct { input: [:0]const u8, operator: []const u8, integerValue: u32 };

    var tests: [2]PrefixTests = .{
        .{ .input = "!5;", .operator = "!", .integerValue = 5 },
        .{ .input = "-15;", .operator = "-", .integerValue = 15 },
    };

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = std.heap.ArenaAllocator.init(gpa.allocator());
    defer allocator.deinit();

    var alloc = allocator.allocator();

    for (tests) |tc| {
        var lex = Lexer.init(tc.input);
        var par = Parser.init(lex, alloc);
        defer par.deinit();

        var program = par.parseProgram();

        //assert
        try testing.expect(1 == program.statements.items.len);

        const node = program.statements.items[0];

        try testing.expectEqual(ast.ExpressionStatement, @TypeOf(node.statement.expressionStatement.*));

        const expr = node.statement.expressionStatement.*.expression.?.prefixExpression;

        try testing.expectEqual(ast.PrefixExpression, @TypeOf(expr.*));

        try testing.expectEqual(tc.operator, expr.operator);

        try testIntegerLiteral(alloc, expr.rightExprPtr.*.?, tc.integerValue);
    }
}

fn testIntegerLiteral(allocator: std.mem.Allocator, stmt: ast.Expression, value: u32) !void {
    try testing.expectEqual(ast.IntegerLiteral, @TypeOf(stmt.integerLiteral.*));

    try testing.expectEqual(stmt.integerLiteral.*.value, value);

    try testing.expectEqualStrings(stmt.integerLiteral.*.TokenLiteral(), try std.fmt.allocPrint(allocator, "{d}", .{value}));
}
