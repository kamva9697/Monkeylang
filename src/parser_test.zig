const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Node = ast.Node;
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

    const program = try p.parseProgram();
    // defer program.deinit();

    const tests = [_][]const u8{
        "x",
        "y",
        "foobar",
    };

    try testing.expectEqual(program.statements.items.len, tests.len);

    for (tests, 0..) |tc, i| {
        var node = program.statements.items[i];
        try testing.expectEqual(Node.Id.LetStatement, node.id);
        try testing.expectEqual(TokenType.LET, node.tokenType());
        const letNode = @fieldParentPtr(Node.LetStatement, "base", node);
        try testing.expectEqualStrings(tc, letNode.*.name.value);
    }
}

test "ParseReturnStatements" {
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

        const program = try p.parseProgram();

        std.debug.assert(program.statements.items.len == 1);

        var node = program.statements.items[0];

        try testing.expectEqual(Node.Id.ReturnStatement, node.id);
    }
}

test "IdentifierExpression" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = std.heap.ArenaAllocator.init(gpa.allocator());
    var input = "foobar;";
    var lex = Lexer.init(input);
    var par = Parser.init(lex, allocator.allocator());
    defer allocator.deinit();

    var program = try par.parseProgram();

    try testing.expect(program.statements.items.len == 1);

    try testLiteralExpressions(program.statements.items[0], "foobar");
}

test "IntegerLiteral" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = std.heap.ArenaAllocator.init(gpa.allocator());
    const input = "5;";
    var lex = Lexer.init(input);
    var par = Parser.init(lex, allocator.allocator());
    defer allocator.deinit();

    var program = try par.parseProgram();

    try testing.expect(program.statements.items.len == 1);

    try testLiteralExpressions(program.statements.items[0], @as(u32, 5));
}

test "PrefixTests" {
    const PrefixTests = struct { input: [:0]const u8, operator: ast.Operator, integerValue: u32 };

    const tests: [2]PrefixTests = .{
        .{ .input = "!5;", .operator = .not, .integerValue = 5 },
        .{ .input = "-15;", .operator = .minus, .integerValue = 15 },
    };

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = std.heap.ArenaAllocator.init(gpa.allocator());
    defer allocator.deinit();

    var alloc = allocator.allocator();

    for (tests) |tc| {
        var lex = Lexer.init(tc.input);
        var par = Parser.init(lex, alloc);
        defer par.deinit();

        var program = try par.parseProgram();

        //assert
        try testing.expect(1 == program.statements.items.len);

        const node = program.statements.items[0];

        try testing.expectEqual(Node.Id.PrefixExpression, node.id);

        const prefixExpressionNode = Node.cast(node, Node.Id.PrefixExpression).?;

        try testing.expectEqual(tc.operator, prefixExpressionNode.operator);

        try testLiteralExpressions(prefixExpressionNode.rightExprPtr.?, tc.integerValue);
    }
}

test "InfixTests" {
    const InfixStmt = struct { input: [:0]const u8, left: u32, op: ast.Operator, right: u32 };
    const tests = [_]InfixStmt{
        .{ .input = "5 + 5;", .left = 5, .op = .plus, .right = 5 },
        .{ .input = "5 - 5;", .left = 5, .op = .minus, .right = 5 },
        .{ .input = "5 * 5;", .left = 5, .op = .multiply, .right = 5 },
        .{ .input = "5 / 5;", .left = 5, .op = .divide, .right = 5 },
        .{ .input = "5 > 5;", .left = 5, .op = .greater_than, .right = 5 },
        .{ .input = "5 < 5;", .left = 5, .op = .less_than, .right = 5 },
        .{ .input = "5 == 5;", .left = 5, .op = .equal_to, .right = 5 },
        .{ .input = "5 != 5;", .left = 5, .op = .not_equal_to, .right = 5 },
    };

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = std.heap.ArenaAllocator.init(gpa.allocator());
    defer allocator.deinit();

    var alloc = allocator.allocator();

    for (tests) |tc| {
        var lex = Lexer.init(tc.input);
        var par = Parser.init(lex, alloc);
        defer par.deinit();

        var program = try par.parseProgram();

        try testing.expectEqual(@as(usize, 1), program.statements.items.len);

        const node = program.statements.items[0];

        // assert
        try testInfixExpressions(node, tc.left, tc.op, tc.right);
    }
}

test "ToString" {
    const Tests = [_]struct {
        input: [:0]const u8,
        expected: []const u8,
    }{
        .{
            .input = "-a * b",
            .expected = "((-a) * b)",
        },
        .{
            .input = "!-a",
            .expected = "(!(-a))",
        },
        .{
            .input = "a + b + c",
            .expected = "((a + b) + c)",
        },
        .{
            .input = "a + b - c",
            .expected = "((a + b) - c)",
        },
        .{
            .input = "a * b * c",
            .expected = "((a * b) * c)",
        },
        .{
            .input = "a * b / c",
            .expected = "((a * b) / c)",
        },
        .{
            .input = "a + b / c",
            .expected = "(a + (b / c))",
        },
        .{
            .input = "a + b * c + d / e - f",
            .expected = "(((a + (b * c)) + (d / e)) - f)",
        },
        // No precedence for ';', thus this should
        // be parsed as two different statements not one.
        // .{
        //     .input = "3 + 4; -5 * 5",
        //     .expected = "(3 + 4)((-5) * 5)",
        // },
        .{
            .input = "5 > 4 == 3 < 4",
            .expected = "((5 > 4) == (3 < 4))",
        },
        .{
            .input = "5 < 4 != 3 > 4",
            .expected = "((5 < 4) != (3 > 4))",
        },
        .{
            .input = "3 + 4 * 5 == 3 * 1 + 4 * 5",
            .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        },
        .{
            .input = "3 + 4 * 5 == 3 * 1 + 4 * 5",
            .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        },
    };

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var alloc = gpa.allocator();
    var buf = std.ArrayList(u8).init(alloc);

    for (Tests) |tc| {
        var lex = Lexer.init(tc.input);
        var par = Parser.init(lex, alloc);
        defer par.deinit();

        var program = try par.parseProgram();

        try program.toString(buf.writer());

        try testing.expectEqualStrings(tc.expected, buf.items);

        buf.clearRetainingCapacity();
    }
}

fn testIdentifier(node: *Node, expected: anytype) !void {
    try testing.expectEqual(Node.Id.Identifier, node.id);

    const identifierNode = node.cast(Node.Id.Identifier).?;

    try testing.expectEqualStrings(expected, identifierNode.token.Literal);
}

fn testIntegerLiteral(node: *Node, value: anytype) !void {
    var buf: [1024]u8 = undefined;

    const stmt = Node.cast(node, Node.Id.IntegerLiteral).?;

    try testing.expectEqual(Node.IntegerLiteral, @TypeOf(stmt.*));

    const int = stmt.value;

    try testing.expectEqual(int, value);

    try testing.expectEqualStrings(stmt.token.Literal, try std.fmt.bufPrint(&buf, "{d}", .{value}));
}

fn testLiteralExpressions(node: *Node, expected: anytype) !void {
    return switch (@typeInfo(@TypeOf(expected))) {
        .Int => try testIntegerLiteral(node, expected),
        .Pointer, .Array => try testIdentifier(node, expected),
        else => @compileError("Literal Expression of type {" ++ @typeName(@TypeOf(expected)) ++ "} is not handled"),
    };
}

fn testInfixExpressions(node: *Node, left: anytype, op: ast.Operator, right: anytype) !void {
    try testing.expectEqual(Node.Id.InfixExpression, node.id);

    const expr = node.cast(Node.Id.InfixExpression).?;

    try testLiteralExpressions(expr.leftExprPtr.?, left);

    try testing.expectEqual(op, expr.operator);

    try testLiteralExpressions(expr.rightExprPtr.?, right);
}
