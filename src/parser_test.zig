const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Node = ast.Node;
const TokenType = token.TokenType;
const testing = std.testing;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var allocator = std.heap.ArenaAllocator.init(gpa.allocator());
var alloc = allocator.allocator(); // the ast Test Deallocates

test "Parser Error" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;
    var p = Parser.init(input, allocator.allocator());

    try testing.expectEqual(p.errors.items.len, 0);
}

test "ParseLetStatements" {
    const input = "let x = 5;\nlet y = 10;\nlet foobar = 838383;";

    var p = Parser.init(input, allocator.allocator());

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
    for (tests) |tc| {
        var p = Parser.init(tc.input, allocator.allocator());

        const program = try p.parseProgram();

        std.debug.assert(program.statements.items.len == 1);

        var node = program.statements.items[0];

        try testing.expectEqual(Node.Id.ReturnStatement, node.id);
    }
}

test "IdentifierExpression" {
    try IdentifierLiteralTest([]const u8).run("foobar;", "foobar");
}

test "IntegerLiteral" {
    try IntegerLiteraltest(u32).run("5;", @as(u32, 5));
}

test "PrefixTests" {
    try PrefixTest(u32).run("!5;", .not, 5);
    try PrefixTest(u32).run("-15;", .minus, 15);
    try PrefixTest(bool).run("!true;", .not, true);
    try PrefixTest(bool).run("!false", .not, false);
}

test "InfixTests" {
    try InfixTest(u32).run("5 + 5;", 5, .plus, 5);
    try InfixTest(u32).run("5 - 5;", 5, .minus, 5);
    try InfixTest(u32).run("5 * 5;", 5, .multiply, 5);
    try InfixTest(u32).run("5 / 5;", 5, .divide, 5);
    try InfixTest(u32).run("5 > 5;", 5, .greater_than, 5);
    try InfixTest(u32).run("5 < 5;", 5, .less_than, 5);
    try InfixTest(u32).run("5 == 5;", 5, .equal_to, 5);
    try InfixTest(u32).run("5 !=5;", 5, .not_equal_to, 5);
    try InfixTest(bool).run("true == true;", true, .equal_to, true);
    try InfixTest(bool).run("true != false;", true, .not_equal_to, false);
    try InfixTest(bool).run("false != false;", false, .not_equal_to, false);
}

test "ToString" {
    try ToStringTest([:0]const u8).run("-a * b", "((-a) * b)");
    try ToStringTest([:0]const u8).run("!-a", "(!(-a))");
    try ToStringTest([:0]const u8).run("a + b + c", "((a + b) + c)");
    try ToStringTest([:0]const u8).run("a + b - c", "((a + b) - c)");
    try ToStringTest([:0]const u8).run("a * b * c", "((a * b) * c)");
    try ToStringTest([:0]const u8).run("a * b / c", "((a * b) / c)");
    try ToStringTest([:0]const u8).run("a + b / c", "(a + (b / c))");
    try ToStringTest([:0]const u8).run("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)");
    try ToStringTest([:0]const u8).run("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))");
    try ToStringTest([:0]const u8).run("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))");
    try ToStringTest([:0]const u8).run("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))");
    // No precedence for ';', thus this should
    // be parsed as two different statements not one.
    // try ToStringTest([:0]const u8).run("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)");
    try ToStringTest([:0]const u8).run("true", "true");
    try ToStringTest([:0]const u8).run("false", "false");
    try ToStringTest([:0]const u8).run("3 > 5 == false", "((3 > 5) == false)");
    try ToStringTest([:0]const u8).run("3 < 5 == true", "((3 < 5) == true)");
    try ToStringTest([:0]const u8).run("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)");
    try ToStringTest([:0]const u8).run("(5 + 5) * 2", "((5 + 5) * 2)");
    try ToStringTest([:0]const u8).run("2 / (5 + 5)", "(2 / (5 + 5))");
    try ToStringTest([:0]const u8).run("-(5 + 5)", "(-(5 + 5))");
    try ToStringTest([:0]const u8).run("!(true == true)", "(!(true == true))");
}
////////// Helpers /////////////////////////
pub fn ToStringTest(comptime T: type) type {
    return struct {
        pub fn run(comptime input: T, comptime expected: []const u8) !void {
            var buf = std.ArrayList(u8).init(alloc);
            defer buf.deinit();

            var par = Parser.init(input, alloc);

            var program = try par.parseProgram();

            try program.toString(buf.writer());

            try testing.expectEqualStrings(expected, buf.items);

            buf.clearRetainingCapacity();
        }
    };
}

pub fn IdentifierLiteralTest(comptime T: type) type {
    return struct {
        pub fn run(comptime input: [:0]const u8, comptime value: []const u8) !void {
            var par = Parser.init(input, allocator.allocator());

            var program = try par.parseProgram();

            try testing.expect(program.statements.items.len == 1);

            try testLiteralExpressions(T, program.statements.items[0], value);
        }
    };
}

fn IntegerLiteraltest(comptime T: type) type {
    return struct {
        pub fn run(comptime input: [:0]const u8, comptime value: T) !void {
            var par = Parser.init(input, allocator.allocator());

            var program = try par.parseProgram();

            try testing.expect(program.statements.items.len == 1);

            try testLiteralExpressions(T, program.statements.items[0], value);
        }
    };
}

fn testIdentifierLiteral(node: *Node, val: []const u8) !void {
    try testing.expectEqual(Node.Id.Identifier, node.id);

    const identifierNode = node.cast(Node.Id.Identifier).?;

    try testing.expectEqualStrings(val, identifierNode.token.Literal);
}

fn testIntegerLiteral(node: *Node, value: u32) !void {
    var buf: [1024]u8 = undefined;

    const stmt = Node.cast(node, Node.Id.IntegerLiteral).?;

    try testing.expectEqual(Node.IntegerLiteral, @TypeOf(stmt.*));

    const int = stmt.value;

    try testing.expectEqual(int, value);

    try testing.expectEqualStrings(stmt.token.Literal, try std.fmt.bufPrint(&buf, "{d}", .{value}));
}

fn testLiteralExpressions(comptime T: type, node: *Node, comptime val: T) !void {
    return switch (T) {
        u32 => try testIntegerLiteral(node, val),
        bool => try testBooleanLiteral(node, val),
        []const u8 => try testIdentifierLiteral(node, val),
        else => @compileError("Literal Expression of type  '" ++ @typeName(@TypeOf(val)) ++ "' is not handled"),
    };
}

fn testInfixExpressions(comptime T: type, node: *Node, comptime left: T, comptime op: ast.Operator, comptime right: T) !void {
    try testing.expectEqual(Node.Id.InfixExpression, node.id);

    const expr = node.cast(Node.Id.InfixExpression).?;

    try testLiteralExpressions(T, expr.leftExprPtr.?, left);

    try testing.expectEqual(op, expr.operator);

    try testLiteralExpressions(T, expr.rightExprPtr.?, right);
}

fn testBooleanLiteral(node: *Node, value: bool) !void {
    try testing.expectEqual(Node.Id.Boolean, node.id);

    const expr = node.cast(.Boolean).?;

    try testing.expectEqual(value, expr.value);
}

pub fn PrefixTest(comptime T: type) type {
    return struct {
        pub fn run(comptime input: [:0]const u8, comptime op: ast.Operator, comptime value: T) !void {
            var par = Parser.init(input, alloc);

            var program = try par.parseProgram();

            //assert
            try testing.expect(1 == program.statements.items.len);

            const node = program.statements.items[0];

            try testing.expectEqual(Node.Id.PrefixExpression, node.id);

            const prefixExpressionNode = Node.cast(node, Node.Id.PrefixExpression).?;

            try testing.expectEqual(op, prefixExpressionNode.operator);

            try testLiteralExpressions(T, prefixExpressionNode.rightExprPtr.?, value);
        }
    };
}

pub fn InfixTest(comptime T: type) type {
    return struct {
        pub fn run(comptime input: [:0]const u8, comptime left: T, comptime op: ast.Operator, comptime right: T) !void {
            var par = Parser.init(input, alloc);

            var program = try par.parseProgram();

            try testing.expectEqual(@as(usize, 1), program.statements.items.len);

            const node = program.statements.items[0];

            // assert
            try testInfixExpressions(T, node, left, op, right);
        }
    };
}
