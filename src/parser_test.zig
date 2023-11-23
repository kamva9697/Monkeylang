const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Node = ast.Node;
const TokenType = token.TokenType;
const testing = std.testing;

/// Global Allocator
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var allocator = std.heap.ArenaAllocator.init(gpa.allocator());
var alloc = allocator.allocator(); // the ast Test Deallocates

test "Parser Error" {
    const input = "let = 5";
    var p = Parser.init(input, allocator.allocator());
    checkParserErrors(&p);
}

test "ParseLetStatements" {
    try testLetStatements(
        u32,
        "let x = 5;",
        "x",
        5,
    );
    try testLetStatements(
        bool,
        "let y = true;",
        "y",
        true,
    );
    try testLetStatements(
        []const u8,
        "let foobar = y;",
        "foobar",
        "y",
    );
}

test "ParseReturnStatements" {
    try testReturnStatement();
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
    try InfixTest(u32).run(
        "5 + 5;",
        5,
        .plus,
        5,
    );
    try InfixTest(u32).run(
        "5 - 5;",
        5,
        .minus,
        5,
    );
    try InfixTest(u32).run(
        "5 * 5;",
        5,
        .multiply,
        5,
    );
    try InfixTest(u32).run(
        "5 / 5;",
        5,
        .divide,
        5,
    );
    try InfixTest(u32).run(
        "5 > 5;",
        5,
        .greater_than,
        5,
    );
    try InfixTest(u32).run(
        "5 < 5;",
        5,
        .less_than,
        5,
    );
    try InfixTest(u32).run(
        "5 == 5;",
        5,
        .equal_to,
        5,
    );
    try InfixTest(u32).run(
        "5 !=5;",
        5,
        .not_equal_to,
        5,
    );
    try InfixTest(bool).run(
        "true == true;",
        true,
        .equal_to,
        true,
    );
    try InfixTest(bool).run(
        "true != false;",
        true,
        .not_equal_to,
        false,
    );
    try InfixTest(bool).run(
        "false != false;",
        false,
        .not_equal_to,
        false,
    );
}

test "Precedence-ToString" {
    try ToStringTest(
        "a + add(b * c) + d",
        "((a + add((b * c))) + d)",
    );
    try ToStringTest(
        "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
        "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
    );
    try ToStringTest(
        "add(a + b + c * d / f + g)",
        "add((((a + b) + ((c * d) / f)) + g))",
    );
    try ToStringTest(
        "-a * b",
        "((-a) * b)",
    );
    try ToStringTest(
        "!-a",
        "(!(-a))",
    );
    try ToStringTest(
        "a + b + c",
        "((a + b) + c)",
    );
    try ToStringTest(
        "a + b - c",
        "((a + b) - c)",
    );
    try ToStringTest(
        "a * b * c",
        "((a * b) * c)",
    );
    try ToStringTest(
        "a * b / c",
        "((a * b) / c)",
    );
    try ToStringTest(
        "a + b / c",
        "(a + (b / c))",
    );
    try ToStringTest(
        "a + b * c + d / e - f",
        "(((a + (b * c)) + (d / e)) - f)",
    );
    try ToStringTest(
        "5 > 4 == 3 < 4",
        "((5 > 4) == (3 < 4))",
    );
    try ToStringTest(
        "5 < 4 != 3 > 4",
        "((5 < 4) != (3 > 4))",
    );
    try ToStringTest(
        "3 + 4 * 5 == 3 * 1 + 4 * 5",
        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
    );
    // No precedence for ';', thus this should
    // be parsed as two different statements not one.
    // try ToStringTest([:0]const u8"3 + 4; -5 * 5", "(3 + 4)((-5) * 5)",);
    try ToStringTest(
        "true",
        "true",
    );
    try ToStringTest(
        "false",
        "false",
    );
    try ToStringTest(
        "3 > 5 == false",
        "((3 > 5) == false)",
    );
    try ToStringTest(
        "3 < 5 == true",
        "((3 < 5) == true)",
    );
    try ToStringTest(
        "1 + (2 + 3) + 4",
        "((1 + (2 + 3)) + 4)",
    );
    try ToStringTest(
        "(5 + 5) * 2",
        "((5 + 5) * 2)",
    );
    try ToStringTest(
        "2 / (5 + 5)",
        "(2 / (5 + 5))",
    );
    try ToStringTest(
        "-(5 + 5)",
        "(-(5 + 5))",
    );
    try ToStringTest(
        "!(true == true)",
        "(!(true == true))",
    );
    try ToStringTest(
        "x + y)",
        "(x + y)",
    );
}

test "IfExpressions" {
    try IfExpressionTest("if (x < y) { x }");
}

test "FunctionLiterals" {
    try FunctionLiteralTest("fn(x, y) { x + y; }");
}

test "FunctionParameterTest" {
    const expected: []const []const []const u8 = &.{
        &.{},
        &.{"x"},
        &.{ "x", "y", "z" },
    };

    try FunctionParameterTest("fn() {}", expected[0]);
    try FunctionParameterTest("fn(x) {}", expected[1]);
    try FunctionParameterTest("fn(x, y, z) {}", expected[2]);
}

test "CallExpressionsTest" {
    try CallExpressionTest("add(1, 2 * 3, 4 + 5);");
}

////////// Helpers /////////////////////////
pub fn CallExpressionTest(comptime input: [:0]const u8) !void {
    var par = Parser.init(input, alloc);

    const program = try par.parseProgram();
    // checkParserErrors(&par);

    try testing.expectEqual(program.statements.items.len, @as(usize, 1));

    var node = program.statements.items[0];
    const callExprNode = node.cast(.CallExpression).?;

    try testIdentifierLiteral(callExprNode.function, "add");

    try testing.expectEqual(@as(usize, 3), callExprNode.arguments.len);

    const args = callExprNode.arguments;

    try testLiteralExpressions(u32, args[0], 1);
    try testInfixExpression(u32, args[1], 2, .multiply, 3);
    try testInfixExpression(u32, args[2], 4, .plus, 5);
}

pub fn FunctionParameterTest(
    comptime input: [:0]const u8,
    comptime expected: []const []const u8,
) !void {
    var par = Parser.init(input, alloc);

    const program = try par.parseProgram();
    // checkParserErrors(&par);

    var node = program.statements.items[0];
    var functionParams = node.cast(.FunctionLiteral).?;

    try testing.expectEqual(expected.len, functionParams.parameters.len);

    for (expected, 0..) |tc, i| {
        try testIdentifierLiteral(&functionParams.parameters[i].base, tc);
    }
}

pub fn FunctionLiteralTest(comptime input: [:0]const u8) !void {
    var par = Parser.init(input, alloc);

    const program = try par.parseProgram();
    // checkParserErrors(&par);

    //assert
    try testing.expectEqual(@as(usize, 1), program.statements.items.len);

    var node = program.statements.items[0];

    var funcLitNode = node.cast(.FunctionLiteral).?;

    try testing.expectEqual(Node.Id.FunctionLiteral, funcLitNode.base.id);

    try testing.expectEqual(@as(usize, 2), funcLitNode.parameters.len);

    if (funcLitNode.parameters.len != 0) {
        try testLiteralExpressions([]const u8, &funcLitNode.parameters[0].base, "x");
        try testLiteralExpressions([]const u8, &funcLitNode.parameters[1].base, "y");
    }

    try testing.expectEqual(@as(usize, 1), funcLitNode.body.statements.items.len);

    const bodyStmt = funcLitNode.body.statements.items[0];
    try testing.expectEqual(Node.Id.InfixExpression, bodyStmt.id);

    try testInfixExpression([]const u8, bodyStmt, "x", .plus, "y");
}

pub fn IfExpressionTest(comptime input: [:0]const u8) !void {
    var par = Parser.init(input, alloc);

    const program = try par.parseProgram();
    checkParserErrors(&par);

    try testing.expectEqual(@as(usize, 1), program.statements.items.len);

    // assert
    var node = program.statements.items[0];

    try testing.expectEqual(Node.Id.IfExpression, node.id);

    const expr = node.cast(.IfExpression).?;

    try testInfixExpression([]const u8, expr.condition, "x", .less_than, "y");

    try testing.expectEqual(@as(usize, 1), expr.consequence.statements.items.len);

    const conseq = expr.consequence.statements.items[0];

    try testLiteralExpressions([]const u8, conseq, "x");

    try testing.expect(expr.alternative == null);
}

pub fn ToStringTest(comptime input: [:0]const u8, comptime expected: []const u8) !void {
    var buf = std.ArrayList(u8).init(alloc);
    defer buf.deinit();

    var par = Parser.init(input, alloc);

    var program = try par.parseProgram();

    try program.toString(buf.writer());

    try testing.expectEqualStrings(expected, buf.items);

    buf.clearRetainingCapacity();
}

pub fn IdentifierLiteralTest(comptime T: type) type {
    return struct {
        pub fn run(comptime input: [:0]const u8, comptime value: []const u8) !void {
            var par = Parser.init(input, allocator.allocator());

            const program = try par.parseProgram();

            try testing.expect(program.statements.items.len == 1);

            try testLiteralExpressions(T, program.statements.items[0], value);
        }
    };
}

fn IntegerLiteraltest(comptime T: type) type {
    return struct {
        pub fn run(comptime input: [:0]const u8, comptime value: T) !void {
            var par = Parser.init(input, allocator.allocator());

            const program = try par.parseProgram();

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

    const stmt = node.cast(.IntegerLiteral).?;

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

fn testInfixExpression(comptime T: type, node: *Node, comptime left: T, comptime op: ast.Operator, comptime right: T) !void {
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

            const program = try par.parseProgram();

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
        pub fn run(
            comptime input: [:0]const u8,
            comptime left: T,
            comptime op: ast.Operator,
            comptime right: T,
        ) !void {
            var par = Parser.init(input, alloc);

            const program = try par.parseProgram();

            try testing.expectEqual(@as(usize, 1), program.statements.items.len);

            const node = program.statements.items[0];

            // assert
            try testInfixExpression(T, node, left, op, right);
        }
    };
}

pub fn testLetStatements(
    comptime T: type,
    comptime input: [:0]const u8,
    comptime expectedIdentifier: []const u8,
    comptime expectedValue: T,
) !void {
    var p = Parser.init(input, allocator.allocator());

    checkParserErrors(&p);

    const program = try p.parseProgram();
    try testing.expectEqual(program.statements.items.len, @as(usize, 1));

    var node = program.statements.items[0];
    try testing.expectEqual(Node.Id.LetStatement, node.id);
    try testing.expectEqual(TokenType.LET, node.tokenType());

    const letNode = @fieldParentPtr(Node.LetStatement, "base", node);

    try testing.expectEqualStrings(expectedIdentifier, letNode.name.value);

    try testLiteralExpressions(T, letNode.value.?, expectedValue);
}
fn testReturnStatement() !void {
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

        const node = program.statements.items[0];

        try testing.expectEqual(Node.Id.ReturnStatement, node.id);
    }
}

fn checkParserErrors(parser: *Parser) void {
    if (parser.errors.items.len == 0) return;

    for (parser.errors.items, 0..) |err, i| {
        std.log.err("[{d}] {any}: {s}", .{ i, err.err, err.msg });
    }
    std.debug.panic("There are {d} parser errors", .{parser.errors.items.len});
}
