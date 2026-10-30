const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Node = ast.Node;
const TokenType = token.TokenType;
const testing = std.testing;

/// Global Allocator
var gpa: std.heap.SafeAllocator = .init(std.heap.page_allocator, .{});
var allocator = std.heap.ArenaAllocator.init(gpa.allocator());
const alloc = allocator.allocator();

pub fn deinitGlobalAllocator() void {
    allocator.deinit();
    _ = gpa.deinit();
}

test "Parser Error" {
    const input = "let = 5";
    var gpa_test: std.heap.SafeAllocator = .init(std.heap.page_allocator, .{});
    defer _ = gpa_test.deinit();
    var p = Parser.init(input, gpa_test.allocator());
    defer p.deinit();
    checkParserErrors(&p);
}

test "Debug Parse Simple" {
    var gpa_test: std.heap.SafeAllocator = .init(std.heap.page_allocator, .{});
    defer _ = gpa_test.deinit();
    var parser = Parser.init("let x = 5;", gpa_test.allocator());

    // Create a fresh Tree manually to test if it works
    const treePtr = try ast.Node.Tree.init(gpa_test.allocator());
    defer {
        treePtr.deinit(gpa_test.allocator());
        gpa_test.allocator().destroy(treePtr);
    }
    std.debug.print("\nFresh tree statements length: {}\n", .{treePtr.statements.items.len});

    const rootNode = try parser.parseProgram();
    const program = rootNode.cast(.Tree);

    std.debug.print("After parsing statements length: {}\n", .{program.statements.items.len});
    std.debug.print("Program statements ptr: {*}\n", .{program.statements.items.ptr});

    if (program.statements.items.len > 0 and program.statements.items.len < 10) {
        const stmt = program.statements.items[0];
        std.debug.print("First statement id: {}\n", .{stmt.id});
    }

    // Only deinit parser after we're done using the Tree
    defer parser.deinit();
}

test "ParseLetStatements" {
    // Commenting out to focus on debug test
    // try testLetStatements(
    //     u32,
    //     "let x = 5;",
    //     "x",
    //     5,
    // );
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

test "MalformedInputReportsErrors" {
    // Each input used to panic on a null unwrap or was silently dropped.
    // The first reported error is pinned so the messages stay stable.
    const cases = [_]struct { input: [:0]const u8, err: []const u8 }{
        .{ .input = "let x =", .err = "Parser Error: No Prefix function found for .EOF" },
        .{ .input = "let x = ;", .err = "Parser Error: No Prefix function found for .SEMICOLON" },
        .{ .input = "-", .err = "Parser Error: No Prefix function found for .EOF" },
        .{ .input = "!", .err = "Parser Error: No Prefix function found for .EOF" },
        .{ .input = "1 +", .err = "Parser Error: No Prefix function found for .EOF" },
        .{ .input = "()", .err = "Parser Error: No Prefix function found for .RPAREN" },
        .{ .input = "(", .err = "Parser Error: No Prefix function found for .EOF" },
        .{ .input = "(1", .err = "Expected next token to be .RPAREN got '.EOF' instead" },
        .{ .input = "return;", .err = "Parser Error: No Prefix function found for .SEMICOLON" },
        .{ .input = "add(1,", .err = "Parser Error: No Prefix function found for .EOF" },
        .{ .input = "f(1,)", .err = "Parser Error: No Prefix function found for .RPAREN" },
        .{ .input = "fn(x y) { x }", .err = "Expected next token to be .RPAREN got '.IDENT' instead" },
        .{ .input = "fn(1) { 1 }", .err = "Expected next token to be .IDENT got '.INT' instead" },
        .{ .input = "fn(x,) { x }", .err = "Expected next token to be .IDENT got '.RPAREN' instead" },
        .{ .input = "fn(x) + 1", .err = "Expected next token to be .LBRACE got '.PLUS' instead" },
        .{ .input = "fn(x) { x", .err = "Expected next token to be .RBRACE got '.EOF' instead" },
        .{ .input = "if () { 1 }", .err = "Parser Error: No Prefix function found for .RPAREN" },
        .{ .input = "if (true) { 1", .err = "Expected next token to be .RBRACE got '.EOF' instead" },
        .{ .input = "if (true) { 1 } else 2", .err = "Expected next token to be .LBRACE got '.INT' instead" },
        .{ .input = "9223372036854775808", .err = "Integer literal out of range: 9223372036854775808" },
        .{ .input = "\"abc", .err = "Parser Error: No Prefix function found for .ILLEGAL" },
        .{ .input = "@", .err = "Parser Error: No Prefix function found for .ILLEGAL" },
        .{ .input = ";", .err = "Parser Error: No Prefix function found for .SEMICOLON" },
        // Bitwise operators are lexed but the parser has no rules for them yet.
        .{ .input = "1 & 2", .err = "Parser Error: No Prefix function found for .AND" },
    };
    for (cases) |tc| {
        try expectFirstParseError(tc.input, tc.err);
    }
}

test "IntegerLiteralRange" {
    var p = Parser.init("9223372036854775807; 5000000000", testing.allocator);
    defer p.deinit();
    const program = (try p.parseProgram()).cast(.Tree);

    checkParserErrors(&p);
    try testIntegerLiteral(program.statements.items[0], std.math.maxInt(i64));
    try testIntegerLiteral(program.statements.items[1], 5000000000);
}

test "SemicolonEndsStatement" {
    // A `;` after a nested operand used to be consumed too early, so the outer
    // expression carried on: `let a = 1 + 2; -3` parsed as `let a = (1 + 2) - 3`.
    try ToStringTest("let a = 1 + 2; -3", "let a = (1 + 2);(-3)");
    try ToStringTest("x * 2; -1", "(x * 2)(-1)");
    try ToStringTest("1 + 2; (3)", "(1 + 2)3");
    try ToStringTest("return 1 + 2; -3", "return (1 + 2);(-3)");
}

test "FunctionLiteralToString" {
    try ToStringTest("let f = fn(x, y) { x + y; }", "let f = fn(x, y) (x + y);");
    try ToStringTest("fn() { 1 }", "fn() 1");
}

////////// Helpers /////////////////////////
pub fn CallExpressionTest(comptime input: [:0]const u8) !void {
    var gpa_test: std.heap.SafeAllocator = .init(std.heap.page_allocator, .{});
    defer _ = gpa_test.deinit();
    var par = Parser.init(input, gpa_test.allocator());
    defer par.deinit();

    const rootNode = try par.parseProgram();
    const program = rootNode.cast(.Tree);
    // checkParserErrors(&par);

    try testing.expectEqual(program.statements.items.len, @as(usize, 1));

    var node = program.statements.items[0];
    const callExprNode = node.cast(.CallExpression);

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
    var gpa_test: std.heap.SafeAllocator = .init(std.heap.page_allocator, .{});
    defer _ = gpa_test.deinit();
    var par = Parser.init(input, gpa_test.allocator());
    defer par.deinit();

    const rootNode = try par.parseProgram();
    const program = rootNode.cast(.Tree);
    // checkParserErrors(&par);

    var node = program.statements.items[0];
    var functionParams = node.cast(.FunctionLiteral);

    try testing.expectEqual(expected.len, functionParams.parameters.len);

    for (expected, 0..) |tc, i| {
        try testIdentifierLiteral(functionParams.parameters[i].toNode(), tc);
    }
}

pub fn FunctionLiteralTest(comptime input: [:0]const u8) !void {
    var gpa_test: std.heap.SafeAllocator = .init(std.heap.page_allocator, .{});
    defer _ = gpa_test.deinit();
    var par = Parser.init(input, gpa_test.allocator());
    defer par.deinit();

    const rootNode = try par.parseProgram();
    const program = rootNode.cast(.Tree);
    // checkParserErrors(&par);

    //assert
    try testing.expectEqual(@as(usize, 1), program.statements.items.len);

    var node = program.statements.items[0];

    var funcLitNode = node.cast(.FunctionLiteral);

    try testing.expectEqual(Node.Id.FunctionLiteral, funcLitNode.base.id);

    try testing.expectEqual(@as(usize, 2), funcLitNode.parameters.len);

    if (funcLitNode.parameters.len != 0) {
        try testLiteralExpressions([]const u8, funcLitNode.parameters[0].toNode(), "x");
        try testLiteralExpressions([]const u8, funcLitNode.parameters[1].toNode(), "y");
    }

    try testing.expectEqual(@as(usize, 1), funcLitNode.body.statements.items.len);

    const bodyStmt = funcLitNode.body.statements.items[0];
    try testing.expectEqual(Node.Id.InfixExpression, bodyStmt.id);

    try testInfixExpression([]const u8, bodyStmt, "x", .plus, "y");
}

pub fn IfExpressionTest(comptime input: [:0]const u8) !void {
    var gpa_test: std.heap.SafeAllocator = .init(std.heap.page_allocator, .{});
    defer _ = gpa_test.deinit();
    var par = Parser.init(input, gpa_test.allocator());
    defer par.deinit();

    const rootNode = try par.parseProgram();
    const program = rootNode.cast(.Tree);
    checkParserErrors(&par);

    try testing.expectEqual(@as(usize, 1), program.statements.items.len);

    // assert
    var node = program.statements.items[0];

    try testing.expectEqual(Node.Id.IfExpression, node.id);

    const expr = node.cast(.IfExpression);

    try testInfixExpression([]const u8, expr.condition, "x", .less_than, "y");

    try testing.expectEqual(@as(usize, 1), expr.consequence.statements.items.len);

    const conseq = expr.consequence.statements.items[0];

    try testLiteralExpressions([]const u8, conseq, "x");

    try testing.expect(expr.alternative == null);
}

pub fn ToStringTest(comptime input: [:0]const u8, comptime expected: []const u8) !void {
    var gpa_test: std.heap.SafeAllocator = .init(std.heap.page_allocator, .{});
    defer _ = gpa_test.deinit();
    var buf: std.Io.Writer.Allocating = .init(gpa_test.allocator());
    defer buf.deinit();

    var par = Parser.init(input, gpa_test.allocator());
    defer par.deinit();

    const program = try par.parseProgram();

    try program.toString(&buf.writer);

    try testing.expectEqualStrings(expected, buf.written());

    buf.clearRetainingCapacity();
}

pub fn IdentifierLiteralTest(comptime T: type) type {
    return struct {
        pub fn run(comptime input: [:0]const u8, comptime value: []const u8) !void {
            var gpa_test: std.heap.SafeAllocator = .init(std.heap.page_allocator, .{});
            defer _ = gpa_test.deinit();
            var par = Parser.init(input, gpa_test.allocator());
            defer par.deinit();

            const rootNode = try par.parseProgram();
            const program = rootNode.cast(.Tree);

            try testing.expectEqual(@as(usize, 1), program.statements.items.len);
            try testLiteralExpressions(T, program.statements.items[0], value);
        }
    };
}

fn IntegerLiteraltest(comptime T: type) type {
    return struct {
        pub fn run(comptime input: [:0]const u8, comptime value: T) !void {
            var gpa_test: std.heap.SafeAllocator = .init(std.heap.page_allocator, .{});
            defer _ = gpa_test.deinit();
            var par = Parser.init(input, gpa_test.allocator());
            defer par.deinit();

            const rootNode = try par.parseProgram();
            const program = rootNode.cast(.Tree);

            try testing.expectEqual(@as(usize, 1), program.statements.items.len);
            try testLiteralExpressions(T, program.statements.items[0], value);
        }
    };
}

fn testIdentifierLiteral(node: *Node, val: []const u8) !void {
    try testing.expectEqual(Node.Id.Identifier, node.id);

    const identifierNode = node.cast(Node.Id.Identifier);

    try testing.expectEqualStrings(val, identifierNode.token.Literal);
}

fn testIntegerLiteral(node: *Node, value: i64) !void {
    var buf: [1024]u8 = undefined;

    const stmt = node.cast(.IntegerLiteral);

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

    const expr = node.cast(Node.Id.InfixExpression);

    try testLiteralExpressions(T, expr.leftExprPtr.?, left);

    try testing.expectEqual(op, expr.operator);

    try testLiteralExpressions(T, expr.rightExprPtr.?, right);
}

fn testBooleanLiteral(node: *Node, value: bool) !void {
    try testing.expectEqual(Node.Id.Boolean, node.id);

    const expr = node.cast(.Boolean);

    try testing.expectEqual(value, expr.value);
}

pub fn PrefixTest(comptime T: type) type {
    return struct {
        pub fn run(comptime input: [:0]const u8, comptime op: ast.Operator, comptime value: T) !void {
            var gpa_test: std.heap.SafeAllocator = .init(std.heap.page_allocator, .{});
            defer _ = gpa_test.deinit();
            var par = Parser.init(input, gpa_test.allocator());
            defer par.deinit();

            const rootNode = try par.parseProgram();
            const program = rootNode.cast(.Tree);

            //assert
            try testing.expectEqual(@as(usize, 1), program.statements.items.len);

            const node = program.statements.items[0];

            try testing.expectEqual(Node.Id.PrefixExpression, node.id);

            const prefixExpressionNode = Node.cast(node, Node.Id.PrefixExpression);

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
            var gpa_test: std.heap.SafeAllocator = .init(std.heap.page_allocator, .{});
            defer _ = gpa_test.deinit();
            var par = Parser.init(input, gpa_test.allocator());
            defer par.deinit();

            const rootNode = try par.parseProgram();
            const program = rootNode.cast(.Tree);

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
    var gpa_test: std.heap.SafeAllocator = .init(std.heap.page_allocator, .{});
    defer _ = gpa_test.deinit();
    var parser = Parser.init(input, gpa_test.allocator());
    defer parser.deinit();

    checkParserErrors(&parser);

    const rootNode = try parser.parseProgram();

    const program = rootNode.cast(.Tree);
    // Ensure program has statements

    // Validate statement count
    if (program.statements.items.len == 0) return error.EmptyStatements;
    try testing.expectEqual(@as(usize, 1), program.statements.items.len);

    const node = program.statements.items[0];
    try testing.expectEqual(Node.Id.LetStatement, node.id);
    // try testing.expectEqual(TokenType.LET, node.tokenType());

    const letNode = node.cast(.LetStatement);

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
        var gpa_test: std.heap.SafeAllocator = .init(std.heap.page_allocator, .{});
        defer _ = gpa_test.deinit();
        var p = Parser.init(tc.input, gpa_test.allocator());
        defer p.deinit();

        const rootNode = try p.parseProgram();

        const program = rootNode.cast(.Tree);
        // Ensure program has statements

        // Validate statement count
        if (program.statements.items.len == 0) return error.EmptyStatements;
        try testing.expectEqual(@as(usize, 1), program.statements.items.len);

        const node = program.statements.items[0];

        try testing.expectEqual(Node.Id.ReturnStatement, node.id);
    }
}

fn expectFirstParseError(input: [:0]const u8, expected: []const u8) !void {
    var p = Parser.init(input, testing.allocator);
    defer p.deinit();
    _ = try p.parseProgram();

    if (p.errors.items.len == 0) {
        std.debug.print("input `{s}` parsed without errors\n", .{input});
        return error.TestExpectedParseError;
    }
    testing.expectEqualStrings(expected, p.errors.items[0].msg) catch |err| {
        std.debug.print("input: `{s}`\n", .{input});
        return err;
    };
}

fn checkParserErrors(parser: *Parser) void {
    if (parser.errors.items.len == 0) return;

    for (parser.errors.items, 0..) |err, i| {
        std.log.err("[{d}] {any}: {s}", .{ i, err.err, err.msg });
    }
    std.debug.panic("There are {d} parser errors", .{parser.errors.items.len});
}
