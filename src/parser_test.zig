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

    const stmt = program.statements.items[0];

    try testing.expectEqual(Node.Id.Identifier, stmt.id);
    const identifierNode = @fieldParentPtr(Node.Identifier, "base", stmt);

    try testing.expectEqualStrings("foobar", identifierNode.token.Literal);
}

test "IntegerLiteral" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = std.heap.ArenaAllocator.init(gpa.allocator());
    var input = "5;";
    var lex = Lexer.init(input);
    var par = Parser.init(lex, allocator.allocator());
    defer allocator.deinit();

    var program = try par.parseProgram();

    try testing.expect(program.statements.items.len == 1);

    const stmt = program.statements.items[0];

    try testing.expectEqual(Node.Id.IntegerLiteral, stmt.id);
    const integerLiteralNode = @fieldParentPtr(Node.IntegerLiteral, "base", stmt);
    try testing.expectEqual(@as(u32, 5), integerLiteralNode.value);
    try testing.expectEqualStrings("5", integerLiteralNode.token.Literal);
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

        var program = try par.parseProgram();

        //assert
        try testing.expect(1 == program.statements.items.len);

        const node = program.statements.items[0];

        try testing.expectEqual(Node.Id.PrefixExpression, node.id);

        const prefixExpressionNode = Node.cast(node, Node.Id.PrefixExpression).?;

        try testing.expectEqual(tc.operator, prefixExpressionNode.operator);

        const rightExprNode = Node.cast(prefixExpressionNode.rightExprPtr.?, Node.Id.IntegerLiteral).?;

        try testIntegerLiteral(alloc, rightExprNode, tc.integerValue);
    }
}

fn testIntegerLiteral(allocator: std.mem.Allocator, stmt: *Node.IntegerLiteral, value: u32) !void {
    try testing.expectEqual(Node.IntegerLiteral, @TypeOf(stmt.*));

    const int = stmt.value;

    try testing.expectEqual(int, value);

    try testing.expectEqualStrings(stmt.token.Literal, try std.fmt.allocPrint(allocator, "{d}", .{value}));
}

// test "InfixTests" {
//     const InfixStmt = struct { input: [:0]const u8, left: u32, op: []const u8, right: u32 };
//     const tests = [_]InfixStmt{
//         .{ .input = "5 + 5;", .left = 5, .op = "+", .right = 5 },
//         .{ .input = "5 - 5;", .left = 5, .op = "-", .right = 5 },
//         .{ .input = "5 * 5;", .left = 5, .op = "*", .right = 5 },
//         .{ .input = "5 / 5;", .left = 5, .op = "/", .right = 5 },
//         .{ .input = "5 > 5;", .left = 5, .op = ">", .right = 5 },
//         .{ .input = "5 < 5;", .left = 5, .op = "<", .right = 5 },
//         .{ .input = "5 == 5;", .left = 5, .op = "==", .right = 5 },
//         .{ .input = "5 != 5;", .left = 5, .op = "!=", .right = 5 },
//     };

//     var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//     var allocator = std.heap.ArenaAllocator.init(gpa.allocator());
//     defer allocator.deinit();

//     var alloc = allocator.allocator();

//     for (tests) |tc| {
//         var lex = Lexer.init(tc.input);
//         var par = Parser.init(lex, alloc);
//         defer par.deinit();

//         var program = par.parseProgram();

//         // try testing.expectEqual(1, program.statements.items.len);

//         {
//             const node = program.statements.items[0];
//             try testing.expectEqual(Node.ExpressionStatement, @TypeOf(node.statement.expressionStatement.*));
//         }
//         {
//             const node = program.statements.items[0];

//             try testing.expectEqual(Node.ExpressionStatement, @TypeOf(node.statement.expressionStatement.*));
//         }

//         {
//             const node = program.statements.items[0];

//             const expr = node.statement.expressionStatement.*.expression.?.infixExpression.*;

//             try testing.expectEqual(Node.InfixExpression, @TypeOf(expr));
//         }

//         {
//             const node = program.statements.items[0];

//             const expr = node.statement.expressionStatement.*.expression.?.infixExpression.*;

//             try testing.expectEqualStrings(tc.op, expr.operator);
//         }
//         {
//             const node = program.statements.items[0];

//             const expr = node.statement.expressionStatement.*.expression.?.infixExpression.*;

//             try testIntegerLiteral(alloc, expr.leftExprPtr.*.?, tc.left);
//         }

//         {
//             const node = program.statements.items[0];

//             const expr = node.statement.expressionStatement.*.expression.?.infixExpression.*;
//             try testIntegerLiteral(alloc, expr.rightExprPtr.*.?, tc.right);
//         }
//     }
// }
