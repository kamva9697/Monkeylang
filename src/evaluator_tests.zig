const std = @import("std");
const Lexer = @import("lexer.zig");
const _Parser = @import("parser.zig");
const Parser = _Parser.Parser;
const _Object = @import("object.zig");
const Ast = @import("ast.zig");
const Object = _Object.Object;
const ObjectType = Object.ObjectType;
const evaluator = @import("evaluator.zig");
const testing = std.testing;

/// Global Allocator
var gpa = std.heap.GeneralPurposeAllocator(.{}){};

test "testEvalIntegerExpressions" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator(); // the ast Test Deallocates
    defer arena.deinit();
    try testIntegerExpressions(allocator, "5", 5);
    try testIntegerExpressions(allocator, "10", 10);
    try testIntegerExpressions(allocator, "-5", -5);
    try testIntegerExpressions(allocator, "-10", -10);
    try testIntegerExpressions(allocator, "5 + 5", 10);
    try testIntegerExpressions(allocator, "5 + 5 + 5", 15);
    try testIntegerExpressions(allocator, "5 + 5 + 5 + 5 - 10", 10);
    try testIntegerExpressions(allocator, "2 * 2 * 2 * 2 * 2", 32);
    try testIntegerExpressions(allocator, "-50 + 100 + -50", 0);
    try testIntegerExpressions(allocator, "5 * 2 + 10", 20);
    try testIntegerExpressions(allocator, "5 + 2 * 10", 25);
    try testIntegerExpressions(allocator, "20 + 2 * -10", 0);
    try testIntegerExpressions(allocator, "50 / 2 * 2 + 10", 60);
    try testIntegerExpressions(allocator, "2 * (5 + 10)", 30);
    try testIntegerExpressions(allocator, "3 * 3 * 3 + 10", 37);
    try testIntegerExpressions(allocator, "3 * (3 * 3) + 10", 37);
    try testIntegerExpressions(allocator, "(5 + 10 * 2 + 15 / 3) * 2 + -10", 50);
}

test "testEvalBooleanExpressions" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator(); // the ast Test Deallocates
    defer arena.deinit();
    try testBooleanExpressions(allocator, "true", true);
    try testBooleanExpressions(allocator, "false", false);
    try testBooleanExpressions(allocator, "!true", false);
    try testBooleanExpressions(allocator, "!false", true);
    try testBooleanExpressions(allocator, "!5", false);
    try testBooleanExpressions(allocator, "!!true", true);
    try testBooleanExpressions(allocator, "!!false", false);
    try testBooleanExpressions(allocator, "!!5", true);
    try testBooleanExpressions(allocator, "1 < 2", true);
    try testBooleanExpressions(allocator, "1 > 2", false);
    try testBooleanExpressions(allocator, "1 < 1", false);
    try testBooleanExpressions(allocator, "1 > 1", false);
    try testBooleanExpressions(allocator, "1 == 1", true);
    try testBooleanExpressions(allocator, "1 != 1", false);
    try testBooleanExpressions(allocator, "1 == 2", false);
    try testBooleanExpressions(allocator, "1 != 2", true);
    try testBooleanExpressions(allocator, "true == true", true);
    try testBooleanExpressions(allocator, "false == false", true);
    try testBooleanExpressions(allocator, "true == false", false);
    try testBooleanExpressions(allocator, "true != false", true);
    try testBooleanExpressions(allocator, "false != true", true);
    try testBooleanExpressions(allocator, "(1 < 2) == true", true);
    try testBooleanExpressions(allocator, "(1 < 2) == false", false);
    try testBooleanExpressions(allocator, "(1 > 2) == true", false);
    try testBooleanExpressions(allocator, "(1 > 2) == false", true);
}

test "TestEvalIfExpressions" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator(); // the ast Test Deallocates
    defer arena.deinit();
    try testIfExpressions(allocator, "if (true) { 10 }", 10);
    try testIfExpressions(allocator, "if (false) { 10 }", null);
    try testIfExpressions(allocator, "if (1) { 10 }", 10);
    try testIfExpressions(allocator, "if (1 < 2) { 10 }", 10);
    try testIfExpressions(allocator, "if (1 > 2) { 10 }", null);
    try testIfExpressions(allocator, "if (1 > 2) { 10 } else { 20 }", 20);
    try testIfExpressions(allocator, "if (1 < 2) { 10 } else { 20 }", 10);
}

fn testIfExpressions(alloc: std.mem.Allocator, input: [:0]const u8, expected: ?i64) !void {
    const evaluated = (try testEval(alloc, input)).?;

    if (expected) |is_expected| {
        try testIntegerObject(evaluated, is_expected);
    } else {
        try testNullObject(evaluated);
    }
}

fn testNullObject(obj: *Object) !void {
    try testing.expectEqual(ObjectType.Null, obj.ty);
}

fn testBangOperator(alloc: std.mem.Allocator, input: [:0]const u8, expected: bool) !void {
    const evaluated = (try testEval(alloc, input)).?;
    try testBooleanObject(evaluated, expected);
}

fn testBooleanExpressions(alloc: std.mem.Allocator, input: [:0]const u8, expected: bool) !void {
    const evaluated = (try testEval(alloc, input)).?;
    try testBooleanObject(evaluated, expected);
}

fn testIntegerExpressions(alloc: std.mem.Allocator, input: [:0]const u8, expected: i64) !void {
    const evaluated = (try testEval(alloc, input)).?;
    try testIntegerObject(evaluated, expected);
}

fn testEval(alloc: std.mem.Allocator, input: [:0]const u8) !?*Object {
    var parser = Parser.init(input, alloc);
    defer parser.deinit();
    const astTree = (try parser.parseProgram());

    return (try evaluator.evalStatements(alloc, astTree.statements.items)).?;
}

fn testIntegerObject(obj: *Object, expected: i64) !void {
    try testing.expectEqual(ObjectType.Integer, obj.ty);
    const integerValue = obj.cast(.Integer).?;
    try testing.expectEqual(expected, integerValue.value);
}

fn testBooleanObject(obj: *Object, expected: bool) !void {
    try testing.expectEqual(ObjectType.Boolean, obj.ty);
    const boolValue = obj.cast(.Boolean).?;
    try testing.expectEqual(expected, boolValue.value);
}
