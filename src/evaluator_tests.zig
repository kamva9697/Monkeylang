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
var arena = std.heap.ArenaAllocator.init(gpa.allocator());
var alloc = arena.allocator(); // the ast Test Deallocates

test "testEvalIntegerExpressions" {
    try testIntegerExpressions("5", 5);
    try testIntegerExpressions("10", 10);
    try testIntegerExpressions("-5", -5);
    try testIntegerExpressions("-10", -10);
    try testIntegerExpressions("5 + 5", 10);
    try testIntegerExpressions("5 + 5 + 5", 15);
    try testIntegerExpressions("5 + 5 + 5 + 5 - 10", 10);
    try testIntegerExpressions("2 * 2 * 2 * 2 * 2", 32);
    try testIntegerExpressions("-50 + 100 + -50", 0);
    try testIntegerExpressions("5 * 2 + 10", 20);
    try testIntegerExpressions("5 + 2 * 10", 25);
    try testIntegerExpressions("20 + 2 * -10", 0);
    try testIntegerExpressions("50 / 2 * 2 + 10", 60);
    try testIntegerExpressions("2 * (5 + 10)", 30);
    try testIntegerExpressions("3 * 3 * 3 + 10", 37);
    try testIntegerExpressions("3 * (3 * 3) + 10", 37);
    try testIntegerExpressions("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50);
}

test "testEvalBooleanExpressions" {
    try testBooleanExpressions("true", true);
    try testBooleanExpressions("false", false);
    try testBooleanExpressions("!true", false);
    try testBooleanExpressions("!false", true);
    try testBooleanExpressions("!5", false);
    try testBooleanExpressions("!!true", true);
    try testBooleanExpressions("!!false", false);
    try testBooleanExpressions("!!5", true);

    arena.deinit();
}

fn testBangOperator(input: [:0]const u8, expected: bool) !void {
    const evaluated = (try testEval(input)).?;
    try testBooleanObject(evaluated, expected);
}

fn testBooleanExpressions(input: [:0]const u8, expected: bool) !void {
    const evaluated = (try testEval(input)).?;
    try testBooleanObject(evaluated, expected);
}

fn testIntegerExpressions(input: [:0]const u8, expected: i64) !void {
    const evaluated = (try testEval(input)).?;
    try testIntegerObject(evaluated, expected);
}

fn testEval(input: [:0]const u8) !?*Object {
    var parser = Parser.init(input, alloc);
    const astTree = (try parser.parseProgram());

    return evaluator.evalStatements(alloc, astTree);
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
