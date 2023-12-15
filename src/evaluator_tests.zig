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
const Environment = @import("environment.zig").Environment;

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

test "TestReturnStatements" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator(); // the ast Test Deallocates
    defer arena.deinit();
    try testReturnStatements(allocator, "return 10;", 10);
    try testReturnStatements(allocator, "return 10; 9;", 10);
    try testReturnStatements(allocator, "return 2 * 5; 9;", 10);
    try testReturnStatements(allocator, "7; return 2 * 5; 9;", 10);
}

test "TestErrorHanlding" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator(); // the ast Test Deallocates
    defer arena.deinit();
    try testErrorHandling(
        allocator,
        "5 + true;",
        "Type Mismatch: Integer + Boolean",
    );
    try testErrorHandling(
        allocator,
        "5 + true; 5;",
        "Type Mismatch: Integer + Boolean",
    );
    try testErrorHandling(
        allocator,
        "-true",
        "Unknown Operator: -Boolean",
    );
    try testErrorHandling(
        allocator,
        "true + false;",
        "Unknown Operator: Boolean + Boolean",
    );
    try testErrorHandling(
        allocator,
        "5; true + false; 5",
        "Unknown Operator: Boolean + Boolean",
    );
    try testErrorHandling(
        allocator,
        "if (10 > 1) { true + false; }",
        "Unknown Operator: Boolean + Boolean",
    );
    try testErrorHandling(
        allocator,
        "foobar",
        "Identifier not found: foobar",
    );
    try testErrorHandling(
        allocator,
        \\if (10 > 1) {
        \\    if (10 > 1) {
        \\      return true + false;
        \\      }
        \\  return 1;
        \\  }
    ,
        "Unknown Operator: Boolean + Boolean",
    );
}

test "TestLetStatement" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator(); // the ast Test Deallocates
    defer arena.deinit();
    try testLetStatement(allocator, "let a = 5; a;", 5);
    try testLetStatement(allocator, "let a = 5 * 5; a;", 25);
    try testLetStatement(allocator, "let a = 5; let b = a; b;", 5);
    try testLetStatement(allocator, "let a = 5; let b = a; let c = a + b + 5; c;", 15);
}

test "TestFunction" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator(); // the ast Test Deallocates
    defer arena.deinit();
    try testFunction(allocator, "fn(x) { x + 2; };");
}

fn testFunction(alloc: std.mem.Allocator, input: [:0]const u8) !void {
    var buf = std.ArrayList(u8).init(alloc);
    const evaluated = (try testEval(alloc, input)).?;

    try testing.expectEqual(ObjectType.Function, evaluated.ty);
    const fnObj = evaluated.cast(.Function).?;

    try testing.expectEqual(fnObj.parameters.len, 1);
    try (&fnObj.parameters[0].base).toString(buf.writer()); // upcast
    try testing.expectEqualStrings("x", buf.items);

    const expectedBody = "(x + 2)";

    buf.clearRetainingCapacity(); // clear previous use
    try (&fnObj.body.base).toString(buf.writer()); // upcast
    try testing.expectEqualStrings(expectedBody, buf.items);
}

fn testLetStatement(alloc: std.mem.Allocator, input: [:0]const u8, expected: i64) !void {
    const evaluated = (try testEval(alloc, input)).?;
    try testIntegerObject(evaluated, expected);
}

fn testErrorHandling(alloc: std.mem.Allocator, input: [:0]const u8, expected: []const u8) !void {
    const evaluated = (try testEval(alloc, input)).?;

    try testing.expectEqual(ObjectType.Error, evaluated.ty);
    const errorObj = evaluated.cast(.Error).?;
    try testing.expectEqualStrings(expected, errorObj.message);
}

fn testReturnStatements(alloc: std.mem.Allocator, input: [:0]const u8, expected: i64) !void {
    const evaluated = (try testEval(alloc, input)).?;
    try testIntegerObject(evaluated, expected);
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
    var envPtr = try alloc.create(Environment);
    envPtr.store = std.StringHashMap(*Object).init(alloc);
    envPtr.outer = null;
    defer parser.deinit();
    const rootNode = (try parser.parseProgram());

    return (try evaluator.eval(alloc, rootNode, envPtr)).?;
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
