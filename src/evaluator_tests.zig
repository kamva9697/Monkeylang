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
var gpa: std.heap.SafeAllocator = .init(std.heap.page_allocator, .{});

test "testEvalIntegerExpressions" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator();
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
    const allocator = arena.allocator();
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
    const allocator = arena.allocator();
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
    const allocator = arena.allocator();
    defer arena.deinit();
    try testReturnStatements(allocator, "return 10;", 10);
    try testReturnStatements(allocator, "return 10; 9;", 10);
    try testReturnStatements(allocator, "return 2 * 5; 9;", 10);
    try testReturnStatements(allocator, "7; return 2 * 5; 9;", 10);
}

test "TestErrorHandling" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator();
    defer arena.deinit();
    try testErrorHandling(
        allocator,
        "5 + true;",
        "Type Mismatch: Integer + Boolean",
    );
    try testErrorHandling(
        allocator,
        \\"Hello" - "World"
    ,
        "Unknown Operator: String - String",
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
    const allocator = arena.allocator();
    defer arena.deinit();
    try testLetStatement(allocator, "let a = 5; a;", 5);
    try testLetStatement(allocator, "let a = 5 * 5; a;", 25);
    try testLetStatement(allocator, "let a = 5; let b = a; b;", 5);
    try testLetStatement(allocator, "let a = 5; let b = a; let c = a + b + 5; c;", 15);
}

test "TestFunction" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator();
    defer arena.deinit();
    try testFunction(allocator, "fn(x) { x + 2; };");
}

test "TestStringConcat" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator();
    defer arena.deinit();

    try testStringConcatenation(
        allocator,
        \\"Hello" + " " + "World"
    ,
        "Hello World",
    );
}

test "TestBuiltinFunctions" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator();
    defer arena.deinit();

    try testIntegerExpressions(allocator, "len(\"\")", 0);
    try testIntegerExpressions(allocator, "len(\"four\")", 4);
    try testIntegerExpressions(allocator, "len(\"hello world\")", 11);
    try testIntegerExpressions(allocator, "let len = fn(x) { 42 }; len(\"four\")", 42);
    try testErrorHandling(allocator, "len(1)", "Argument to 'len' not supported, got Integer");
    try testErrorHandling(allocator, "len(\"one\", \"two\")", "Wrong number of arguments: got 2, want 1");
    try testErrorHandling(allocator, "5(1)", "Not a function: Integer");
    try testNullObject((try testEval(allocator, "puts(\"hello\", 1)")).?);
}

test "TestFunctionApplication" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator();
    defer arena.deinit();

    try testIntegerExpressions(allocator, "let identity = fn(x) { x; }; identity(5);", 5);
    try testIntegerExpressions(allocator, "let identity = fn(x) { return x; }; identity(5);", 5);
    try testIntegerExpressions(allocator, "let double = fn(x) { x * 2; }; double(5);", 10);
    try testIntegerExpressions(allocator, "let add = fn(x, y) { x + y; }; add(5, 5);", 10);
    try testIntegerExpressions(allocator, "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20);
    try testIntegerExpressions(allocator, "fn(x) { x; }(5)", 5);
    try testIntegerExpressions(allocator, "fn() { 1 }()", 1);
    try testIntegerExpressions(allocator, "let apply = fn(f, x) { f(x) }; apply(fn(n) { n * 3 }, 4)", 12);
}

test "TestClosuresAndScope" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator();
    defer arena.deinit();

    try testIntegerExpressions(allocator, "let newAdder = fn(x) { fn(y) { x + y } }; let addTwo = newAdder(2); addTwo(2);", 4);
    // Lookups walk every enclosing scope, so a closure two levels deep still sees globals.
    try testIntegerExpressions(allocator, "let z = 1; let f = fn(x) { fn(y) { x + y + z } }; f(1)(2)", 4);
    // Closures share their defining environment, so they see later rebinding.
    try testIntegerExpressions(allocator, "let x = 1; let f = fn() { x }; let x = 2; f()", 2);
    // Parameters shadow outer names without changing them.
    try testIntegerExpressions(allocator, "let x = 1; let f = fn(x) { x }; f(5) + x", 6);
}

test "TestRecursionAndNestedReturns" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator();
    defer arena.deinit();

    try testIntegerExpressions(allocator, "let fact = fn(n) { if (n < 2) { 1 } else { n * fact(n - 1) } }; fact(10)", 3628800);
    try testIntegerExpressions(allocator, "let fib = fn(n) { if (n < 2) { n } else { fib(n - 1) + fib(n - 2) } }; fib(15)", 610);
    try testIntegerExpressions(allocator, "if (10 > 1) { if (10 > 1) { return 10; } return 1; }", 10);
    try testIntegerExpressions(allocator, "let f = fn(x) { if (x > 0) { return 1; } return 0; }; f(5)", 1);
    try testIntegerExpressions(allocator, "let f = fn(x) { if (x > 0) { return 1; } return 0; }; f(-5)", 0);
}

test "TestErrorPropagation" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator();
    defer arena.deinit();

    // An error in any operand, condition or argument becomes the result of the whole expression.
    const msg = "Identifier not found: foo";
    try testErrorHandling(allocator, "foo + 1", msg);
    try testErrorHandling(allocator, "1 + foo", msg);
    try testErrorHandling(allocator, "-foo", msg);
    try testErrorHandling(allocator, "!foo", msg);
    try testErrorHandling(allocator, "if (foo) { 1 } else { 2 }", msg);
    try testErrorHandling(allocator, "let add = fn(a, b) { a + b }; add(1, foo)", msg);
    try testErrorHandling(allocator, "puts(1, foo)", msg);
    try testErrorHandling(allocator, "foo(1)", msg);
    try testErrorHandling(allocator, "let x = foo; 5", msg);
    try testErrorHandling(allocator, "fn() { return foo; }()", msg);
}

test "TestArity" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator();
    defer arena.deinit();

    try testErrorHandling(allocator, "let f = fn(a, b) { a }; f(1)", "Wrong number of arguments: got 1, want 2");
    try testErrorHandling(allocator, "let f = fn(a) { a }; f(1, 2)", "Wrong number of arguments: got 2, want 1");
    try testErrorHandling(allocator, "fn() { 1 }(1)", "Wrong number of arguments: got 1, want 0");
}

test "TestNullValues" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator();
    defer arena.deinit();

    try testNullObject((try testEval(allocator, "if (true) {}")).?);
    try testNullObject((try testEval(allocator, "if (false) { 1 } else {}")).?);
    try testNullObject((try testEval(allocator, "fn() {}()")).?);
    try testBooleanExpressions(allocator, "!if (false) { 1 }", true);
    try testBooleanExpressions(allocator, "!!if (false) { 1 }", false);
    // An empty program has no value at all; the REPL prints nothing for it.
    try testing.expect((try testEval(allocator, "")) == null);
}

test "TestIntegerEdgeCases" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator();
    defer arena.deinit();

    try testIntegerExpressions(allocator, "9223372036854775807", std.math.maxInt(i64));
    try testIntegerExpressions(allocator, "-9223372036854775807 - 1", std.math.minInt(i64));
    try testIntegerExpressions(allocator, "5000000000 * 2", 10000000000);
    // Division truncates toward zero.
    try testIntegerExpressions(allocator, "-7 / 2", -3);
    try testIntegerExpressions(allocator, "7 / -2", -3);
    try testErrorHandling(allocator, "1 / 0", "Division by zero: 1 / 0");
    try testErrorHandling(allocator, "9223372036854775807 + 1", "Integer overflow: 9223372036854775807 + 1");
    try testErrorHandling(allocator, "-9223372036854775807 - 2", "Integer overflow: -9223372036854775807 - 2");
    try testErrorHandling(allocator, "4294967296 * 4294967296", "Integer overflow: 4294967296 * 4294967296");
    try testErrorHandling(allocator, "let m = -9223372036854775807 - 1; m / -1", "Integer overflow: -9223372036854775808 / -1");
    try testErrorHandling(allocator, "let m = -9223372036854775807 - 1; -m", "Integer overflow: -(-9223372036854775808)");
}

test "TestPinnedSemantics" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator();
    defer arena.deinit();

    // These are current language rules, pinned so any change to them is deliberate.
    // `let` evaluates to the bound value, which is why the REPL echoes it.
    try testIntegerExpressions(allocator, "let a = 5", 5);
    try testIntegerExpressions(allocator, "let a = 1; let a = 2; a", 2);
    // A program evaluates to its last statement.
    try testIntegerExpressions(allocator, "5 6", 6);
    // Everything except false and null is truthy, including 0 and "".
    try testIfExpressions(allocator, "if (0) { 1 } else { 2 }", 1);
    try testIfExpressions(allocator, "if (\"\") { 1 } else { 2 }", 1);
    // Comparisons need operands of the same type, and strings and null have no ==.
    try testErrorHandling(allocator, "1 == true", "Type Mismatch: Integer == Boolean");
    try testErrorHandling(allocator, "5 < true", "Type Mismatch: Integer < Boolean");
    try testErrorHandling(allocator, "\"a\" == \"a\"", "Unknown Operator: String == String");
    try testErrorHandling(allocator, "if (false) { 1 } == if (false) { 1 }", "Unknown Operator: Null == Null");
}

test "TestCallDepthLimit" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator();
    defer arena.deinit();

    const countdown = "let f = fn(n) { if (n == 0) { 0 } else { 1 + f(n - 1) } }; ";
    // f(999) nests exactly 1000 calls, the limit; f(1000) needs one more.
    try testIntegerObject(try testEvalOnEvaluatorStack(allocator, countdown ++ "f(999)"), 999);
    const beyond = try testEvalOnEvaluatorStack(allocator, countdown ++ "f(1000)");
    try testing.expectEqual(ObjectType.Error, beyond.ty);
    try testing.expectEqualStrings("Maximum call depth of 1000 exceeded", beyond.cast(.Error).message);
    // The depth resets after an error, so later calls work normally.
    try testIntegerObject(try testEvalOnEvaluatorStack(allocator, countdown ++ "f(10)"), 10);
}

test "TestInspect" {
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    const allocator = arena.allocator();
    defer arena.deinit();

    try testInspect(allocator, "let f = fn(x, y) { x + y }; f", "fn(x, y) {\n(x + y)\n}");
    try testInspect(allocator, "fn() { 1 }", "fn() {\n1\n}");
    try testInspect(allocator, "5", "5");
    try testInspect(allocator, "true", "true");
    try testInspect(allocator, "if (false) { 1 }", "null");
    try testInspect(allocator, "\"hi\"", "hi");
    try testInspect(allocator, "len", "Builtin Fn");
    try testInspect(allocator, "1 / 0", "Error: Division by zero: 1 / 0");
}

fn testStringConcatenation(alloc: std.mem.Allocator, input: [:0]const u8, expected: []const u8) !void {
    const evaluated = (try testEval(alloc, input)).?;

    try testing.expectEqual(ObjectType.String, evaluated.ty);
    const string = evaluated.cast(.String);
    try testing.expectEqualStrings(expected, string.value);
}

fn testFunction(alloc: std.mem.Allocator, input: [:0]const u8) !void {
    var buf: std.Io.Writer.Allocating = .init(alloc);
    defer buf.deinit();
    const evaluated = (try testEval(alloc, input)).?;

    try testing.expectEqual(ObjectType.Function, evaluated.ty);
    const fnObj = evaluated.cast(.Function);

    try testing.expectEqual(fnObj.parameters.len, 1);
    try (&fnObj.parameters[0].base).toString(&buf.writer); // upcast
    try testing.expectEqualStrings("x", buf.written());

    const expectedBody = "(x + 2)";

    buf.clearRetainingCapacity(); // clear previous use
    try (&fnObj.body.base).toString(&buf.writer); // upcast
    try testing.expectEqualStrings(expectedBody, buf.written());
}

fn testLetStatement(alloc: std.mem.Allocator, input: [:0]const u8, expected: i64) !void {
    const evaluated = (try testEval(alloc, input)).?;
    try testIntegerObject(evaluated, expected);
}

fn testErrorHandling(alloc: std.mem.Allocator, input: [:0]const u8, expected: []const u8) !void {
    const evaluated = (try testEval(alloc, input)).?;

    try testing.expectEqual(ObjectType.Error, evaluated.ty);
    const errorObj = evaluated.cast(.Error);
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

// `alloc` is the test's arena and also owns the AST. Function objects point into the
// AST, so the parser is not deinitialized here; the arena frees everything at test end.
fn testEval(alloc: std.mem.Allocator, input: [:0]const u8) !?*Object {
    var parser = Parser.init(input, alloc);
    var envPtr = try alloc.create(Environment);
    envPtr.store = std.StringHashMap(*Object).init(alloc);
    envPtr.outer = null;
    const rootNode = (try parser.parseProgram());
    if (parser.errors.items.len != 0) {
        for (parser.errors.items) |err| {
            std.debug.print("parser error in `{s}`: {s}\n", .{ input, err.msg });
        }
        return error.TestUnexpectedParseError;
    }

    return try evaluator.eval(alloc, rootNode, envPtr);
}

// Runs testEval on a thread with the evaluator's stack size, as the REPL does, so deep
// recursion reaches max_call_depth on any OS instead of overflowing the test runner's stack.
fn testEvalOnEvaluatorStack(alloc: std.mem.Allocator, input: [:0]const u8) !*Object {
    const Run = struct {
        fn run(a: std.mem.Allocator, in: [:0]const u8, result: *anyerror!?*Object) void {
            result.* = testEval(a, in);
        }
    };
    var result: anyerror!?*Object = null;
    const thread = try std.Thread.spawn(.{ .stack_size = evaluator.stack_size }, Run.run, .{ alloc, input, &result });
    thread.join();
    return (try result).?;
}

fn testInspect(alloc: std.mem.Allocator, input: [:0]const u8, expected: []const u8) !void {
    const evaluated = (try testEval(alloc, input)).?;
    var buf: std.Io.Writer.Allocating = .init(alloc);
    try evaluated.Inspect(alloc, &buf.writer);
    try testing.expectEqualStrings(expected, buf.written());
}

fn testIntegerObject(obj: *Object, expected: i64) !void {
    try testing.expectEqual(ObjectType.Integer, obj.ty);
    const integerValue = obj.cast(.Integer);
    try testing.expectEqual(expected, integerValue.value);
}

fn testBooleanObject(obj: *Object, expected: bool) !void {
    try testing.expectEqual(ObjectType.Boolean, obj.ty);
    const boolValue = obj.cast(.Boolean);
    try testing.expectEqual(expected, boolValue.value);
}
