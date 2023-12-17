const std = @import("std");
const print = std.debug.print;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const token = @import("token.zig");
const equal = std.mem.eql;
const Prompt = ">> ";
const evaluator = @import("evaluator.zig");
const Environment = @import("environment.zig").Environment;
const _Object = @import("object.zig");
const Object = _Object.Object;

pub fn main() !void {
    print("Hello this is the Monkey-lang, Code your world, one banana at a time\n", .{});
    print("Start typing, Monkey-style :) \n\n", .{});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    try start(arena.allocator());
}

pub fn start(gpa: std.mem.Allocator) !void {
    var bufIn: [1024]u8 = undefined;
    var reader = std.io.getStdIn().reader();
    var bufOut = std.ArrayList(u8).init(gpa);
    const writer = bufOut.writer();
    const env = try Environment.newEnvironment(gpa);
    const internPool = evaluator.InternPool(gpa);
    _ = internPool;

    while (true) {
        print("{s}", .{Prompt});
        const line = reader.readUntilDelimiterOrEof(&bufIn, '\n') catch {
            std.debug.print("Input too Long, input buffer size is 1 Kib\n", .{});
            continue;
        };
        const input = std.mem.trim(u8, line.?, "\r\n");

        const stripLine = try std.mem.concatWithSentinel(gpa, u8, &[_][]const u8{input}, 0);

        var parser = Parser.init(stripLine, gpa);

        const program = try parser.parseProgram();
        if (parser.errors.items.len != 0) {
            try printParserErrors(parser.errors.items);
            continue;
        }

        const evaluated = try evaluator.eval(gpa, program, env);
        if (evaluated) |evaled| {
            try evaled.Inspect(gpa, writer);
        }

        // try program.toString(writer);
        print("{s}", .{bufOut.items});
        print("\n", .{});
        bufOut.clearRetainingCapacity();
    }
}

pub fn printParserErrors(errors: anytype) !void {
    for (errors) |err| {
        print("Error: {s}\n", .{err.msg});
    }
}
