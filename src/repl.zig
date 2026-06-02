const std = @import("std");
const print = std.debug.print;
const Parser = @import("parser.zig").Parser;
const Prompt = ">> ";
const evaluator = @import("evaluator.zig");
const Environment = @import("environment.zig").Environment;

pub fn main(init: std.process.Init) !void {
    print("Hello this is the Monkey-lang, Code your world, one banana at a time\n", .{});
    print("Start typing, Monkey-style :) \n\n", .{});

    var arena = std.heap.ArenaAllocator.init(init.gpa);
    defer arena.deinit();

    try start(arena.allocator(), init.io);
}

pub fn start(gpa: std.mem.Allocator, io: std.Io) !void {
    var input_buffer: [1024]u8 = undefined;
    var reader = std.Io.File.stdin().reader(io, &input_buffer);
    var output = std.Io.Writer.Allocating.init(gpa);
    defer output.deinit();
    const env = try Environment.newEnvironment(gpa);

    while (true) {
        print("{s}", .{Prompt});
        const line = reader.interface.takeDelimiter('\n') catch |err| switch (err) {
            error.StreamTooLong => {
                std.debug.print("Input too Long, input buffer size is 1 Kib\n", .{});
                _ = reader.interface.discardDelimiterInclusive('\n') catch {};
                continue;
            },
            error.ReadFailed => {
                std.debug.print("Failed to read from stdin\n", .{});
                continue;
            },
        };
        const raw_line = line orelse break;
        const input = std.mem.trim(u8, raw_line, "\r\n");

        const stripLine = try std.mem.concatWithSentinel(gpa, u8, &[_][]const u8{input}, 0);

        var parser = Parser.init(stripLine, gpa);

        const program = try parser.parseProgram();
        if (parser.errors.items.len != 0) {
            try printParserErrors(parser.errors.items);
            continue;
        }

        const evaluated = try evaluator.eval(gpa, program, env);
        if (evaluated) |evaled| {
            try evaled.Inspect(gpa, &output.writer);
        }

        print("{s}", .{output.written()});
        print("\n", .{});
        output.clearRetainingCapacity();
    }
}

pub fn printParserErrors(errors: anytype) !void {
    for (errors) |err| {
        print("Error: {s}\n", .{err.msg});
    }
}
