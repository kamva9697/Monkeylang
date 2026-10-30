const std = @import("std");
const repl = @import("./repl.zig");
const evaluator = @import("./evaluator.zig");
const print = std.debug.print;

pub fn main(init: std.process.Init) !void {
    print("Hello this is the Monkey-lang, Code your world, one banana at a time\n", .{});
    print("Start typing, Monkey-style :) \n\n", .{});

    var arena = std.heap.ArenaAllocator.init(init.gpa);
    defer arena.deinit();

    // Run the REPL on a thread with a known, large stack so deep recursion in Monkey
    // code stops at evaluator.max_call_depth instead of overflowing the stack.
    var result: anyerror!void = {};
    const thread = try std.Thread.spawn(
        .{ .stack_size = evaluator.stack_size },
        runRepl,
        .{ arena.allocator(), init.io, &result },
    );
    thread.join();
    return result;
}

fn runRepl(gpa: std.mem.Allocator, io: std.Io, result: *anyerror!void) void {
    result.* = repl.start(gpa, io);
}
