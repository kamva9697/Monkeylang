const std = @import("std");

const repl = @import("./repl.zig");

const print = std.debug.print;

pub fn main() !void {
    print("Hello this is the Monkey-lang, Code your world, one banana at a time\n", .{});
    print("Start typing, Monkey-style :) \n\n", .{});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    try repl.start(arena.allocator());
}
