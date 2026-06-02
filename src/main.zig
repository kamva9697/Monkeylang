const std = @import("std");
const repl = @import("./repl.zig");
const print = std.debug.print;

pub fn main(init: std.process.Init) !void {
    print("Hello this is the Monkey-lang, Code your world, one banana at a time\n", .{});
    print("Start typing, Monkey-style :) \n\n", .{});

    var arena = std.heap.ArenaAllocator.init(init.gpa);
    defer arena.deinit();

    try repl.start(arena.allocator(), init.io);
}
