const std = @import("std");
const print = std.debug.print;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const token = @import("token.zig");
const equal = std.mem.eql;
const Prompt = ">> ";

pub fn start(gpa: std.mem.Allocator) !void {
    var bufIn: [1024]u8 = undefined;
    var bufOut = std.ArrayList(u8).init(gpa);
    var reader = std.io.getStdIn().reader();
    var writer = bufOut.writer();

    while (true) {
        print("{s}", .{Prompt});
        var line = try reader.readUntilDelimiterOrEof(&bufIn, '\n');
        var input = std.mem.trim(u8, line.?, "\r\n");

        var stripLine = try std.mem.concatWithSentinel(gpa, u8, &[_][]const u8{input}, 0);

        var par = Parser.init(stripLine, gpa);

        var program = try par.parseProgram();
        if (par.errors.items.len != 0) {
            try printParserErrors(par.errors.items);
            continue;
        }

        try program.toString(writer);
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

pub fn main() !void {
    print("Hello this is the Monkey-lang, Code your world, one banana at a time\n", .{});
    print("Start typing, Monkey-style :) \n\n", .{});

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    try start(arena.allocator());
}
