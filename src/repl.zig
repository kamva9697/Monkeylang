const std = @import("std");
const print = std.debug.print;
const Lexer = @import("./lexer.zig").Lexer;
const token = @import("./token.zig");
const equal = std.mem.eql;
const Prompt = ">> ";

pub fn start() !void {
    const reader = std.io.getStdIn().reader();
    var buf: [1024]u8 = undefined;
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    while (true) {
        print("{s}", .{Prompt});
        var line = try reader.readUntilDelimiterOrEof(&buf, '\n');
        var input = std.mem.trim(u8, line.?, "\r\n");

        var stripLine = try std.mem.concatWithSentinel(gpa.allocator(), u8, &[_][]const u8{input}, 0);

        var lex = Lexer.init(stripLine);

        var tok: token.Token = lex.nextToken();

        while (tok.Type != token.TokenType.EOF) : ({
            tok = lex.nextToken();
        }) {
            print("Token[ .Type = {any}, .Literal = {s} ]\n", .{ tok.Type, tok.Literal });
        }
    }

    _ = gpa.deinit();
}

pub fn main() !void {
    print("Hello this is the Lollie-lang, 'cause its sweet ;)!\n", .{});
    print("Start typing in lollie syntax: \n", .{});
    try start();
}
