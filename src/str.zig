const std = @import("std");
const print = std.debug.print;
const mem = std.mem;
const testing = std.testing;

pub fn main() !void {
    var msg = "Kamva";
    const fun = "Function";
    const keywords = std.ComptimeStringMap([]const u8, .{
        .{ "fn", "fn" },
        .{ "let", "let" },
    });

    print("{s} == {s}\n", .{ "fn", @TypeOf(keywords.get("fn").?) });
    try testing.expect(mem.eql(u8, "fn", keywords.get("fn").?));

    try std.testing.expectEqual(msg, "Kamva");
    printIt(msg);
}

fn printIt(s: []const u8) void {
    std.debug.print("{any} : {s}\n", .{ @TypeOf(s), s });
}
