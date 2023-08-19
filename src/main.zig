const std = @import("std");
const token = @import("./token.zig");
const lexerTests = @import("./lexerTest.zig");
const repl = @import("./repl.zig");

pub fn main() anyerror!void {
    try repl.start();
}
