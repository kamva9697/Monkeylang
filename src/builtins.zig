const std = @import("std");
const Allocator = std.mem.Allocator;
const Object = @import("object.zig").Object;
const evaluator = @import("evaluator.zig");

const builtins = std.StaticStringMap(*const Object.Builtin).initComptime(.{
    .{ "len", &Object.Builtin{ ._fn = len } },
    .{ "puts", &Object.Builtin{ ._fn = puts } },
});

pub fn lookup(name: []const u8) ?*Object {
    const builtin = builtins.get(name) orelse return null;
    return @constCast(&builtin.base);
}

fn len(alloc: Allocator, args: []*Object) anyerror!*Object {
    if (args.len != 1) {
        return evaluator.newError(alloc, "Wrong number of arguments: got {d}, want 1", .{args.len});
    }
    return switch (args[0].ty) {
        .String => {
            const str = args[0].cast(.String);
            const length = try Object.create(Object.Integer, alloc, .{ .value = @intCast(str.value.len) });
            return length.toObject();
        },
        else => evaluator.newError(
            alloc,
            "Argument to 'len' not supported, got {s}",
            .{args[0].ty.toString()},
        ),
    };
}

fn puts(alloc: Allocator, args: []*Object) anyerror!*Object {
    var out: std.Io.Writer.Allocating = .init(alloc);
    defer out.deinit();
    for (args) |arg| {
        try arg.Inspect(alloc, &out.writer);
        try out.writer.writeByte('\n');
    }
    std.debug.print("{s}", .{out.written()});

    const obj = @constCast(evaluator.NULL_VALUE);
    return obj.toObject();
}
