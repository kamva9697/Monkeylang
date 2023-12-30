const std = @import("std");
const Allocator = std.mem.Allocator;
const Object = @import("object.zig").Object;
const evaluator = @import("evaluator.zig");
const Environment = @import("environment.zig").Environment;

pub fn len(alloc: Allocator, args: []*Object) !*Object {
    if (args.len != 1) {
        return try evaluator.newError(alloc, "Wrong number of arguments: {d}", .{args.len});
    }
    return switch (args[0].ty) {
        .String => {
            const str = args[0].cast(.String);
            const length = try Object.create(Object.Integer, alloc, Object.Integer{ .value = @intCast(str.value.len) });
            return length.toObject();
        },
        else => try evaluator.newError(
            alloc,
            "Argument to 'len' not supported got: {any}",
            .{args[0].ty},
        ),
    };
}

fn puts(alloc: Allocator, args: []*Object) !*Object {
    var BufIn = std.ArrayList(u8).init(alloc);
    defer BufIn.deinit();
    const writer = BufIn.writer();
    for (args) |arg| {
        try arg.Inspect(alloc, writer);
    }
    const str = try Object.create(
        Object.String,
        alloc,
        Object.String{ .value = try BufIn.toOwnedSliceSentinel(0) },
    );

    return str.toObject();
}

pub fn builtins(str: []const u8) ?*const Object {
    if (std.mem.eql(u8, "len", str)) {
        const builtin = &Object.Builtin{
            ._fn = len,
        };
        return &builtin.base;
    }
    if (std.mem.eql(u8, "puts", str)) {
        const builtin = &Object.Builtin{
            ._fn = puts,
        };
        return &builtin.base;
    }
    return null;
}

// pub const builtins = std.ComptimeStringMap(Object.Builtin, .{
//     .{
//         "len", Object.Builtin{
//             ._fn = len,
//         },
//     },
// });
