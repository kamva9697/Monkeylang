const std = @import("std");
const ast = @import("ast.zig");
const Lexer = @import("lexer.zig").Lexer;
const token = @import("token.zig");
const TokenType = token.TokenType;
const Token = token.Token;
const Node = ast.Node;
const Operator = ast.Operator;
const mem = std.mem;
const testing = std.testing;

pub const Object = struct {
    ty: ObjectType,
    const Self = @This();

    pub const ObjectType = enum {
        Integer,
        Boolean,
        Null,
        ReturnValue,

        pub fn Type(comptime ty: ObjectType) type {
            return switch (ty) {
                .Integer => Integer,
                .Boolean => Boolean,
                .Null => Null,
                .ReturnValue => ReturnValue,
            };
        }
    };

    pub fn cast(base: *Object, comptime ty: ObjectType) ?*ty.Type() {
        if (base.ty == ty) {
            return @fieldParentPtr(ty.Type(), "base", base);
        }
        return null;
    }

    pub fn Inspect(self: *Self, writer: anytype) !void {
        switch (self.ty) {
            .Integer => {
                const int = cast(self, .Integer).?;
                try writer.print("{d}", .{int.value});
            },
            .Boolean => {
                const _bool = cast(self, .Boolean).?;
                try writer.print("{any}", .{_bool.value});
            },
            .ReturnValue => {
                const rv = cast(self, .ReturnValue).?;
                try rv.value.Inspect(writer);
            },
            .Null => {
                try writer.print("null", .{});
            },
        }
    }

    pub const Integer = struct {
        base: Object = .{ .ty = .Integer },
        value: i64,
    };
    pub const Boolean = struct {
        base: Object = .{ .ty = .Boolean },
        value: bool,
    };
    pub const Null = struct {
        base: Object = .{ .ty = .Null },
    };
    pub const ReturnValue = struct {
        base: Object = .{ .ty = .ReturnValue },
        value: *Object,
    };
};
