const std = @import("std");
const Allocator = std.mem.Allocator;
const ast = @import("ast.zig");
const Lexer = @import("lexer.zig").Lexer;
const token = @import("token.zig");
const TokenType = token.TokenType;
const Token = token.Token;
const Node = ast.Node;
const Operator = ast.Operator;
const mem = std.mem;
const testing = std.testing;
const Environment = @import("environment.zig").Environment;

pub const Object = struct {
    ty: ObjectType,
    const Self = @This();

    pub const ObjectType = enum {
        Integer,
        Boolean,
        Null,
        ReturnValue,
        Function,
        Error,

        pub fn Type(comptime ty: ObjectType) type {
            return switch (ty) {
                .Integer => Integer,
                .Boolean => Boolean,
                .Null => Null,
                .ReturnValue => ReturnValue,
                .Function => Function,
                .Error => Error,
            };
        }
        pub fn toString(ty: ObjectType) []const u8 {
            return switch (ty) {
                .Integer => "Integer",
                .Boolean => "Boolean",
                .Null => "Null",
                .ReturnValue => "ReturnValue",
                .Function => "Function",
                .Error => "Error",
            };
        }
    };

    pub fn cast(base: *Object, comptime ty: ObjectType) ?*ty.Type() {
        if (base.ty == ty) {
            return @fieldParentPtr(ty.Type(), "base", base);
        }
        return null;
    }

    pub fn Inspect(self: *Self, alloc: Allocator, writer: anytype) !void {
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
                try rv.value.Inspect(alloc, writer);
            },
            .Null => {
                try writer.print("null", .{});
            },
            .Function => {
                const func = cast(self, .Function).?;

                var params = std.ArrayList(u8).init(alloc);
                const paramsWriter = params.writer();
                for (func.parameters) |param| {
                    try (&param.base).toString(paramsWriter);
                }
                try writer.writeAll("fn");
                try writer.writeAll("(");
                // const joinedParms = try std.mem.join(alloc, ", ", params.items);
                try writer.print("{any}", .{params.items});
                try writer.writeAll(") {\n");
                try (&func.body.base).toString(writer);
                try writer.writeAll("\n");
            },
            .Error => {
                const err = cast(self, .Error).?;
                try writer.print("Error: {s}", .{err.message});
            },
        }
    }

    pub const Function = struct {
        base: Object = .{ .ty = .Function },
        parameters: []*Node.Identifier,
        body: *Node.Block,
        env: *Environment,
    };

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

    pub const Error = struct {
        base: Object = .{ .ty = .Error },
        message: []const u8,
    };
};
