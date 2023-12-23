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

    pub const ObjectType = enum {
        Integer,
        Boolean,
        Null,
        ReturnValue,
        Function,
        Error,
        String,

        pub inline fn Type(comptime ty: ObjectType) type {
            return switch (ty) {
                .Integer => Integer,
                .Boolean => Boolean,
                .Null => Null,
                .ReturnValue => ReturnValue,
                .Function => Function,
                .String => String,
                .Error => Error,
            };
        }
        pub inline fn toString(ty: ObjectType) []const u8 {
            return switch (ty) {
                .Integer => "Integer",
                .Boolean => "Boolean",
                .Null => "Null",
                .ReturnValue => "ReturnValue",
                .Function => "Function",
                .String => "String",
                .Error => "Error",
            };
        }
    };

    pub inline fn cast(base: *Object, comptime ty: ObjectType) *ty.Type() {
        return @fieldParentPtr(ty.Type(), "base", base);
    }

    pub inline fn create(comptime T: type, alloc: Allocator, value: T) !*T {
        const obj = try alloc.create(T);
        obj.* = value;
        return obj;
    }

    pub fn Inspect(self: *Object, alloc: Allocator, writer: anytype) !void {
        switch (self.ty) {
            .Integer => {
                const int = cast(self, .Integer);
                try writer.print("{d}", .{int.value});
            },
            .Boolean => {
                const _bool = cast(self, .Boolean);
                try writer.print("{any}", .{_bool.value});
            },
            .ReturnValue => {
                const rv = cast(self, .ReturnValue);
                try rv.value.Inspect(alloc, writer);
            },
            .Null => {
                try writer.print("null", .{});
            },
            .String => {
                const str = cast(self, .String);
                try writer.print("{s}", .{str.value});
            },
            .Function => {
                const func = cast(self, .Function);

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
                const err = cast(self, .Error);
                try writer.print("Error: {s}", .{err.message});
            },
        }
    }
    pub const String = struct {
        pub const Self = @This();
        base: Object = .{ .ty = .String },
        value: []const u8,

        pub inline fn toObject(self: *Self) *Object {
            return &self.base;
        }
    };

    pub const Function = struct {
        pub const Self = @This();
        base: Object = .{ .ty = .Function },
        parameters: []*Node.Identifier,
        body: *Node.Block,
        env: *Environment,

        pub inline fn toObject(self: *Self) *Object {
            return &self.base;
        }
    };

    pub const Integer = struct {
        pub const Self = @This();
        base: Object = .{ .ty = .Integer },
        value: i64,

        pub inline fn toObject(self: *Self) *Object {
            return &self.base;
        }
    };
    pub const Boolean = struct {
        pub const Self = @This();
        base: Object = .{ .ty = .Boolean },
        value: bool,

        pub inline fn toObject(self: *Self) *Object {
            return &self.base;
        }
    };
    pub const Null = struct {
        pub const Self = @This();
        base: Object = .{ .ty = .Null },

        pub inline fn toObject(self: *Self) *Object {
            return &self.base;
        }
    };
    pub const ReturnValue = struct {
        pub const Self = @This();
        base: Object = .{ .ty = .ReturnValue },
        value: *Object,

        pub inline fn toObject(self: *@This()) *Object {
            return &self.base;
        }
    };

    pub const Error = struct {
        pub const Self = @This();
        base: Object = .{ .ty = .Error },
        message: []const u8,

        pub inline fn toObject(self: *Self) *Object {
            return &self.base;
        }
    };
};
