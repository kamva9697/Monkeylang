const std = @import("std");
const token = @import("token.zig");
const TokenType = @import("token.zig").TokenType;
const ArrayList = std.ArrayList;

///// Abstract Types/ Interfaces //////
pub const Node = union(enum) {
    statement: Statement,
    expression: Expression,

    pub fn tokenType(self: *const Node) TokenType {
        return switch (self.*) {
            inline else => |case| case.tokenType(),
        };
    }
    pub fn toString(self: *const Node) ![:0]u8 {
        return switch (self.*) {
            inline else => |*case| try case.toString(),
        };
    }
};

pub const Statement = union(enum) {
    letStatement: LetStatement,
    returnStatement: ReturnStatement,

    pub fn tokenType(self: *Statement) TokenType {
        return switch (self.*) {
            inline else => |*case| case.tokenType(),
        };
    }

    pub fn statementNode(self: *Statement) void {
        switch (self) {
            inline else => |case| case.statementNode(),
        }
    }

    pub fn toString(self: *const Statement) ![:0]u8 {
        return switch (self.*) {
            inline else => |*case| try case.toString(),
        };
    }
};

////// Concrete Types ///////////////
pub const ReturnStatement = struct {
    token: token.Token,
    returnValue: ?Expression,

    pub fn statementNode() void {}

    pub fn tokenType(self: *const ReturnStatement) TokenType {
        return self.token.Type;
    }
    // Free the returned Heap allocated Slice
    pub fn toString(self: *const ReturnStatement) ![:0]u8 {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        var outBuf = ArrayList(u8).init(gpa.allocator());
        defer outBuf.deinit();

        try outBuf.writer().print("{s}", .{self.token.Literal});

        if (self.returnValue) |returnValue| {
            var val = try returnValue.toString();
            try outBuf.writer().print(" {any}", .{val});
        }
        try outBuf.writer().print(";", .{});
        return outBuf.toOwnedSliceSentinel(0);
    }
};

pub const LetStatement = struct {
    token: token.Token,
    name: Identifier,
    value: Expression,

    pub fn statementNode() void {}

    pub fn tokenType(self: *const LetStatement) TokenType {
        return self.token.Type;
    }

    pub fn toString(self: *const LetStatement) ![:0]u8 {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        var outBuf = ArrayList(u8).init(gpa.allocator());
        defer outBuf.deinit();

        var val = try self.value.toString();
        try outBuf.writer().print("{s} {s} = {s};", .{ self.token.Literal, self.name.value, val });
        return outBuf.toOwnedSliceSentinel(0);
    }
};

pub const ExpressionStatement = struct {
    token: token.Token,
    expression: ?Expression,

    pub fn statementNode() void {}

    pub fn tokenType(self: *const ReturnStatement) TokenType {
        return self.token.Type;
    }
    pub fn toString(self: *const ExpressionStatement) ![:0]u8 {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        var slice = try ArrayList(u8).init(gpa.allocator());

        if (self.expression) |expr| {
            var val = try expr.toString();
            try slice.appendSlice(val);
            return slice.toOwnedSliceSentinel(0);
        }

        try slice.appendSlice("");
        return slice.toOwnedSliceSentinel(0);
    }
};

//////////// Expresions /////////////
pub const Expression = union(enum) {
    identifier: Identifier,

    pub fn tokenType(self: *const Expression) TokenType {
        return self.identifier.token.Type;
    }

    pub fn statementNode(self: *Expression) void {
        switch (self) {
            inline else => |case| case.statementNode(),
        }
    }
    pub fn toString(self: *const Expression) ![:0]u8 {
        return switch (self.*) {
            inline else => |case| case.toString(),
        };
    }
};

pub const Identifier = struct {
    token: token.Token,
    value: []const u8,

    pub fn expression() void {}

    pub fn tokenType(self: *Identifier) TokenType {
        return self.token.Type;
    }
    pub fn toString(self: *const Identifier) ![:0]u8 {
        var alloc = std.heap.GeneralPurposeAllocator(.{}){};
        return try std.mem.concatWithSentinel(alloc.allocator(), u8, &[_][]const u8{self.value}, 0);
    }
};

///////////// AST //////////////

pub const Program = struct {
    statements: std.ArrayList(Node),

    pub fn init(alloc: std.mem.Allocator) Program {
        return Program{
            .statements = std.ArrayList(Node).init(alloc),
        };
    }

    pub fn tokenType(self: *Program) TokenType {
        if (self.statements.items.len > 0) {
            self.statements.items[0].token.tokenType();
        } else {
            return "";
        }
    }

    pub fn toString(self: *const Program) ![]u8 {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};

        var array = std.ArrayList(u8).init(gpa.allocator());
        defer array.deinit();

        for (self.statements.items) |stmt| {
            try array.appendSlice(try stmt.toString());
        }
        return try array.toOwnedSliceSentinel(0);
    }

    pub fn deinit(self: @This()) void {
        self.statements.deinit();
    }
};

test "Tree Test" {
    const testing = std.testing;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var program = Program.init(gpa.allocator());
    defer program.deinit();

    try program.statements.append(Node{ .statement = Statement{
        .letStatement = LetStatement{
            .token = token.Token{ .Type = .LET, .Literal = "let" },
            .name = Identifier{
                .token = token.Token{ .Type = .IDENT, .Literal = "myVar" },
                .value = "myVar",
            },
            .value = Expression{ .identifier = Identifier{
                .token = token.Token{ .Type = .IDENT, .Literal = "anotherVar" },
                .value = "anotherVar",
            } },
        },
    } });

    try testing.expectEqualStrings("let myVar = anotherVar;", try program.toString());
}
