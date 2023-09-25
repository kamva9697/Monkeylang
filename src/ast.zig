const std = @import("std");
const token = @import("token.zig");
const TokenType = @import("token.zig").TokenType;
const ArrayList = std.ArrayList;

///// Abstract Types/ Interfaces //////
pub const Node = union(enum) {
    statement: Statement,

    pub fn tokenType(self: *const Node) TokenType {
        return switch (self.*) {
            inline else => |case| case.tokenType(),
        };
    }
    pub fn toString(self: *const Node, allocator: std.mem.Allocator) []u8 {
        return switch (self.*) {
            inline else => |*case| case.toString(allocator),
        };
    }
};

pub const Statement = union(enum) {
    letStatement: LetStatement,
    returnStatement: ReturnStatement,
    expressionStatement: ExpressionStatement,

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

    pub fn toString(self: *const Statement, allocator: std.mem.Allocator) []u8 {
        return switch (self.*) {
            inline else => |*case| case.toString(allocator),
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

    pub fn toString(self: *const ReturnStatement, allocator: std.mem.Allocator) []u8 {
        if (self.returnValue) |returnValue| {
            var val = returnValue.toString(allocator);
            const formatString = "{any} {any};";

            const slice = std.fmt.allocPrint(allocator, formatString, .{ self.token.Literal, val }) catch |err| {
                std.debug.panic("Error occured: {any}", .{err});
            };

            return slice[0..];
        }

        const formatSting = "{any}";
        const slice = std.fmt.allocPrint(allocator, formatSting, .{self.token.Literal}) catch |err| {
            std.debug.panic("Error occured: {any}", .{err});
        };

        return slice[0..];
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

    pub fn toString(self: *const LetStatement, allocator: std.mem.Allocator) []u8 {
        var val = self.value.toString(allocator);
        const formatString = "{s} {s} = {s};";

        const slice = std.fmt.allocPrint(allocator, formatString, .{ self.token.Literal, self.name.value, val[0..] }) catch |err| {
            std.debug.panic("Error occured: {any}", .{err});
        };
        return slice[0..];
    }
};

pub const ExpressionStatement = struct {
    token: token.Token,
    expression: ?Expression,

    pub fn statementNode() void {}

    pub fn tokenType(self: *const ExpressionStatement) TokenType {
        return self.token.Type;
    }
    pub fn toString(self: *const ExpressionStatement, allocator: std.mem.Allocator) []u8 {
        var slice = ArrayList(u8).init(allocator);

        if (self.expression) |expr| {
            var val = expr.toString(allocator);
            slice.appendSlice(val) catch unreachable;
            return slice.toOwnedSliceSentinel(0) catch |err| {
                std.debug.panic("An error occured: {any}", .{err});
            };
        }

        slice.appendSlice("") catch unreachable;
        return slice.toOwnedSliceSentinel(0) catch |err| {
            std.debug.panic("An error occured: {any}", .{err});
        };
    }
};

//////////// Expresions /////////////
pub const Expression = union(enum) {
    identifier: Identifier,
    integerLiteral: IntegerLiteral,

    pub fn tokenType(self: *const Expression) TokenType {
        return self.identifier.token.Type;
    }

    pub fn statementNode(self: *Expression) void {
        switch (self) {
            inline else => |case| case.statementNode(),
        }
    }
    pub fn toString(self: *const Expression, allocator: std.mem.Allocator) []u8 {
        return switch (self.*) {
            inline else => |case| case.toString(allocator),
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
    pub fn toString(self: *const Identifier, allocator: std.mem.Allocator) []u8 {
        var buf = std.fmt.allocPrint(allocator, "{s}", .{self.value}) catch |err| {
            std.debug.panic("[IntegerLiteral-toString]: An error occured {any}", .{err});
        };

        return buf[0..];
    }
};

pub const IntegerLiteral = struct {
    token: token.Token,
    value: u32,

    pub fn expressionNode(self: *const IntegerLiteral) void {
        _ = self;
    }
    pub fn TokenLiteral(self: *const IntegerLiteral) []const u8 {
        return self.token.Literal;
    }

    pub fn toString(self: *const IntegerLiteral, allocator: std.mem.Allocator) []u8 {
        var buf = std.fmt.allocPrint(allocator, "{d}", .{self.value}) catch |err| {
            std.debug.panic("[IntegerLiteral-toString]: An error occured {any}", .{err});
        };
        return buf[0..];
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

    pub fn tokenType(self: Program) TokenType {
        if (self.statements.items.len > 0) {
            self.statements.items[0].token.tokenType();
        } else {
            return "";
        }
    }

    pub fn toString(self: Program, allocator: std.mem.Allocator) []u8 {
        var array = std.ArrayList(u8).init(allocator);

        for (self.statements.items) |stmt| {
            array.appendSlice(stmt.toString(allocator)) catch unreachable;
        }
        return array.toOwnedSliceSentinel(0) catch |err| {
            std.debug.panic("An error occured: {any}", .{err});
        };
    }

    pub fn deinit(self: @This()) void {
        self.statements.deinit();
    }
};

test "toString" {
    const testing = std.testing;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    var statement = LetStatement{
        .token = token.Token{ .Type = .LET, .Literal = "let" },
        .name = Identifier{
            .token = token.Token{ .Type = .IDENT, .Literal = "myVar" },
            .value = "myVar",
        },
        .value = Expression{ .identifier = Identifier{
            .token = token.Token{ .Type = .IDENT, .Literal = "anotherVar" },
            .value = "anotherVar",
        } },
    };

    const stmt = statement.toString(gpa.allocator());

    try testing.expect(stmt.len > 0);
    try testing.expectEqualStrings("let myVar = anotherVar;", stmt);
}

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

    try testing.expectEqualStrings("let myVar = anotherVar;", program.toString(gpa.allocator()));
}
