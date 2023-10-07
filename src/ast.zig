const std = @import("std");
const token = @import("token.zig");
const TokenType = @import("token.zig").TokenType;
const ArrayList = std.ArrayList;

///// Abstract Types/ Interfaces //////
pub const Node = struct {
    id: Id,

    pub const Id = enum {
        LetStatement,
        ReturnStatement,
        Identifier,
        IntegerLiteral,
        PrefixExpression,
        InfixExpression,

        pub fn Type(comptime id: Id) type {
            return switch (id) {
                .LetStatement => LetStatement,
                .ReturnStatement => ReturnStatement,
                .Identifier => Identifier,
                .IntegerLiteral => IntegerLiteral,
                .PrefixExpression => PrefixExpression,
                .InfixExpression => InfixExpression,
            };
        }
    };

    ////// Concrete Types ///////////////
    pub const ReturnStatement = struct {
        base: Node = .{ .id = .ReturnStatement },
        token: token.Token,
        returnValue: ?*Node,
    };

    pub const LetStatement = struct {
        base: Node = .{ .id = .LetStatement },
        token: token.Token,
        name: Identifier,
        value: ?*Node,
    };

    pub const Identifier = struct {
        base: Node = .{ .id = .Identifier },
        token: token.Token,
        value: []const u8,
    };

    pub const IntegerLiteral = struct {
        base: Node = .{ .id = .IntegerLiteral },
        token: token.Token,
        value: u32,
    };

    pub const PrefixExpression = struct {
        base: Node = .{ .id = .PrefixExpression },
        token: token.Token,
        operator: []const u8,
        rightExprPtr: ?*Node,
    };

    pub const InfixExpression = struct {
        base: Node = .{ .id = .InfixExpression },
        token: token.Token,
        leftExprPtr: ?*Node,
        operator: []const u8,
        rightExprPtr: ?*Node,
    };

    pub fn cast(base: *Node, comptime id: Id) ?*id.Type() {
        if (base.id == id) {
            return @fieldParentPtr(id.Type(), "base", base);
        }
        return null;
    }

    pub fn tokenType(base: *Node) TokenType {
        return switch (base.id) {
            inline else => |case| {
                // const node = base.cast(case).?;
                const node = @fieldParentPtr(Id.Type(case), "base", base);
                return node.token.Type;
            },
        };
    }

    pub fn toString(node: *const Node, writer: anytype) !void {
        return switch (node.id) {
            .ReturnStatement => {
                const returnStatementNode = @fieldParentPtr(Node.ReturnStatement, "base", node);

                if (returnStatementNode.returnValue) |returnValue| {
                    try writer.writeAll(returnStatementNode.token.Literal);
                    try writer.writeAll(" ");
                    try returnValue.toString(writer);
                    try writer.writeAll(";");
                } else {
                    try writer.writeAll(returnStatementNode.token.Literal);
                    try writer.writeAll(";");
                }
            },
            .LetStatement => {
                const letStmtNode = @fieldParentPtr(Node.LetStatement, "base", node);

                try writer.writeAll(letStmtNode.token.Literal);
                try writer.writeAll(" ");
                var identNode = &letStmtNode.name.base;
                try identNode.toString(writer);
                try writer.writeAll(" = ");
                try letStmtNode.value.?.toString(writer);
                try writer.writeAll(";");
            },
            .Identifier => {
                const identifierNode = @fieldParentPtr(Identifier, "base", node);
                try writer.writeAll(identifierNode.value);
            },
            .IntegerLiteral => {
                const integerLiteral = @fieldParentPtr(IntegerLiteral, "base", node);

                try writer.print("{d}", .{integerLiteral.value});
            },
            .PrefixExpression => {
                const prefixExpr = @fieldParentPtr(PrefixExpression, "base", node);
                try writer.writeAll(prefixExpr.operator);

                if (prefixExpr.rightExprPtr) |rightExpr| {
                    try rightExpr.toString(writer);
                }
            },
            .InfixExpression => {
                const infixExpr = @fieldParentPtr(InfixExpression, "base", node);

                if (infixExpr.leftExprPtr) |leftExpr| {
                    try leftExpr.toString(writer);
                }

                try writer.writeAll(infixExpr.operator);

                if (infixExpr.rightExprPtr) |rightExpr| {
                    try rightExpr.toString(writer);
                }
            },
        };
    }
};

///////////// AST //////////////
pub const Tree = struct {
    statements: std.ArrayList(*Node),

    // arena: std.heap.ArenaAllocator.State,
    allocator: std.mem.Allocator,

    pub fn toString(self: *Tree, writer: anytype) !void {
        for (self.statements) |st| {
            try st.toString(writer);
        }
    }

    pub fn deinit(self: *Tree) void {
        self.arena.promote(self.allocator).deinit();
    }
};

test "toString" {
    const testing = std.testing;

    var alloc = testing.allocator;
    var buf = std.ArrayList(u8).init(alloc);
    defer buf.deinit();

    var identPtr = try alloc.create(Node.Identifier);
    defer alloc.destroy(identPtr);

    identPtr.* = Node.Identifier{ .token = token.Token{ .Type = .IDENT, .Literal = "anotherVar" }, .value = "anotherVar" };

    var statement = Node.LetStatement{
        .token = token.Token{ .Type = .LET, .Literal = "let" },
        .name = Node.Identifier{
            .token = token.Token{ .Type = .IDENT, .Literal = "myVar" },
            .value = "myVar",
        },
        .value = &identPtr.base,
    };

    const node = &statement.base;
    try node.toString(buf.writer());

    try testing.expect(buf.items.len > 0);

    try testing.expectEqualStrings("let myVar = anotherVar;", buf.items);
}
// test "Tree Test" {
//     const testing = std.testing;

//     var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//     var allocator = gpa.allocator();
//     var program = Program.init(allocator);
//     defer program.deinit();

//     var identPtr = allocator.create(Node.Identifier) catch unreachable;
//     identPtr.* = Node.Identifier{ .token = token.Token{ .Type = .IDENT, .Literal = "anotherVar" }, .value = "anotherVar" };

//     var letStmtPtr = allocator.create(Node.LetStatement) catch unreachable;

//     letStmtPtr.* = Node.LetStatement{ .token = token.Token{ .Type = .LET, .Literal = "let" }, .name = Node.Identifier{
//         .token = token.Token{ .Type = .IDENT, .Literal = "myVar" },
//         .value = "myVar",
//     }, .value = Node.Expression{
//         .identifier = identPtr,
//     } };

//     try program.statements.append(Node{ .statement = Statement{
//         .letStatement = letStmtPtr,
//     } });

//     var string = program.toString(allocator);
//     try testing.expectEqualStrings("let myVar = anotherVar;", string);
// }
