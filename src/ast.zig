const std = @import("std");
const token = @import("token.zig");
const TokenType = @import("token.zig").TokenType;
const Token = @import("token.zig").Token;
const ArrayList = std.ArrayList;
const Parser = @import("parser.zig");
const io = std.io;

pub const Operator = enum {
    plus,
    minus,
    multiply,
    divide,
    not,
    greater_than,
    less_than,
    equal_to,
    not_equal_to,

    pub fn toString(op: Operator) []const u8 {
        return switch (op) {
            Operator.plus => "+",
            Operator.minus => "-",
            Operator.multiply => "*",
            Operator.divide => "/",
            Operator.not => "!",
            Operator.greater_than => ">",
            Operator.less_than => "<",
            Operator.equal_to => "==",
            Operator.not_equal_to => "!=",
        };
    }
    pub fn fromString(literal: []const u8) ?Operator {
        const ops = @typeInfo(Operator).Enum.fields;
        inline for (ops) |op| {
            const operator: Operator = @enumFromInt(op.value);

            if (std.mem.eql(u8, operator.toString(), literal)) {
                return operator;
            }
        }
        return null;
    }
};

///// Abstract Types/ Interfaces //////
pub const Node = struct {
    id: Id,

    pub const Id = enum {
        Tree,
        LetStatement,
        ReturnStatement,
        Identifier,
        IntegerLiteral,
        PrefixExpression,
        InfixExpression,
        Boolean,
        IfExpression,
        Block,
        FunctionLiteral,
        CallExpression,

        pub fn Type(comptime id: Id) type {
            return switch (id) {
                .Tree => Tree,
                .LetStatement => LetStatement,
                .ReturnStatement => ReturnStatement,
                .Identifier => Identifier,
                .IntegerLiteral => IntegerLiteral,
                .PrefixExpression => PrefixExpression,
                .InfixExpression => InfixExpression,
                .Boolean => Boolean,
                .IfExpression => IfExpression,
                .Block => Block,
                .FunctionLiteral => FunctionLiteral,
                .CallExpression => CallExpression,
            };
        }
    };

    ////// Concrete Types ///////////////
    pub const Tree = struct {
        base: Node = .{ .id = .Tree },
        statements: std.ArrayList(*Node),

        pub fn deinit(self: *Tree) void {
            self.statements.deinit();
        }
    };
    pub const ReturnStatement = struct {
        base: Node = .{ .id = .ReturnStatement },
        token: Token,
        returnValue: ?*Node,
    };

    pub const LetStatement = struct {
        base: Node = .{ .id = .LetStatement },
        token: Token,
        name: Identifier,
        value: ?*Node,
    };

    pub const Identifier = struct {
        base: Node = .{ .id = .Identifier },
        token: Token,
        value: []const u8,
    };

    pub const IntegerLiteral = struct {
        base: Node = .{ .id = .IntegerLiteral },
        token: Token,
        value: u32,
    };

    pub const PrefixExpression = struct {
        base: Node = .{ .id = .PrefixExpression },
        token: Token,
        operator: Operator,
        rightExprPtr: ?*Node,
    };

    pub const InfixExpression = struct {
        base: Node = .{ .id = .InfixExpression },
        token: Token,
        leftExprPtr: ?*Node,
        operator: Operator,
        rightExprPtr: ?*Node,
    };

    pub const Boolean = struct {
        base: Node = .{ .id = .Boolean },
        token: Token,
        value: bool,
    };

    pub const IfExpression = struct {
        base: Node = .{ .id = .IfExpression },
        token: Token,
        condition: *Node,
        consequence: *Block,
        alternative: ?*Block,
    };

    pub const Block = struct {
        base: Node = .{ .id = .Block },
        token: Token,
        statements: std.ArrayListUnmanaged(*Node),
    };

    pub const FunctionLiteral = struct {
        base: Node = .{ .id = .FunctionLiteral },
        token: Token,
        parameters: []*Identifier,
        body: *Block,
    };

    pub const CallExpression = struct {
        base: Node = .{ .id = .CallExpression },
        token: Token,
        function: *Node,
        arguments: []*Node,
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
                const node = @fieldParentPtr(Id.Type(case), "base", base);
                return node.token.Type;
            },
        };
    }

    pub fn toString(node: *Node, writer: anytype) !void {
        return switch (node.id) {
            .Tree => {
                const treeNode = node.cast(.Tree).?;

                for (treeNode.statements.items) |st| {
                    try st.toString(writer);
                }
            },
            .ReturnStatement => {
                const returnStatementNode = node.cast(.ReturnStatement).?;

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
                const letStmtNode = node.cast(.LetStatement).?;

                try writer.writeAll(letStmtNode.token.Literal);
                try writer.writeAll(" ");
                var identNode = &letStmtNode.name.base;
                try identNode.toString(writer);
                try writer.writeAll(" = ");
                try letStmtNode.value.?.toString(writer);
                try writer.writeAll(";");
            },
            .Identifier => {
                const identifierNode = node.cast(.Identifier).?;
                try writer.writeAll(identifierNode.value);
            },
            .IntegerLiteral => {
                const integerLiteral = node.cast(.IntegerLiteral).?;

                try writer.print("{d}", .{integerLiteral.value});
            },
            .PrefixExpression => {
                const prefixExpr = node.cast(.PrefixExpression).?;

                try writer.writeAll("(");
                try writer.writeAll(prefixExpr.operator.toString());

                if (prefixExpr.rightExprPtr) |rightExpr| {
                    try rightExpr.toString(writer);
                }
                try writer.writeAll(")");
            },
            .InfixExpression => {
                const infixExpr = node.cast(.InfixExpression).?;

                try writer.writeAll("(");
                if (infixExpr.leftExprPtr) |leftExpr| {
                    try leftExpr.toString(writer);
                }

                try writer.print(" {s} ", .{infixExpr.operator.toString()});

                if (infixExpr.rightExprPtr) |rightExpr| {
                    try rightExpr.toString(writer);
                }
                try writer.writeAll(")");
            },
            .Boolean => {
                const booleanExpr = node.cast(.Boolean).?;

                try writer.print("{any}", .{booleanExpr.value});
            },
            .IfExpression => {
                const ifExprPtr = node.cast(.IfExpression).?;

                try writer.writeAll("if");
                try ifExprPtr.condition.toString(writer);
                try writer.writeAll(" ");
                try (&ifExprPtr.consequence.base).toString(writer);

                if (ifExprPtr.alternative) |altNode| {
                    try writer.writeAll("else ");
                    try (&altNode.base).toString(writer);
                }
            },
            .Block => {
                const blockNode = node.cast(.Block).?;
                for (blockNode.statements.items) |st| {
                    try st.toString(writer);
                }
            },
            .FunctionLiteral => {
                var functionLiteralNode = node.cast(.FunctionLiteral).?;

                try writer.writeAll(functionLiteralNode.token.Literal);
                try writer.writeAll("(");

                for (functionLiteralNode.parameters) |params| {
                    try writer.print("{s}, ", .{params.value});
                }
                try writer.writeAll(") ");

                try (&functionLiteralNode.body.base).toString(writer);
            },
            .CallExpression => {
                var callExpressionNode = node.cast(.CallExpression).?;

                try callExpressionNode.function.toString(writer);
                try writer.writeAll("(");
                for (callExpressionNode.arguments, 0..) |arg, i| {
                    try arg.toString(writer);
                    if (callExpressionNode.arguments.len != i + 1) {
                        try writer.writeAll(", ");
                    }
                }
                try writer.writeAll(")");
            },
        };
    }
};

///////////// AST //////////////
// pub const Tree = struct {
//     statements: std.ArrayList(*Node),

//     pub fn toString(self: *Tree, writer: anytype) !void {
//         for (self.statements.items) |st| {
//             try st.toString(writer);
//         }
//     }

//     pub fn deinit(self: *Tree) void {
//         self.statements.deinit();
//         self.string_ast.deinit();
//     }
// };

test "toString" {
    const testing = std.testing;

    var alloc = testing.allocator;
    var buf = std.ArrayList(u8).init(alloc);
    defer buf.deinit();

    var identPtr = try alloc.create(Node.Identifier);
    defer alloc.destroy(identPtr);

    identPtr.* = Node.Identifier{
        .token = Token{ .Type = .IDENT, .Literal = "anotherVar" },
        .value = "anotherVar",
    };

    var statement = Node.LetStatement{
        .token = Token{ .Type = .LET, .Literal = "let" },
        .name = Node.Identifier{
            .token = Token{ .Type = .IDENT, .Literal = "myVar" },
            .value = "myVar",
        },
        .value = &identPtr.base,
    };

    const node = &statement.base;
    try node.toString(buf.writer());

    try testing.expect(buf.items.len > 0);

    try testing.expectEqualStrings("let myVar = anotherVar;", buf.items);
}

test "Tree Test" {
    const testing = std.testing;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();
    var buf = std.ArrayList(u8).init(allocator);

    var identPtr = try allocator.create(Node.Identifier);
    identPtr.* = Node.Identifier{ .token = Token{ .Type = .IDENT, .Literal = "anotherVar" }, .value = "anotherVar" };

    var letStmtPtr = try allocator.create(Node.LetStatement);

    letStmtPtr.* = Node.LetStatement{
        .token = Token{ .Type = .LET, .Literal = "let" },
        .name = Node.Identifier{
            .token = Token{ .Type = .IDENT, .Literal = "myVar" },
            .value = "myVar",
        },
        .value = &identPtr.base,
    };

    try (&letStmtPtr.base).toString(buf.writer());

    try testing.expectEqualStrings("let myVar = anotherVar;", buf.items);
}
