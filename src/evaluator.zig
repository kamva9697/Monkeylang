const std = @import("std");
const Lexer = @import("lexer.zig");
const Parser = @import("parser.zig");
const Ast = @import("ast.zig");
const Node = Ast.Node;
const _Object = @import("object.zig");
const Object = _Object.Object;
const ObjectType = Object.ObjectType;
const Allocator = std.mem.Allocator;
const Math = std.math;

// Interned values
const TRUE_VALUE = Object.Boolean{ .value = true };
const FALSE_VALUE = Object.Boolean{ .value = false };
const NULL_VALUE = Object.Null{};

// Todo: Block, FunctionLiteral,
pub fn eval(alloc: Allocator, node: *Ast.Node) !?*Object {
    return switch (node.id) {
        .IntegerLiteral => {
            const intNode = node.cast(.IntegerLiteral).?;
            var obj = try createObject(
                Object.Integer,
                alloc,
                Object.Integer{
                    .value = intNode.value,
                },
            );
            return &obj.base;
        },
        .Boolean => {
            const boolNode = node.cast(.Boolean).?;
            var obj = try createObject(
                Object.Boolean,
                alloc,
                if (boolNode.value) TRUE_VALUE else FALSE_VALUE,
            );
            return &obj.base;
        },
        .PrefixExpression => {
            const prefNode = node.cast(.PrefixExpression).?;

            const right = try eval(alloc, prefNode.rightExprPtr.?);
            return try evalPrefixExpressions(alloc, prefNode.operator, right.?);
        },
        .InfixExpression => {
            const infixNode = node.cast(.InfixExpression).?;

            const left = try eval(alloc, infixNode.leftExprPtr.?);
            const right = try eval(alloc, infixNode.rightExprPtr.?);
            return try evalInfixExpression(
                alloc,
                infixNode.operator,
                right.?,
                left.?,
            );
        },
        else => {
            var obj = try createObject(Object.Null, alloc, NULL_VALUE);
            return &obj.base;
        },
    };
}

pub fn evalStatements(alloc: Allocator, tree: Ast.Tree) !?*Object {
    var result: ?*Object = null;

    for (tree.statements.items) |stmt| {
        result = (try eval(alloc, stmt)).?;
    }
    return result;
}

pub fn evalInfixExpression(
    alloc: Allocator,
    op: Ast.Operator,
    right: *Object,
    left: *Object,
) !*Object {
    if (right.ty == left.ty) {
        return try evalIntegerInfixExpressions(
            alloc,
            op,
            right,
            left,
        );
    }
    var obj = try createObject(Object.Null, alloc, NULL_VALUE);
    return &obj.base;
}

pub fn evalIntegerInfixExpressions(
    alloc: Allocator,
    op: Ast.Operator,
    right: *Object,
    left: *Object,
) !*Object {
    const leftIntegerObj = left.cast(.Integer).?;
    const leftVal = leftIntegerObj.value;
    const rightIntegerObj = right.cast(.Integer).?;
    const rightVal = rightIntegerObj.value;

    return switch (op) {
        .plus => {
            var obj = try createObject(
                Object.Integer,
                alloc,
                Object.Integer{
                    .value = leftVal + rightVal,
                },
            );
            return &obj.base;
        },
        .minus => {
            var obj = try createObject(
                Object.Integer,
                alloc,
                Object.Integer{
                    .value = leftVal - rightVal,
                },
            );
            return &obj.base;
        },
        .multiply => {
            var obj = try createObject(
                Object.Integer,
                alloc,
                Object.Integer{
                    .value = leftVal * rightVal,
                },
            );
            return &obj.base;
        },
        .divide => {
            var obj = try createObject(
                Object.Integer,
                alloc,
                Object.Integer{
                    .value = try Math.divTrunc(i64, leftVal, rightVal),
                },
            );
            return &obj.base;
        },
        else => {
            var obj = try createObject(Object.Null, alloc, NULL_VALUE);
            return &obj.base;
        },
    };
}

pub fn evalPrefixExpressions(
    alloc: Allocator,
    op: Ast.Operator,
    right: *Object,
) !*Object {
    return switch (op) {
        .not => try evalBangOperatorExpressions(alloc, right),
        .minus => try evalMinusOperatorExpressions(alloc, right),
        else => {
            var obj = try createObject(Object.Null, alloc, NULL_VALUE);
            return &obj.base;
        },
    };
}

pub fn evalMinusOperatorExpressions(
    alloc: Allocator,
    right: *Object,
) !*Object {
    if (right.ty != .Integer) {
        var obj = try createObject(Object.Null, alloc, NULL_VALUE);
        return &obj.base;
    }

    const boolNode = right.cast(.Integer).?;
    const value = boolNode.value;
    var obj = try createObject(
        Object.Integer,
        alloc,
        Object.Integer{ .value = -value },
    );
    return &obj.base;
}

pub fn evalBangOperatorExpressions(alloc: Allocator, right: *Object) !*Object {
    return switch (right.ty) {
        .Boolean => {
            const boolObj = right.cast(.Boolean).?;
            return switch (boolObj.value) {
                true => {
                    var obj = try createObject(Object.Boolean, alloc, FALSE_VALUE);
                    return &obj.base;
                },
                false => {
                    var obj = try createObject(Object.Boolean, alloc, TRUE_VALUE);
                    return &obj.base;
                },
            };
        },
        .Null => {
            var obj = try createObject(Object.Null, alloc, NULL_VALUE);
            return &obj.base;
        },
        else => {
            var obj = try createObject(Object.Boolean, alloc, FALSE_VALUE);
            return &obj.base;
        },
    };
}

fn createObject(comptime T: type, alloc: Allocator, value: T) !*T {
    const obj = try alloc.create(T);
    obj.* = value;
    return obj;
}
