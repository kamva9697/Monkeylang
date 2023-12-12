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
pub const TRUE_VALUE = Object.Boolean{ .value = true };
pub const FALSE_VALUE = Object.Boolean{ .value = false };
pub const NULL_VALUE = Object.Null{};

// Todo: Block, FunctionLiteral,
pub fn eval(alloc: Allocator, node: *Ast.Node) anyerror!?*Object {
    return switch (node.id) {
        .Tree => {
            const rootNode = node.cast(.Tree).?;
            return try evalProgram(alloc, rootNode.statements.items);
        },
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
        .Block => {
            const blockNode = node.cast(.Block).?;
            return evalBlock(
                alloc,
                blockNode,
            );
        },
        .IfExpression => {
            const ifNode = node.cast(.IfExpression).?;
            return try evalIfExpression(alloc, ifNode);
        },
        .ReturnStatement => {
            const returnStatement = node.cast(.ReturnStatement).?;
            const rvNode = (try eval(alloc, returnStatement.returnValue.?)).?;
            var obj = try createObject(
                Object.ReturnValue,
                alloc,
                Object.ReturnValue{ .value = rvNode },
            );
            return &obj.base;
        },
        else => {
            var obj = try createObject(Object.Null, alloc, NULL_VALUE);
            return &obj.base;
        },
    };
}

pub fn newError(alloc: Allocator, comptime format: []const u8, args: anytype) !*Object {
    var errorObj = try alloc.create(Object.Error);
    const msg = try std.fmt.allocPrint(alloc, format, args);
    errorObj.* = Object.Error{
        .message = msg,
    };
    return &errorObj.base;
}

pub fn evalProgram(alloc: Allocator, tree: []*Node) !?*Object {
    var result: ?*Object = null;

    for (tree) |stmt| {
        result = (try eval(alloc, stmt)).?;

        if (result.?.ty == .ReturnValue) {
            const returnValue = result.?.cast(.ReturnValue).?;
            return returnValue.value;
        }
        if (result.?.ty == .Error) {
            return result.?;
        }
    }
    return result;
}

pub fn evalBlock(alloc: Allocator, block: *Node.Block) !?*Object {
    var result: ?*Object = null;

    for (block.statements.items) |stmt| {
        result = (try eval(alloc, stmt)).?;

        if (result.?.ty == .ReturnValue or result.?.ty == .Error) {
            return result.?;
        }
    }
    return result;
}

pub fn evalIfExpression(alloc: Allocator, ifNode: *Node.IfExpression) !?*Object {
    const condition = (try eval(alloc, ifNode.condition)).?;

    if (isTruthy(condition)) {
        return try eval(alloc, &ifNode.consequence.base);
    } else if (ifNode.alternative) |alternative| {
        return try eval(alloc, &alternative.base);
    } else {
        var obj = try createObject(Object.Null, alloc, NULL_VALUE);
        return &obj.base;
    }
}

pub fn isTruthy(obj: *Object) bool {
    return switch (obj.ty) {
        .Null => false,
        .Boolean => {
            const boolNode = obj.cast(.Boolean).?;
            return if (boolNode.value) true else false;
        },
        else => true,
    };
}

pub fn evalInfixExpression(
    alloc: Allocator,
    op: Ast.Operator,
    right: *Object,
    left: *Object,
) !*Object {
    if (right.ty != left.ty) {
        return newError(
            alloc,
            "Type Mismatch: {s} {s} {s}",
            .{ left.ty.toString(), op.toString(), right.ty.toString() },
        );
    }
    if (right.ty == .Integer and left.ty == .Integer) {
        return try evalIntegerInfixExpressions(
            alloc,
            op,
            right,
            left,
        );
    }
    if (right.ty == .Boolean and left.ty == .Boolean) {
        const rightVal = right.cast(.Boolean).?;
        const leftVal = left.cast(.Boolean).?;
        return switch (op) {
            .equal_to => {
                var obj = try createObject(
                    Object.Boolean,
                    alloc,
                    // This might Never be true
                    if (rightVal.value == leftVal.value) TRUE_VALUE else FALSE_VALUE,
                );
                return &obj.base;
            },
            .not_equal_to => {
                var obj = try createObject(
                    Object.Boolean,
                    alloc,
                    // This might Never be true
                    if (rightVal.value != leftVal.value) TRUE_VALUE else FALSE_VALUE,
                );
                return &obj.base;
            },
            else => newError(
                alloc,
                "Unknown Operator: {s} {s} {s}",
                .{
                    left.ty.toString(),
                    op.toString(),
                    right.ty.toString(),
                },
            ),
        };
    }
    return newError(
        alloc,
        "Unknown Operator: {s} {s} {s}",
        .{
            left.ty.toString(),
            op.toString(),
            right.ty.toString(),
        },
    );
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
        .less_than => {
            var obj = try createObject(
                Object.Boolean,
                alloc,
                if (leftVal < rightVal) TRUE_VALUE else FALSE_VALUE,
            );
            return &obj.base;
        },
        .greater_than => {
            var obj = try createObject(
                Object.Boolean,
                alloc,
                if (leftVal > rightVal) TRUE_VALUE else FALSE_VALUE,
            );
            return &obj.base;
        },
        .equal_to => {
            var obj = try createObject(
                Object.Boolean,
                alloc,
                if (leftVal == rightVal) TRUE_VALUE else FALSE_VALUE,
            );
            return &obj.base;
        },
        .not_equal_to => {
            var obj = try createObject(
                Object.Boolean,
                alloc,
                if (leftVal != rightVal) TRUE_VALUE else FALSE_VALUE,
            );
            return &obj.base;
        },
        else => {
            return newError(
                alloc,
                "Unkown Operator: {s} {s} {s}",
                .{ left.ty.toString(), op.toString(), right.ty.toString() },
            );
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
            return newError(alloc, "Unkown Operator: {s}{s}", .{ op.toString(), right.ty.toString() });
        },
    };
}

pub fn evalMinusOperatorExpressions(
    alloc: Allocator,
    right: *Object,
) !*Object {
    const op: Ast.Operator = .minus;
    if (right.ty != .Integer) {
        return newError(
            alloc,
            "Unknown Operator: {s}{s}",
            .{ op.toString(), right.ty.toString() },
        );
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
