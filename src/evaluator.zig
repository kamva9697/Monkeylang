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
const Environment = @import("environment.zig").Environment;
const Builtins = @import("builtins.zig");

pub const TRUE_VALUE = &Object.Boolean{ .value = true };
pub const FALSE_VALUE = &Object.Boolean{ .value = false };
pub const NULL_VALUE = &Object.Null{};

pub fn eval(
    alloc: Allocator,
    node: *Ast.Node,
    env: *Environment,
) anyerror!?*Object {
    return switch (node.id) {
        .Tree => {
            const rootNode = node.cast(.Tree);
            return try evalProgram(alloc, rootNode.statements.items, env);
        },
        .Identifier => {
            return try evalIdentifier(alloc, node, env);
        },
        .LetStatement => {
            const letStmt = node.cast(.LetStatement);
            const val = (try eval(alloc, letStmt.value.?, env)).?;
            if (isError(val)) {
                return val;
            }
            return try env.set(letStmt.name.value, val);
        },
        .IntegerLiteral => {
            const intNode = node.cast(.IntegerLiteral);
            const obj = try Object.create(
                Object.Integer,
                alloc,
                Object.Integer{
                    .value = intNode.value,
                },
            );
            return obj.toObject();
        },
        .StringLiteral => {
            const strNode = node.cast(.StringLiteral);
            const obj = try Object.create(
                Object.String,
                alloc,
                Object.String{
                    .value = strNode.value,
                },
            );
            return obj.toObject();
        },
        .Boolean => {
            const boolNode = node.cast(.Boolean);
            const obj = if (boolNode.value) @constCast(TRUE_VALUE) else @constCast(FALSE_VALUE);
            return obj.toObject();
        },
        .PrefixExpression => {
            const prefNode = node.cast(.PrefixExpression);

            const right = try eval(alloc, prefNode.rightExprPtr.?, env);
            return try evalPrefixExpressions(alloc, prefNode.operator, right.?);
        },
        .InfixExpression => {
            const infixNode = node.cast(.InfixExpression);

            const left = try eval(alloc, infixNode.leftExprPtr.?, env);
            const right = try eval(alloc, infixNode.rightExprPtr.?, env);
            return try evalInfixExpression(
                alloc,
                infixNode.operator,
                right.?,
                left.?,
            );
        },
        .Block => {
            const blockNode = node.cast(.Block);
            return evalBlock(
                alloc,
                blockNode,
                env,
            );
        },
        .IfExpression => {
            const ifNode = node.cast(.IfExpression);
            return try evalIfExpression(alloc, ifNode, env);
        },
        .FunctionLiteral => {
            const funcNode = node.cast(.FunctionLiteral);
            var obj = try Object.create(
                Object.Function,
                alloc,
                Object.Function{
                    .parameters = funcNode.parameters,
                    .body = funcNode.body,
                    .env = env,
                },
            );
            return obj.toObject();
        },
        .CallExpression => {
            const funcNode = node.cast(.CallExpression);
            const callNode = (try eval(alloc, funcNode.function, env)).?;
            if (isError(callNode)) {
                return callNode;
            }
            const args = try evalExpressions(alloc, funcNode.arguments, env);
            if (args.len == 1 and isError(args[0])) {
                return args[0];
            }

            return try applyFunction(alloc, callNode, args);
        },
        .ReturnStatement => {
            const returnStatement = node.cast(.ReturnStatement);
            const rvNode = (try eval(alloc, returnStatement.returnValue.?, env)).?;
            var obj = try Object.create(
                Object.ReturnValue,
                alloc,
                Object.ReturnValue{ .value = rvNode },
            );
            return obj.toObject();
        },
    };
}

pub fn applyFunction(alloc: Allocator, fnObj: *Object, args: []*Object) !*Object {
    return switch (fnObj.ty) {
        .Function => {
            const function = fnObj.cast(.Function);
            const extendedEnv = try extendedEnvFunction(alloc, fnObj, args);
            const evaluated = (try eval(alloc, function.body.toNode(), extendedEnv)).?;
            return unwrapReturnValue(evaluated);
        },
        .Builtin => {
            const function = fnObj.cast(.Builtin);
            const resObj = function._fn(alloc, args);
            return resObj;
        },
        else => {
            return newError(alloc, "not a function: {any}", .{fnObj.ty});
        },
    };
}

pub fn extendedEnvFunction(alloc: Allocator, func: *Object, args: []*Object) !*Environment {
    const funcObj = func.cast(.Function);
    var env = try Environment.newEnclosedEnvironment(alloc, funcObj.env);

    for (funcObj.parameters, 0..) |param, idx| {
        _ = try env.set(param.value, args[idx]);
    }

    return env;
}

fn unwrapReturnValue(obj: *Object) *Object {
    const returnValue = obj.cast(.ReturnValue);
    return returnValue.value;
}

pub fn evalExpressions(
    alloc: Allocator,
    exps: []*Ast.Node,
    env: *Environment,
) ![]*Object {
    var result = std.ArrayList(*Object).init(alloc);

    for (exps) |e| {
        const evaled = (try eval(alloc, e, env)).?;
        if (isError(evaled)) {
            try result.append(evaled);
            return result.items;
        }
        try result.append(evaled);
    }
    return result.items;
}

pub fn evalIdentifier(alloc: Allocator, node: *Ast.Node, env: *Environment) !*Object {
    const identNode = node.cast(.Identifier);
    // const val = env.get(identNode.value) orelse {
    //     return Builtins.builtins.get(identNode.value) orelse
    //         newError(alloc, "Identifier not found: {s}", .{identNode.value});
    // };

    const val = env.get(identNode.value) orelse {
        const builtPtr = Builtins.builtins(identNode.value) orelse
            try newError(alloc, "Identifier not found: {s}", .{identNode.value});
        return @constCast(builtPtr);
    };

    return val;
}

pub inline fn isError(obj: *Object) bool {
    return if (obj.ty == .Error) true else false;
}

pub fn newError(alloc: Allocator, comptime format: []const u8, args: anytype) !*Object {
    const msg = try std.fmt.allocPrint(alloc, format, args);
    var errorObj = try Object.create(
        Object.Error,
        alloc,
        Object.Error{
            .message = msg,
        },
    );
    return errorObj.toObject();
}

pub fn evalProgram(alloc: Allocator, tree: []*Node, env: *Environment) !?*Object {
    var result: ?*Object = null;

    for (tree) |stmt| {
        result = (try eval(alloc, stmt, env)).?;

        if (result.?.ty == .ReturnValue) {
            const returnValue = result.?.cast(.ReturnValue);
            return returnValue.value;
        }
        if (result.?.ty == .Error) {
            return result.?;
        }
    }
    return result;
}

pub fn evalBlock(alloc: Allocator, block: *Node.Block, env: *Environment) !?*Object {
    var result: ?*Object = null;

    for (block.statements.items) |stmt| {
        result = (try eval(alloc, stmt, env)).?;

        if (result.?.ty == .ReturnValue or result.?.ty == .Error) {
            return result.?;
        }
    }
    return result;
}

pub fn evalIfExpression(alloc: Allocator, ifNode: *Node.IfExpression, env: *Environment) !?*Object {
    const condition = (try eval(alloc, ifNode.condition, env)).?;

    if (isTruthy(condition)) {
        return try eval(alloc, ifNode.consequence.toNode(), env);
    } else if (ifNode.alternative) |alternative| {
        return try eval(alloc, alternative.toNode(), env);
    } else {
        var obj = @constCast(NULL_VALUE);
        return obj.toObject();
    }
}

pub fn isTruthy(obj: *Object) bool {
    return switch (obj.ty) {
        .Null => false,
        .Boolean => {
            const boolNode = obj.cast(.Boolean);
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
    if (right.ty == .String and left.ty == .String) {
        return try evalStringInfixExpressions(
            alloc,
            op,
            right,
            left,
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
        const rightVal = right.cast(.Boolean);
        const leftVal = left.cast(.Boolean);
        return switch (op) {
            .equal_to => {
                const obj = if (rightVal.value == leftVal.value) @constCast(TRUE_VALUE) else @constCast(FALSE_VALUE);
                return obj.toObject();
            },
            .not_equal_to => {
                const obj = if (rightVal.value != leftVal.value) @constCast(TRUE_VALUE) else @constCast(FALSE_VALUE);
                return obj.toObject();
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

pub fn evalStringInfixExpressions(
    alloc: Allocator,
    op: Ast.Operator,
    right: *Object,
    left: *Object,
) !*Object {
    const leftIntegerObj = left.cast(.String);
    const leftVal = leftIntegerObj.value;
    const rightIntegerObj = right.cast(.String);
    const rightVal = rightIntegerObj.value;

    return switch (op) {
        .plus => {
            const result_string = try std.mem.concat(
                alloc,
                u8,
                &[_][]const u8{ leftVal, rightVal },
            );
            var obj = try Object.create(
                Object.String,
                alloc,
                Object.String{
                    .value = result_string,
                },
            );
            return obj.toObject();
        },
        else => newError(
            alloc,
            "Unknown Operator: {s} {s} {s}",
            .{ left.ty.toString(), op.toString(), right.ty.toString() },
        ),
    };
}

pub fn evalIntegerInfixExpressions(
    alloc: Allocator,
    op: Ast.Operator,
    right: *Object,
    left: *Object,
) !*Object {
    const leftIntegerObj = left.cast(.Integer);
    const leftVal = leftIntegerObj.value;
    const rightIntegerObj = right.cast(.Integer);
    const rightVal = rightIntegerObj.value;

    return switch (op) {
        .plus => {
            var obj = try Object.create(
                Object.Integer,
                alloc,
                Object.Integer{
                    .value = leftVal + rightVal,
                },
            );
            return obj.toObject();
        },
        .minus => {
            var obj = try Object.create(
                Object.Integer,
                alloc,
                Object.Integer{
                    .value = leftVal - rightVal,
                },
            );
            return obj.toObject();
        },
        .multiply => {
            var obj = try Object.create(
                Object.Integer,
                alloc,
                Object.Integer{
                    .value = leftVal * rightVal,
                },
            );
            return obj.toObject();
        },
        .divide => {
            var obj = try Object.create(
                Object.Integer,
                alloc,
                Object.Integer{
                    .value = try Math.divTrunc(i64, leftVal, rightVal),
                },
            );
            return obj.toObject();
        },
        .less_than => {
            const obj = if (leftVal < rightVal) @constCast(TRUE_VALUE) else @constCast(FALSE_VALUE);
            return obj.toObject();
        },
        .greater_than => {
            const obj = if (leftVal > rightVal) @constCast(TRUE_VALUE) else @constCast(FALSE_VALUE);
            return obj.toObject();
        },
        .equal_to => {
            const obj = if (leftVal == rightVal) @constCast(TRUE_VALUE) else @constCast(FALSE_VALUE);
            return obj.toObject();
        },
        .not_equal_to => {
            const obj = if (leftVal != rightVal) @constCast(TRUE_VALUE) else @constCast(FALSE_VALUE);
            return obj.toObject();
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

    const boolNode = right.cast(.Integer);
    const value = boolNode.value;
    var obj = try Object.create(
        Object.Integer,
        alloc,
        Object.Integer{ .value = -value },
    );
    return obj.toObject();
}

pub fn evalBangOperatorExpressions(alloc: Allocator, right: *Object) !*Object {
    _ = alloc;
    return switch (right.ty) {
        .Boolean => {
            const boolObj = right.cast(.Boolean);
            return switch (boolObj.value) {
                true => {
                    var obj = @constCast(FALSE_VALUE);
                    return obj.toObject();
                },
                false => {
                    var obj = @constCast(TRUE_VALUE);
                    return obj.toObject();
                },
            };
        },
        .Null => {
            var obj = @constCast(NULL_VALUE);
            return obj.toObject();
        },
        else => {
            var obj = @constCast(FALSE_VALUE);
            return obj.toObject();
        },
    };
}
