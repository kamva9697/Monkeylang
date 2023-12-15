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

pub const Environment = struct {
    store: std.StringHashMap(
        *Object,
    ),
    outer: ?*Environment = null,

    pub fn newEnclosedEnvironment(alloc: Allocator, outer: *Environment) !*Environment {
        var env = try newEnvironment(alloc);
        env.outer = outer;
        return env;
    }

    pub fn newEnvironment(alloc: Allocator) !*Environment {
        var newEnv = try alloc.create(Environment);
        newEnv.store = std.StringHashMap(*Object).init(alloc);
        return newEnv;
    }

    pub fn get(self: *Environment, name: []const u8) ?*Object {
        return self.store.get(name) orelse {
            if (self.outer) |outer_env| {
                return outer_env.store.get(name);
            }
            return null;
        };
    }

    pub fn set(self: *Environment, name: []const u8, value: *Object) !*Object {
        try self.store.put(name, value);
        return value;
    }
};
