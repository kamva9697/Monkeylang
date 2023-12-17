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
    pub const Self = @This();
    store: std.StringHashMap(
        *Object,
    ),
    outer: ?*Environment,

    fn init(alloc: Allocator, outer_env: ?*Environment) !*Self {
        const newEnv = try alloc.create(Environment);
        newEnv.* = Environment{
            .store = std.StringHashMap(*Object).init(alloc),
            .outer = outer_env,
        };
        return newEnv;
    }

    pub fn newEnclosedEnvironment(alloc: Allocator, outer: *Environment) !*Environment {
        return init(alloc, outer);
    }

    pub fn newEnvironment(alloc: Allocator) !*Environment {
        return init(alloc, null);
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
