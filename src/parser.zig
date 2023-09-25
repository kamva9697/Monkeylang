const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const TokenType = token.TokenType;
const mem = std.mem;
const testing = std.testing;

pub const prefixParseFn = *const fn (*Parser) ?ast.Expression;
pub const infixParseFn = *const fn (ast.Expression) ?ast.Expression;

pub const PRECEDENCES = enum(i32) {
    LOWEST = 1,
    EQUALS, // ==
    LESSGREATER, // > or <
    SUM, // +
    PRODUCT, // *
    PREFIX, // -x or !x
    CALL, // myfunction(x)
};

pub const Parser = struct {
    lex: lexer.Lexer,
    curToken: token.Token,
    peekToken: token.Token,
    prefixParseFns: std.AutoHashMapUnmanaged(token.TokenType, prefixParseFn),
    infixParseFns: std.AutoHashMapUnmanaged(token.TokenType, prefixParseFn),
    errors: std.ArrayList([:0]const u8) = undefined,
    gpa: std.mem.Allocator,

    pub fn init(l: lexer.Lexer, allocator: std.mem.Allocator) Parser {
        var p = Parser{
            .lex = l,
            .gpa = allocator,
            .prefixParseFns = .{},
            .infixParseFns = .{},
            .curToken = .{ .Type = undefined, .Literal = undefined },
            .peekToken = .{ .Type = undefined, .Literal = undefined },
        };

        p.errors = std.ArrayList([:0]const u8).init(p.gpa);

        p.registerPrefix(token.TokenType.IDENT, &parseIdentifier);
        p.registerPrefix(token.TokenType.INT, &parseIntegerLiteral);

        p.nextToken();
        p.nextToken();

        return p;
    }

    pub fn nextToken(self: *Parser) void {
        self.curToken = self.peekToken;
        self.peekToken = self.lex.nextToken();
    }

    pub fn parseProgram(self: *Parser) ast.Program {
        var program = ast.Program.init(self.gpa);

        while (self.curToken.Type != .EOF) {
            if (self.parseStatement()) |statement| {
                var node = ast.Node{ .statement = statement };
                program.statements.append(node) catch |err| {
                    std.debug.panic("An Error occured: {any}", .{err});
                };
                self.nextToken();
            } else {
                break;
            }
        }

        return program;
    }

    pub fn parseStatement(self: *Parser) ?ast.Statement {
        return switch (self.curToken.Type) {
            // TODO: An error should be raised if `parseLetStatement`
            // returns a null, for then, it means a malformed
            // let statement
            .LET => self.parseLetStatement(),
            .RETURN => self.parseReturnStatement(),
            else => self.parseExpressionStatement(),
        };
    }

    pub fn parseExpressionStatement(self: *Parser) ?ast.Statement {
        var stmt = ast.ExpressionStatement{ .token = self.curToken, .expression = null };
        stmt.expression = self.parseExpression(.LOWEST);

        if (self.peekTokenIs(.SEMICOLON)) {
            self.nextToken();
        }

        return ast.Statement{ .expressionStatement = stmt };
    }

    pub fn parseExpression(self: *Parser, prec: PRECEDENCES) ?ast.Expression {
        _ = prec;
        var prefix = self.prefixParseFns.get(self.curToken.Type);
        if (prefix) |unwrapped_prefix| {
            var leftExpr = unwrapped_prefix(self);
            return leftExpr;
        }
        return null;
    }

    pub fn parseIdentifier(self: *Parser) ?ast.Expression {
        var node = ast.Identifier{ .token = self.curToken, .value = self.curToken.Literal };

        return ast.Expression{ .identifier = node };
    }

    pub fn parseIntegerLiteral(self: *Parser) ?ast.Expression {
        var lit = ast.IntegerLiteral{ .token = self.curToken, .value = undefined };

        lit.value = std.fmt.parseInt(u32, lit.token.Literal, 10) catch |err| {
            std.debug.panic("Error: {any} could not parse {any} as integer", .{ err, self.curToken });
        };

        return ast.Expression{ .integerLiteral = lit };
    }

    pub fn parseLetStatement(self: *Parser) ?ast.Statement {
        var letStmt = ast.LetStatement{
            .token = self.curToken,
            .value = undefined,
            .name = undefined,
        };

        if (!self.expectPeek(.IDENT)) {
            return null;
        }

        letStmt.name = ast.Identifier{ .token = self.curToken, .value = self.curToken.Literal };

        if (!self.expectPeek(.ASSIGN)) {
            return null;
        }

        // TODO: We're skipping the expressions until we
        // encounter a semicolon
        while (!self.curTokenIs(.SEMICOLON)) : (self.nextToken()) {}

        return ast.Statement{ .letStatement = letStmt };
    }

    pub fn parseReturnStatement(self: *Parser) ?ast.Statement {
        var returnStmt = ast.ReturnStatement{ .token = self.curToken, .returnValue = undefined };

        // TODO: Evaluate Expressions, For now,
        // We're skipping the expressions until we
        // encounter a semicolon
        while (!self.curTokenIs(.SEMICOLON)) : (self.nextToken()) {}

        return ast.Statement{ .returnStatement = returnStmt };
    }

    pub inline fn curTokenIs(self: *Parser, tok: TokenType) bool {
        return self.curToken.Type == tok;
    }

    pub inline fn peekTokenIs(self: *Parser, tok: TokenType) bool {
        return self.peekToken.Type == tok;
    }

    pub inline fn expectPeek(self: *Parser, tok: TokenType) bool {
        if (self.peekTokenIs(tok)) {
            self.nextToken();
            return true;
        }
        self.peekError(tok);
        return false;
    }

    pub inline fn peekError(self: *Parser, tok: TokenType) void {
        var buf: [1024:0]u8 = undefined;

        var msg = std.fmt.bufPrint(&buf, "Expected next token to be {any} got '{any}' instead", .{ tok, self.peekToken.Type }) catch unreachable;

        var msg_sent = std.mem.concatWithSentinel(self.gpa, u8, &[_][]const u8{msg}, 0) catch unreachable;

        self.errors.append(msg_sent) catch unreachable;
    }

    pub fn registerPrefix(self: *Parser, currentToken: token.TokenType, prefixFunc: prefixParseFn) void {
        self.prefixParseFns.put(self.gpa, currentToken, prefixFunc) catch |err| {
            std.debug.panic("Error: {any}", .{err});
        };
    }

    pub fn registerInfix(self: *Parser, currentToken: token.Token, infixFunc: infixParseFn) void {
        self.infixParseFns.put(self.gpa, currentToken, infixFunc) catch |err| {
            std.debug.panic("Error: {any}", .{err});
        };
    }

    pub fn deinit(self: *Parser) void {
        @setCold(true);
        self.errors.deinit();
        self.prefixParseFns.deinit(self.gpa);
        self.infixParseFns.deinit(self.gpa);
        self.parseProgram().deinit();
    }
};

test "initParser" {
    const input = "let five = 5;";

    var lex = lexer.Lexer.init(input);

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var parser = Parser.init(lex, gpa.allocator());

    try testing.expect(parser.curToken.Type != undefined);
    try testing.expect(parser.peekToken.Type != undefined);
}
