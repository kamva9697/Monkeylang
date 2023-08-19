const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const TokenType = token.TokenType;
const mem = std.mem;
const testing = std.testing;

pub const ParserError = enum([]const u8) { UnexpectedToken };

pub const Parser = struct {
    lex: lexer.Lexer,
    curToken: token.Token = undefined,
    peekToken: token.Token = undefined,
    errors: std.ArrayList([:0]const u8) = undefined,
    gpa: std.mem.Allocator,

    pub fn init(l: lexer.Lexer, allocator: std.mem.Allocator) Parser {
        var p = Parser{ .lex = l, .gpa = allocator };
        p.errors = std.ArrayList([:0]const u8).init(p.gpa);

        p.nextToken();
        p.nextToken();

        return p;
    }

    pub fn nextToken(self: *Parser) void {
        self.curToken = self.peekToken;
        self.peekToken = self.lex.nextToken();
    }

    pub fn parseProgram(self: *Parser, allocator: std.mem.Allocator) ast.Program {
        // Program is an AST TREE
        var program = ast.Program.init(allocator);

        while (self.curToken.Type != .EOF) {
            if (self.parseStatement()) |statement| {
                program.statements.append(statement) catch unreachable;
                self.nextToken();
            } else {
                break;
            }
        }

        return program;
    }

    pub fn parseStatement(self: *Parser) ?ast.Node {
        return switch (self.curToken.Type) {
            // TODO: An error should be raised if `parseLetStatement`
            // returns a null, for then, it means a malformed
            // let statement
            .LET => self.parseLetStatement(),
            .RETURN => self.parseReturnStatement(),
            else => null,
        };
    }

    pub fn parseLetStatement(self: *Parser) ?ast.Node {
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

        const stmt = ast.Node{
            .statement = .{ .letStatement = letStmt },
        };
        return stmt;
    }

    pub fn parseReturnStatement(self: *Parser) ?ast.Node {
        var returnStmt = ast.ReturnStatement{ .token = self.curToken, .returnValue = undefined };

        // TODO: Evaluate Expressions, For now,
        // We're skipping the expressions until we
        // encounter a semicolon
        while (!self.curTokenIs(.SEMICOLON)) : (self.nextToken()) {}

        var stmt = ast.Node{
            .statement = .{ .returnStatement = returnStmt },
        };
        return stmt;
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
        var buf: [512:0]u8 = undefined;
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        var msg = std.fmt.bufPrint(&buf, "Expected next token to be {any} got '{any}' instead", .{ tok, self.peekToken.Type }) catch unreachable;
        var msg_sent = std.mem.concatWithSentinel(gpa.allocator(), u8, &[_][]const u8{msg}, 0) catch unreachable;
        self.errors.append(msg_sent) catch unreachable;
    }

    pub fn deinit(self: *Parser) void {
        self.errors.deinit();
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
