const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const TokenType = token.TokenType;
const mem = std.mem;
const testing = std.testing;

// Both a namespace and Class
// Contains Parser State
pub const Parser = struct {
    lex: lexer.Lexer,
    curToken: token.Token,
    peekToken: token.Token,
    prefixParseFns: std.AutoHashMapUnmanaged(token.TokenType, prefixParseFn),
    infixParseFns: std.AutoHashMapUnmanaged(token.TokenType, prefixParseFn),
    errors: std.ArrayList(ParserErrorContext) = undefined,
    gpa: std.mem.Allocator,

    // Class type Declarations
    pub const prefixParseFn = *const fn (*Parser) ?ast.Expression;
    pub const infixParseFn = *const fn (ast.Expression) ?ast.Expression;

    pub const ParserError = error{ NoPrefixParseFn, UnexpectedToken };

    pub const ParserErrorContext = struct {
        err: ParserError,
        msg: []const u8,
    };

    pub const PRECEDENCES = enum(i32) {
        LOWEST = 1,
        EQUALS, // ==
        LESSGREATER, // > or <
        SUM, // +
        PRODUCT, // *
        PREFIX, // -x or !x
        CALL, // myfunction(x)
    };

    //struct methods(Class methods not instance methods)
    pub fn init(l: lexer.Lexer, allocator: std.mem.Allocator) Parser {
        var p = Parser{
            .lex = l,
            .gpa = allocator,
            .prefixParseFns = .{},
            .infixParseFns = .{},
            .curToken = .{ .Type = undefined, .Literal = undefined },
            .peekToken = .{ .Type = undefined, .Literal = undefined },
            .errors = std.ArrayList(ParserErrorContext).init(allocator),
        };

        p.registerPrefix(token.TokenType.IDENT, &parseIdentifier);
        p.registerPrefix(token.TokenType.INT, &parseIntegerLiteral);
        p.registerPrefix(token.TokenType.BANG, &parsePrefixExpression);
        p.registerPrefix(token.TokenType.MINUS, &parsePrefixExpression);

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

    // Parse Expressions
    pub fn parseExpressionStatement(self: *Parser) ?ast.Statement {
        var stmtPtr = self.gpa.create(ast.ExpressionStatement) catch unreachable;

        stmtPtr.* = ast.ExpressionStatement{ .token = self.curToken, .expression = null };

        stmtPtr.*.expression = self.parseExpression(.LOWEST);

        if (self.peekTokenIs(.SEMICOLON)) {
            self.nextToken();
        }

        return ast.Statement{ .expressionStatement = stmtPtr };
    }

    pub fn parseExpression(self: *Parser, prec: PRECEDENCES) ?ast.Expression {
        _ = prec;
        var prefix = self.prefixParseFns.get(self.curToken.Type);
        if (prefix) |unwrapped_prefixFn| {
            var leftExpr = unwrapped_prefixFn(self);
            return leftExpr;
        }
        // Add Error Context
        self.noPrefixParseFn(self.curToken.Type);
        return null;
    }

    pub fn parsePrefixExpression(self: *Parser) ?ast.Expression {
        var exprPtr = self.gpa.create(ast.PrefixExpression) catch |err| {
            std.debug.panic("Parse Expression Error: {any}", .{err});
        };

        exprPtr.* = ast.PrefixExpression{
            .token = self.curToken,
            .operator = self.curToken.Literal,
            .rightExprPtr = undefined,
        };

        self.nextToken();

        //right-hand expression
        var rightExpr: *?ast.Expression = self.gpa.create(?ast.Expression) catch {
            std.debug.panic("Allocator Error", .{});
        };

        rightExpr.* = self.parseExpression(.PREFIX);

        exprPtr.rightExprPtr = rightExpr;

        return ast.Expression{ .prefixExpression = exprPtr };
    }

    // Parse Identifiers
    pub fn parseIdentifier(self: *Parser) ?ast.Expression {
        var nodePtr = self.gpa.create(ast.Identifier) catch |err| {
            std.debug.panic("Parse Expression Error: {any}", .{err});
        };

        nodePtr.* = ast.Identifier{ .token = self.curToken, .value = self.curToken.Literal };

        return ast.Expression{ .identifier = nodePtr };
    }

    // Parse Literals
    pub fn parseIntegerLiteral(self: *Parser) ?ast.Expression {
        var litPtr = self.gpa.create(ast.IntegerLiteral) catch |err| {
            std.debug.panic("Parse Expression Error: {any}", .{err});
        };

        litPtr.* = ast.IntegerLiteral{ .token = self.curToken, .value = undefined };

        litPtr.value = std.fmt.parseInt(u32, litPtr.token.Literal, 10) catch |err| {
            std.debug.panic("Error: {any} could not parse {any} as integer", .{ err, self.curToken });
        };

        return ast.Expression{ .integerLiteral = litPtr };
    }

    // Parse Statements
    pub fn parseLetStatement(self: *Parser) ?ast.Statement {
        var letStmtPtr = self.gpa.create(ast.LetStatement) catch unreachable;

        letStmtPtr.* = ast.LetStatement{
            .token = self.curToken,
            .value = undefined,
            .name = undefined,
        };

        if (!self.expectPeek(.IDENT)) {
            return null;
        }

        letStmtPtr.*.name = ast.Identifier{ .token = self.curToken, .value = self.curToken.Literal };

        if (!self.expectPeek(.ASSIGN)) {
            return null;
        }

        // TODO: We're skipping the expressions until we
        // encounter a semicolon
        while (!self.curTokenIs(.SEMICOLON)) : (self.nextToken()) {}

        return ast.Statement{ .letStatement = letStmtPtr };
    }

    pub fn parseReturnStatement(self: *Parser) ?ast.Statement {
        var returnStmtPtr = self.gpa.create(ast.ReturnStatement) catch unreachable;

        returnStmtPtr.* = ast.ReturnStatement{ .token = self.curToken, .returnValue = undefined };

        // TODO: Evaluate Expressions, For now,
        // We're skipping the expressions until we
        // encounter a semicolon
        while (!self.curTokenIs(.SEMICOLON)) : (self.nextToken()) {}

        return ast.Statement{ .returnStatement = returnStmtPtr };
    }

    ///////////// Utilities ////////////////////////
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
        var msg = std.fmt.allocPrint(self.gpa, "Expected next token to be {any} got '{any}' instead", .{ tok, self.peekToken.Type }) catch |err| {
            std.debug.panic("peekError01: Out of Memory {any}", .{err});
        };

        var ctx: ParserErrorContext = .{ .err = ParserError.UnexpectedToken, .msg = msg };
        self.errors.append(ctx) catch |err| {
            std.debug.panic("peekError: Out of Memory: {any}", .{err});
        };
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

    pub fn noPrefixParseFn(self: *Parser, tok: token.TokenType) void {
        var errCtx = ParserErrorContext{ .err = ParserError.NoPrefixParseFn, .msg = undefined };

        var msg = std.fmt.allocPrint(self.gpa, "Error: NO Prefix function found for {any}", .{tok}) catch |err| {
            std.debug.panic("{any}", .{err});
        };

        errCtx.msg = msg;
        self.errors.append(errCtx) catch {
            std.debug.panic("Errors: Out of M", .{});
        };
    }

    // Destructor
    pub fn deinit(self: *Parser) void {
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
