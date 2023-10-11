const std = @import("std");
const ast = @import("ast.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const TokenType = token.TokenType;
const Token = token.Token;
const Node = ast.Node;
const Operator = ast.Operator;
const mem = std.mem;
const testing = std.testing;

// Both a namespace and Class
// Contains Parser State
pub const Parser = struct {
    lex: lexer.Lexer,
    curToken: Token,
    peekToken: Token,
    prefixParseFns: std.AutoHashMapUnmanaged(TokenType, prefixParseFn),
    infixParseFns: std.AutoHashMapUnmanaged(TokenType, infixParseFn),
    errors: std.ArrayList(ParserErrorContext) = undefined,
    gpa: std.mem.Allocator,

    // Class type Declarations
    pub const prefixParseFn = *const fn (*Parser) anyerror!?*Node;
    pub const infixParseFn = *const fn (*Parser, ?*Node) anyerror!?*Node;

    pub const ParserError = error{ NoPrefixParseFn, UnexpectedToken };

    pub const ParserErrorContext = struct {
        err: ParserError,
        msg: []const u8,
    };

    pub const Precedence = enum(u8) {
        LOWEST,
        EQUALS, // ==
        LESSGREATER, // > or <
        SUM, // +
        PRODUCT, // *
        PREFIX, // -x or !x
        CALL, // myfunction(x)

        pub fn fromTokenType(tok: TokenType) @This() {
            return switch (tok) {
                .EQ => .EQUALS,
                .NOT_EQ => .EQUALS,
                .LT => .LESSGREATER,
                .GT => .LESSGREATER,
                .PLUS => .SUM,
                .MINUS => .SUM,
                .SLASH => .PRODUCT,
                .ASTERISK => .PRODUCT,
                .LPAREN => .CALL,
                else => .LOWEST,
            };
        }
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

        p.registerPrefix(TokenType.IDENT, &parseIdentifier);
        p.registerPrefix(TokenType.INT, &parseIntegerLiteral);
        p.registerPrefix(TokenType.BANG, &parsePrefixExpression);
        p.registerPrefix(TokenType.MINUS, &parsePrefixExpression);
        p.registerInfix(TokenType.PLUS, &parseInfixExpression);
        p.registerInfix(TokenType.MINUS, &parseInfixExpression);
        p.registerInfix(TokenType.SLASH, &parseInfixExpression);
        p.registerInfix(TokenType.ASTERISK, &parseInfixExpression);
        p.registerInfix(TokenType.EQ, &parseInfixExpression);
        p.registerInfix(TokenType.NOT_EQ, &parseInfixExpression);
        p.registerInfix(TokenType.LT, &parseInfixExpression);
        p.registerInfix(TokenType.GT, &parseInfixExpression);

        p.nextToken();
        p.nextToken();

        return p;
    }

    pub fn nextToken(self: *Parser) void {
        self.curToken = self.peekToken;
        self.peekToken = self.lex.nextToken();
    }

    pub fn parseProgram(self: *Parser) !ast.Tree {
        var tree = ast.Tree{
            .statements = std.ArrayList(*Node).init(self.gpa),
        };

        while (self.curToken.Type != .EOF) {
            if (try self.parseStatement()) |statement| {
                try tree.statements.append(statement);
                self.nextToken();
            } else {
                break;
            }
        }

        return tree;
    }

    pub fn parseStatement(self: *Parser) !?*Node {
        return switch (self.curToken.Type) {
            // TODO: An error should be raised if `parseLetStatement`
            // returns a null, for then, it means a malformed
            // let statement
            .LET => try self.parseLetStatement(),
            .RETURN => try self.parseReturnStatement(),
            else => try self.parseExpression(.LOWEST),
        };
    }

    pub fn parseExpression(self: *Parser, prec: Precedence) !?*Node {
        var prefix = self.prefixParseFns.get(self.curToken.Type);
        var leftExprNode: ?*Node = null;

        if (prefix) |unwrapped_prefixFn| {
            leftExprNode = try unwrapped_prefixFn(self);
        } else {
            // Add Error Context
            self.noPrefixParseFn(self.curToken.Type);
            return null;
        }

        while (!self.peekTokenIs(TokenType.SEMICOLON) and @intFromEnum(prec) < @intFromEnum(self.peekPrecedence())) {
            var infix = self.infixParseFns.get(self.peekToken.Type);
            if (infix) |unwrapped_infix| {
                self.nextToken();
                leftExprNode = try unwrapped_infix(self, leftExprNode);
            }
        }

        return leftExprNode;
    }

    pub fn parseInfixExpression(self: *Parser, left: ?*Node) !?*Node {
        var exprPtr = try self.gpa.create(Node.InfixExpression);
        exprPtr.* = Node.InfixExpression{
            .token = self.curToken,
            .leftExprPtr = left.?,
            .operator = Operator.fromString(self.curToken.Literal).?,
            .rightExprPtr = null,
        };
        var prec = self.curPrecedence();
        self.nextToken();
        exprPtr.rightExprPtr = try self.parseExpression(prec);
        return &exprPtr.base;
    }

    pub fn parsePrefixExpression(self: *Parser) !?*Node {
        var exprPtr = try self.gpa.create(Node.PrefixExpression);
        exprPtr.* = Node.PrefixExpression{
            .token = self.curToken,
            .operator = Operator.fromString(self.curToken.Literal).?,
            .rightExprPtr = undefined,
        };

        self.nextToken();

        var rightExpr: *Node = try self.gpa.create(Node);

        rightExpr = (try self.parseExpression(.PREFIX)).?;

        exprPtr.rightExprPtr = rightExpr;

        return &exprPtr.base;
    }

    // Parse Identifiers
    pub fn parseIdentifier(self: *Parser) !?*Node {
        var nodePtr = try self.gpa.create(Node.Identifier);

        nodePtr.* = Node.Identifier{ .token = self.curToken, .value = self.curToken.Literal };

        return &nodePtr.base;
    }

    // Parse Literals
    pub fn parseIntegerLiteral(self: *Parser) !?*Node {
        var litPtr = try self.gpa.create(Node.IntegerLiteral);

        litPtr.* = Node.IntegerLiteral{ .token = self.curToken, .value = undefined };

        litPtr.value = std.fmt.parseInt(u32, litPtr.token.Literal, 10) catch |err| {
            std.debug.panic("Error: {any} could not parse {any} as integer", .{ err, self.curToken });
        };

        return &litPtr.base;
    }

    // Parse Statements
    pub fn parseLetStatement(self: *Parser) !?*Node {
        var letStmtPtr = try self.gpa.create(Node.LetStatement);

        letStmtPtr.* = Node.LetStatement{
            .token = self.curToken,
            .value = undefined,
            .name = undefined,
        };

        if (!self.expectPeek(.IDENT)) {
            return null;
        }

        letStmtPtr.name = Node.Identifier{ .token = self.curToken, .value = self.curToken.Literal };

        if (!self.expectPeek(.ASSIGN)) {
            return null;
        }

        // TODO: We're skipping the expressions until we
        // encounter a semicolon
        while (!self.curTokenIs(.SEMICOLON)) : (self.nextToken()) {}

        return &letStmtPtr.base;
    }

    pub fn parseReturnStatement(self: *Parser) !?*Node {
        var returnStmtPtr = try self.gpa.create(Node.ReturnStatement);

        returnStmtPtr.* = Node.ReturnStatement{ .token = self.curToken, .returnValue = null };

        // TODO: Evaluate Expressions, For now,
        // We're skipping the expressions until we
        // encounter a semicolon
        while (!self.curTokenIs(.SEMICOLON)) : (self.nextToken()) {}

        return &returnStmtPtr.base;
    }

    ///////////// Utilities ////////////////////////
    pub fn peekPrecedence(self: *Parser) Precedence {
        return Precedence.fromTokenType(self.peekToken.Type);
    }
    pub fn curPrecedence(self: *Parser) Precedence {
        return Precedence.fromTokenType(self.curToken.Type);
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
        var msg = std.fmt.allocPrint(self.gpa, "Expected next token to be {any} got '{any}' instead", .{ tok, self.peekToken.Type }) catch |err| {
            std.debug.panic("peekError01: Out of Memory {any}", .{err});
        };

        var ctx: ParserErrorContext = .{ .err = ParserError.UnexpectedToken, .msg = msg };
        self.errors.append(ctx) catch |err| {
            std.debug.panic("peekError: Out of Memory: {any}", .{err});
        };
    }

    pub fn registerPrefix(self: *Parser, currentToken: TokenType, prefixFunc: prefixParseFn) void {
        self.prefixParseFns.put(self.gpa, currentToken, prefixFunc) catch |err| {
            std.debug.panic("Error: {any}", .{err});
        };
    }

    pub fn registerInfix(self: *Parser, currentToken: TokenType, infixFunc: infixParseFn) void {
        self.infixParseFns.put(self.gpa, currentToken, infixFunc) catch |err| {
            std.debug.panic("Error: {any}", .{err});
        };
    }

    pub fn noPrefixParseFn(self: *Parser, tok: TokenType) void {
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
        // self.parseProgram().deinit();
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
