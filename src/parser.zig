const std = @import("std");
const ast = @import("ast.zig");
const Lexer = @import("lexer.zig").Lexer;
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
    lex: Lexer,
    curToken: Token,
    peekToken: Token,
    prefixParseFns: std.AutoHashMapUnmanaged(TokenType, prefixParseFn),
    infixParseFns: std.AutoHashMapUnmanaged(TokenType, infixParseFn),
    errors: std.ArrayListUnmanaged(ParserErrorContext),
    arena: std.heap.ArenaAllocator,

    // Class type Declarations
    pub const prefixParseFn = *const fn (*Parser) anyerror!?*Node;
    pub const infixParseFn = *const fn (*Parser, ?*Node) anyerror!?*Node;

    pub const ParserError = error{
        NoPrefixParseFn,
        UnexpectedToken,
        ParameterListTooLong,
        InvalidCharacter,
        TypeMismatch,
        UnknownOperator,
    };

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
    pub fn init(input: [:0]const u8, allocator: std.mem.Allocator) Parser {
        var p = Parser{
            .lex = Lexer.init(input),
            .prefixParseFns = .{},
            .infixParseFns = .{},
            .curToken = .{ .Type = undefined, .Literal = undefined },
            .peekToken = .{ .Type = undefined, .Literal = undefined },
            .arena = std.heap.ArenaAllocator.init(allocator),
            .errors = .{},
        };

        p.registerPrefix(TokenType.STRING, &parseStringLiteral);
        p.registerInfix(TokenType.LPAREN, &parseCallExpression);
        p.registerPrefix(TokenType.FUNCTION, &parseFunctionLiteral);
        p.registerPrefix(TokenType.IF, &parseIfExpression);
        p.registerPrefix(TokenType.TRUE, &parseBoolean);
        p.registerPrefix(TokenType.LPAREN, &parseGroupedExpression);
        p.registerPrefix(TokenType.TRUE, &parseBoolean);
        p.registerPrefix(TokenType.FALSE, &parseBoolean);
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

    pub inline fn nextToken(self: *Parser) void {
        self.curToken = self.peekToken;
        self.peekToken = self.lex.nextToken();
    }

    pub fn parseProgram(self: *Parser) !*ast.Node {
        var treePtr = try Node.Tree.init(self.arena.allocator());

        while (!self.curTokenIs(.EOF)) {
            if (try self.parseStatement()) |statement| {
                try treePtr.statements.append(statement);
                self.nextToken();
            } else {
                break;
            }
        }

        return treePtr.toNode();
    }

    pub inline fn parseStatement(self: *Parser) !?*Node {
        return switch (self.curToken.Type) {
            .LET => try self.parseLetStatement(),
            .RETURN => try self.parseReturnStatement(),

            else => try self.parseExpression(.LOWEST),
        };
    }

    pub fn parseExpression(self: *Parser, prec: Precedence) !?*Node {
        var leftExprNode: ?*Node = null;

        const prefixFn = self.prefixParseFns.get(self.curToken.Type);

        if (prefixFn) |unwrapped_prefixFn| {
            leftExprNode = try unwrapped_prefixFn(self);
        } else {
            // Add Error Context
            try self.noPrefixParseFnError(self.curToken.Type);
            return null;
        }

        while (!self.peekTokenIs(TokenType.SEMICOLON) and @intFromEnum(prec) < @intFromEnum(self.peekPrecedence())) {
            const infix = self.infixParseFns.get(self.peekToken.Type);
            if (infix) |unwrapped_infix| {
                self.nextToken();
                leftExprNode = try unwrapped_infix(self, leftExprNode);
            }
        }

        // consume semicolon after statement
        if (self.peekTokenIs(.SEMICOLON)) self.nextToken();

        return leftExprNode;
    }

    pub fn parseInfixExpression(self: *Parser, left: ?*Node) !?*Node {
        var exprNode = try self.arena.allocator().create(Node.InfixExpression);
        exprNode.* = Node.InfixExpression{
            .token = self.curToken,
            .leftExprPtr = left.?,
            .operator = Operator.fromString(self.curToken.Literal).?,
            .rightExprPtr = null,
        };
        const prec = self.curPrecedence();
        self.nextToken();

        exprNode.rightExprPtr = try self.parseExpression(prec);
        return exprNode.toNode();
    }

    pub fn parsePrefixExpression(self: *Parser) !?*Node {
        var exprPtr = try self.arena.allocator().create(Node.PrefixExpression);
        exprPtr.* = Node.PrefixExpression{
            .token = self.curToken,
            .operator = Operator.fromString(self.curToken.Literal).?,
            .rightExprPtr = undefined,
        };

        self.nextToken();

        var rightExpr: *Node = try self.arena.allocator().create(Node);

        rightExpr = (try self.parseExpression(.PREFIX)).?;

        exprPtr.rightExprPtr = rightExpr;

        return exprPtr.toNode();
    }

    // Parse Identifiers
    pub fn parseIdentifier(self: *Parser) !?*Node {
        var nodePtr = try self.arena.allocator().create(Node.Identifier);

        nodePtr.* = Node.Identifier{ .token = self.curToken, .value = self.curToken.Literal };

        return nodePtr.toNode();
    }

    pub fn parseGroupedExpression(self: *Parser) !?*Node {
        self.nextToken();

        const exprPtr = (try self.parseExpression(.LOWEST)).?;

        if (!(try self.expectPeek(TokenType.RPAREN))) {
            return null;
        }

        return exprPtr;
    }

    pub fn parseStringLiteral(self: *Parser) !?*Node {
        const lit = try self.arena.allocator().create(Node.StringLiteral);
        lit.* = Node.StringLiteral{ .token = self.curToken, .value = self.curToken.Literal };
        return lit.toNode();
    }

    // Parse Function Literals
    pub fn parseFunctionLiteral(self: *Parser) !?*Node {
        var lit = try self.arena.allocator().create(Node.FunctionLiteral);

        lit.* = Node.FunctionLiteral{
            .token = self.curToken,
            .parameters = undefined,
            .body = undefined,
        };

        if (!(try self.expectPeek(.LPAREN))) {
            return null;
        }

        lit.parameters = (try self.parseFunctionParameters()).?;

        if (!(try self.expectPeek(.LBRACE))) {
            return null;
        }

        var bodyNode = (try self.parseBlockStatement()).?;

        lit.body = bodyNode.cast(.Block);

        return lit.toNode();
    }

    pub fn parseFunctionParameters(self: *Parser) !?[]*Node.Identifier {
        var identifiers = try std.ArrayList(*Node.Identifier).initCapacity(self.arena.allocator(), 128);

        if (self.peekTokenIs(.RPAREN)) {
            self.nextToken();
            return try identifiers.toOwnedSlice();
        }
        self.nextToken();

        var ident = try self.arena.allocator().create(Node.Identifier);
        ident.* = Node.Identifier{
            .token = self.curToken,
            .value = self.curToken.Literal,
        };

        try identifiers.append(ident);

        while (self.peekTokenIs(.COMMA)) {
            self.nextToken();
            self.nextToken();
            ident = try self.arena.allocator().create(Node.Identifier);
            ident.* = Node.Identifier{
                .token = self.curToken,
                .value = self.curToken.Literal,
            };
            try identifiers.append(ident);
        }

        if (!(try self.expectPeek(.RPAREN))) {
            return null;
        }
        // Assert Parameter limit is 127
        if (identifiers.items.len > 127) {
            const ctx: ParserErrorContext = .{
                .err = ParserError.ParameterListTooLong,
                .msg = "Paramter List Limit is 127",
            };
            try self.errors.append(self.arena.allocator(), ctx);
        }

        return try identifiers.toOwnedSlice();
    }

    pub fn parseIntegerLiteral(self: *Parser) !?*Node {
        var litPtr = try self.arena.allocator().create(Node.IntegerLiteral);

        litPtr.* = Node.IntegerLiteral{ .token = self.curToken, .value = undefined };

        if (!(try self.isDigit(litPtr.token.Literal))) {
            return litPtr.toNode();
        }

        litPtr.value = try std.fmt.parseInt(u32, litPtr.token.Literal, 10);

        return litPtr.toNode();
    }

    pub fn parseIfExpression(self: *Parser) !?*Node {
        var ifExprPtr = try self.arena.allocator().create(Node.IfExpression);

        ifExprPtr.* = Node.IfExpression{
            .token = self.curToken,
            .condition = undefined,
            .consequence = undefined,
            .alternative = null,
        };

        if (!(try self.expectPeek(TokenType.LPAREN))) {
            return null;
        }

        ifExprPtr.condition = (try self.parseExpression(.LOWEST)).?;

        if (!(try self.expectPeek(TokenType.LBRACE))) {
            return null;
        }
        var blockNode = (try self.parseBlockStatement()).?;

        ifExprPtr.consequence = blockNode.cast(.Block);

        if (self.peekTokenIs(TokenType.ELSE)) {
            self.nextToken();

            if (!self.peekTokenIs(TokenType.LBRACE)) {
                return null;
            }

            self.nextToken(); // skip '{'
            var altNode = (try self.parseBlockStatement()).?;
            ifExprPtr.alternative = altNode.cast(.Block);
        }

        return ifExprPtr.toNode();
    }

    pub fn parseCallExpression(self: *Parser, func: ?*Node) !?*Node {
        var expr = try self.arena.allocator().create(Node.CallExpression);
        expr.* = Node.CallExpression{
            .token = self.curToken,
            .function = func.?,
            .arguments = (try self.parseCallArguments()).?,
        };

        return expr.toNode();
    }

    pub fn parseCallArguments(self: *Parser) !?[]*Node {
        var args = std.ArrayList(*Node).init(self.arena.allocator());

        if (self.peekTokenIs(.RPAREN)) {
            self.nextToken();
            return try args.toOwnedSlice();
        }

        self.nextToken();

        try args.append((try self.parseExpression(.LOWEST)).?);

        while (self.peekTokenIs(.COMMA)) {
            self.nextToken();
            self.nextToken();
            try args.append((try self.parseExpression(.LOWEST)).?);
        }

        if (!(try self.expectPeek(.RPAREN))) {
            return null;
        }

        return try args.toOwnedSlice();
    }

    pub fn parseBlockStatement(self: *Parser) !?*Node {
        var blockPtr = try self.arena.allocator().create(Node.Block);

        blockPtr.* = Node.Block{
            .token = self.curToken,
            .statements = .{},
        };

        self.nextToken();

        while (!self.curTokenIs(TokenType.RBRACE) and !self.curTokenIs(TokenType.EOF)) {
            const stmt = try self.parseStatement();
            if (stmt) |st| {
                try blockPtr.statements.append(self.arena.allocator(), st);
            }
            self.nextToken();
        }
        return blockPtr.toNode();
    }

    // Parse Statements
    pub fn parseLetStatement(self: *Parser) !?*Node {
        var letStmtPtr = try self.arena.allocator().create(Node.LetStatement);

        letStmtPtr.* = Node.LetStatement{
            .token = self.curToken,
            .value = undefined,
            .name = undefined,
        };

        if (!(try self.expectPeek(.IDENT))) {
            return null;
        }

        letStmtPtr.name = Node.Identifier{ .token = self.curToken, .value = self.curToken.Literal };

        if (!(try self.expectPeek(.ASSIGN))) {
            return null;
        }

        self.nextToken();

        letStmtPtr.value = (try self.parseExpression(.LOWEST)).?;
        if (self.peekTokenIs(.SEMICOLON)) {
            self.nextToken();
        }

        return letStmtPtr.toNode();
    }

    pub fn parseReturnStatement(self: *Parser) !?*Node {
        var returnStmtPtr = try self.arena.allocator().create(Node.ReturnStatement);

        returnStmtPtr.* = Node.ReturnStatement{ .token = self.curToken, .returnValue = null };

        self.nextToken();

        returnStmtPtr.returnValue = (try self.parseExpression(.LOWEST)).?;

        if (self.peekTokenIs(.SEMICOLON)) {
            self.nextToken();
        }

        return returnStmtPtr.toNode();
    }

    pub fn parseBoolean(self: *Parser) !?*Node {
        var booleanPtr = try self.arena.allocator().create(Node.Boolean);

        const value = self.curTokenIs(.TRUE);

        booleanPtr.* = Node.Boolean{ .token = self.curToken, .value = value };

        return booleanPtr.toNode();
    }

    ///////////// Utilities ////////////////////////
    pub inline fn isDigit(self: *Parser, num: []const u8) !bool {
        for (num) |c| {
            if (!std.ascii.isDigit(c)) {
                const ctx: ParserErrorContext = .{
                    .err = ParserError.InvalidCharacter,
                    .msg = try std.fmt.ParseIntError.allocPrint(
                        self.arena.allocator(),
                        "Invalid Integer Character: {c} in {s}",
                        .{ c, num },
                    ),
                };

                try self.errors.append(self.arena.allocator(), ctx);
                return false;
            }
        }
        return true;
    }
    pub inline fn peekPrecedence(self: *Parser) Precedence {
        return Precedence.fromTokenType(self.peekToken.Type);
    }
    pub inline fn curPrecedence(self: *Parser) Precedence {
        return Precedence.fromTokenType(self.curToken.Type);
    }

    pub inline fn curTokenIs(self: *Parser, tok: TokenType) bool {
        return self.curToken.Type == tok;
    }

    pub inline fn peekTokenIs(self: *Parser, tok: TokenType) bool {
        return self.peekToken.Type == tok;
    }

    pub inline fn expectPeek(self: *Parser, tok: TokenType) !bool {
        if (self.peekTokenIs(tok)) {
            self.nextToken();
            return true;
        }
        try self.peekError(tok);
        return false;
    }

    pub inline fn peekError(self: *Parser, tok: TokenType) !void {
        const msg = try std.fmt.allocPrint(self.arena.allocator(), "Expected next token to be {any} got '{any}' instead", .{ tok, self.peekToken.Type });

        const ctx: ParserErrorContext = .{
            .err = ParserError.UnexpectedToken,
            .msg = msg,
        };
        try self.errors.append(self.arena.allocator(), ctx);
    }

    pub inline fn registerPrefix(self: *Parser, currentToken: TokenType, prefixFunc: prefixParseFn) void {
        self.prefixParseFns.put(self.arena.allocator(), currentToken, prefixFunc) catch |err| {
            std.debug.panic("Error: {any}", .{err});
        };
    }

    pub inline fn registerInfix(self: *Parser, currentToken: TokenType, infixFunc: infixParseFn) void {
        self.infixParseFns.put(self.arena.allocator(), currentToken, infixFunc) catch |err| {
            std.debug.panic("Error: {any}", .{err});
        };
    }

    pub fn noPrefixParseFnError(self: *Parser, tok: TokenType) !void {
        var errCtx = ParserErrorContext{ .err = ParserError.NoPrefixParseFn, .msg = undefined };

        const msg = try std.fmt.allocPrint(self.arena.allocator(), "Parser Error: No Prefix function found for {any}", .{tok});

        errCtx.msg = msg;

        try self.errors.append(self.arena.allocator(), errCtx);
    }

    // Destructor
    pub fn deinit(self: *const Parser) void {
        self.arena.deinit();
        // self.errors.deinit(self.arena.allocator(),);
        // self.prefixParseFns.deinit(self.arena.allocator());
        // self.infixParseFns.deinit(self.arena.allocator());
        // self.parseProgram().deinit();
    }
};

test "initParser" {
    const input = "let five = 5;";

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const parser = Parser.init(input, gpa.allocator());
    defer parser.deinit();
}
