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
    errors: std.ArrayList(ParserErrorContext) = undefined,
    gpa: std.mem.Allocator,

    // Class type Declarations
    pub const prefixParseFn = *const fn (*Parser) anyerror!?*Node;
    pub const infixParseFn = *const fn (*Parser, ?*Node) anyerror!?*Node;

    // TODO: Improve handling of Parser Errors
    pub const ParserError = error{ NoPrefixParseFn, UnexpectedToken, ParameterListTooLong };

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
            .gpa = allocator,
            .prefixParseFns = .{},
            .infixParseFns = .{},
            .curToken = .{ .Type = undefined, .Literal = undefined },
            .peekToken = .{ .Type = undefined, .Literal = undefined },
            .errors = std.ArrayList(ParserErrorContext).init(allocator),
        };

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
        var leftExprNode: ?*Node = null;

        var prefixFn = self.prefixParseFns.get(self.curToken.Type);

        if (prefixFn) |unwrapped_prefixFn| {
            leftExprNode = try unwrapped_prefixFn(self);
        } else {
            // Add Error Context
            try self.noPrefixParseFn(self.curToken.Type);
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

        // right-assoc
        // var associativity = switch (exprPtr.operator) {
        //     .plus => @intFromEnum(prec) - @as(u8, 1),
        //     else => @intFromEnum(prec),
        // };

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

    pub fn parseGroupedExpression(self: *Parser) !?*Node {
        self.nextToken();

        var exprPtr = (try self.parseExpression(.LOWEST)).?;

        if (!(try self.expectPeek(TokenType.RPAREN))) {
            return null;
        }

        return exprPtr;
    }

    // Parse Literals
    pub fn parseFunctionLiteral(self: *Parser) !?*Node {
        var lit = try self.gpa.create(Node.FunctionLiteral);

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

        lit.body = bodyNode.cast(.Block).?;

        return &lit.base;
    }

    pub fn parseFunctionParameters(self: *Parser) !?[]*Node.Identifier {
        var identifiers = try std.ArrayList(*Node.Identifier).initCapacity(self.gpa, 128);

        if (self.peekTokenIs(.RPAREN)) {
            self.nextToken();
            return try identifiers.toOwnedSlice();
        }
        self.nextToken();

        //first Identifier
        var ident = try self.gpa.create(Node.Identifier);
        ident.* = Node.Identifier{
            .token = self.curToken,
            .value = self.curToken.Literal,
        };

        try identifiers.append(ident);

        while (self.peekTokenIs(.COMMA)) {
            self.nextToken();
            self.nextToken();
            ident = try self.gpa.create(Node.Identifier);
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
            var ctx: ParserErrorContext = .{
                .err = ParserError.ParameterListTooLong,
                .msg = "Paramter List Limit is 127",
            };
            try self.errors.append(ctx);
        }

        return try identifiers.toOwnedSlice();
    }

    pub fn parseIntegerLiteral(self: *Parser) !?*Node {
        var litPtr = try self.gpa.create(Node.IntegerLiteral);

        litPtr.* = Node.IntegerLiteral{ .token = self.curToken, .value = undefined };

        litPtr.value = try std.fmt.parseInt(u32, litPtr.token.Literal, 10);

        return &litPtr.base;
    }

    pub fn parseIfExpression(self: *Parser) !?*Node {
        var ifExprPtr = try self.gpa.create(Node.IfExpression);

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

        // Is the RParen part of a grouped expression?
        // if (!(try self.expectPeek(TokenType.RPAREN))) {
        //     return null;
        // }

        if (!(try self.expectPeek(TokenType.LBRACE))) {
            return null;
        }
        var blockNode = (try self.parseBlockStatement()).?;

        ifExprPtr.consequence = blockNode.cast(.Block).?;

        if (self.peekTokenIs(TokenType.ELSE)) {
            self.nextToken();

            if (!self.peekTokenIs(TokenType.RBRACE)) {
                return null;
            }

            var altNode = (try self.parseBlockStatement()).?;
            ifExprPtr.alternative = altNode.cast(.Block).?;
        }

        return &ifExprPtr.base;
    }

    pub fn parseCallExpression(self: *Parser, func: ?*Node) !?*Node {
        var expr = try self.gpa.create(Node.CallExpression);
        expr.* = Node.CallExpression{
            .token = self.curToken,
            .function = func.?,
            .arguments = (try self.parseCallArguments()).?,
        };

        return &expr.base;
    }

    pub fn parseCallArguments(self: *Parser) !?[]*Node {
        var args = std.ArrayList(*Node).init(self.gpa);

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
        var blockPtr = try self.gpa.create(Node.Block);

        blockPtr.* = Node.Block{
            .token = self.curToken,
            .statements = .{},
        };

        self.nextToken();

        while (!self.curTokenIs(TokenType.RBRACE) and !self.curTokenIs(TokenType.EOF)) {
            var stmt = try self.parseStatement();
            if (stmt) |st| {
                try blockPtr.statements.append(self.gpa, st);
            }
            self.nextToken();
        }
        return &blockPtr.base;
    }

    // Parse Statements
    pub fn parseLetStatement(self: *Parser) !?*Node {
        var letStmtPtr = try self.gpa.create(Node.LetStatement);

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

        return &letStmtPtr.base;
    }

    pub fn parseReturnStatement(self: *Parser) !?*Node {
        var returnStmtPtr = try self.gpa.create(Node.ReturnStatement);

        returnStmtPtr.* = Node.ReturnStatement{ .token = self.curToken, .returnValue = null };

        self.nextToken();

        returnStmtPtr.returnValue = (try self.parseExpression(.LOWEST)).?;

        if (self.peekTokenIs(.SEMICOLON)) {
            self.nextToken();
        }

        return &returnStmtPtr.base;
    }

    pub fn parseBoolean(self: *Parser) !?*Node {
        var booleanPtr = try self.gpa.create(Node.Boolean);

        var value = self.curTokenIs(.TRUE);

        booleanPtr.* = Node.Boolean{ .token = self.curToken, .value = value };

        return &booleanPtr.base;
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

    pub inline fn expectPeek(self: *Parser, tok: TokenType) !bool {
        if (self.peekTokenIs(tok)) {
            self.nextToken();
            return true;
        }
        try self.peekError(tok);
        return false;
    }

    pub inline fn peekError(self: *Parser, tok: TokenType) !void {
        var msg = try std.fmt.allocPrint(self.gpa, "Expected next token to be {any} got '{any}' instead", .{ tok, self.peekToken.Type });

        var ctx: ParserErrorContext = .{
            .err = ParserError.UnexpectedToken,
            .msg = msg,
        };
        try self.errors.append(ctx);
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

    pub fn noPrefixParseFn(self: *Parser, tok: TokenType) !void {
        var errCtx = ParserErrorContext{ .err = ParserError.NoPrefixParseFn, .msg = undefined };

        var msg = try std.fmt.allocPrint(self.gpa, "Parser Error: No Prefix function found for {any}", .{tok});

        errCtx.msg = msg;

        try self.errors.append(errCtx);
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

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var parser = Parser.init(input, gpa.allocator());

    try testing.expect(parser.curToken.Type != undefined);
    try testing.expect(parser.peekToken.Type != undefined);
}
