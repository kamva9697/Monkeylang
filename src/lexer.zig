const std = @import("std");
const token = @import("token.zig");
const Token = token.Token;
const mem = std.mem;
const ascii = std.ascii;
const testing = std.testing;
const debug = std.debug;
const log = std.log;

pub const Lexer = struct {
    input: [:0]const u8,
    position: usize = 0, // current position in input (points to current char)
    peekPosition: usize = 0, // current reading position (after current char)
    ch: u8 = 0, // current char under examiniation

    const Self = @This();

    pub fn init(source: [:0]const u8) Self {
        var lexer = Lexer{ .input = source };
        return lexer;
    }

    pub fn nextToken(self: *Lexer) Token {
        self.readChar();

        self.skipWhiteSpace();

        return switch (self.ch) {
            0 => .{ .Type = .EOF, .Literal = token.EOF },
            '/' => .{ .Type = .SLASH, .Literal = token.SLASH },
            ';' => .{ .Type = .SEMICOLON, .Literal = token.SEMICOLON },
            '(' => .{ .Type = .LPAREN, .Literal = token.LPAREN },
            ')' => .{ .Type = .RPAREN, .Literal = token.RPAREN },
            ',' => .{ .Type = .COMMA, .Literal = token.COMMA },
            '+' => .{ .Type = .PLUS, .Literal = token.PLUS },
            '{' => .{ .Type = .LBRACE, .Literal = token.LBRACE },
            '}' => .{ .Type = .RBRACE, .Literal = token.RBRACE },
            '-' => .{ .Type = .MINUS, .Literal = token.MINUS },
            '*' => .{ .Type = .ASTERISK, .Literal = token.ASTERISK },
            '<' => .{ .Type = .LT, .Literal = token.LT },
            '>' => .{ .Type = .GT, .Literal = token.GT },
            '|' => .{ .Type = .OR, .Literal = token.OR },
            '&' => .{ .Type = .AND, .Literal = token.AND },
            '^' => .{ .Type = .XOR, .Literal = token.XOR },
            '=' => blk: {
                if (self.input[self.peekPosition] == '=') {
                    self.readChar();
                    break :blk .{ .Type = .EQ, .Literal = token.EQ };
                }
                break :blk .{ .Type = .ASSIGN, .Literal = token.ASSIGN };
            },
            '!' => blk: {
                if (self.input[self.peekPosition] == '=') {
                    self.readChar();
                    break :blk .{ .Type = .NOT_EQ, .Literal = token.NOT_EQ };
                }
                break :blk .{ .Type = .BANG, .Literal = token.BANG };
            },
            'a'...'z', 'A'...'Z' => |_| blk: {
                const literal = self.readIdentifier();
                const _type = token.getKeyword(literal);
                break :blk .{ .Type = _type, .Literal = literal };
            },
            '0'...'9' => |_| blk: {
                const literal = self.readNumber();
                break :blk .{ .Type = .INT, .Literal = literal };
            },
            else => .{ .Type = .ILLEGAL, .Literal = token.ILLEGAL },
        };
    }

    pub fn readChar(self: *Lexer) void {
        if (self.peekPosition >= self.input.len) {
            self.ch = 0;
        } else {
            self.position = self.peekPosition;
            self.peekPosition += 1;
            self.ch = self.input[self.position];
        }
    }

    pub fn readNumber(self: *Lexer) []const u8 {
        const curPos = self.position;

        var iter = mem.tokenize(u8, self.input[curPos..], " -=+,./{[]\\();:}*|>");
        const ident = iter.next() orelse unreachable;

        advancePointers(self, ident.len);

        return ident;
    }

    pub inline fn readIdentifier(self: *Lexer) []const u8 {
        const position = self.position;

        var iter = mem.tokenize(u8, self.input[position..], " ,|./{[]\\();:}&^");
        const ident = iter.next() orelse unreachable;

        advancePointers(self, ident.len);

        return ident;
    }

    inline fn skipWhiteSpace(self: *Lexer) void {
        if (self.peekPosition > self.input.len) {
            return;
        }

        while (ascii.isWhitespace(self.ch)) {
            self.readChar();
        }
    }

    inline fn advancePointers(self: *Lexer, readObjectLen: usize) void {
        self.position = self.position + readObjectLen - 1;
        self.peekPosition = self.position + 1;
        self.ch = self.input[self.position];
    }
};

///////////////////////////////////////////////
// Unit Tests                               //
//////////////////////////////////////////////

test "ReadChar" {
    const input = "let five = 5;";
    var lex = Lexer.init(input);
    lex.readChar();
    try testing.expect('l' == lex.ch);
}

test "NextToken" {
    const input = "let five = 5;";
    var lex = Lexer.init(input);
    var tok = lex.nextToken();

    try testing.expect(.LET == tok.Type);
    try testing.expectEqualStrings("let", tok.Literal);
}

test "ReadNumber" {
    const input = "523 ";
    var lex = Lexer.init(input);
    lex.readChar();
    var num = lex.readNumber();
    try testing.expectEqualStrings("523", num);
}

test "ReadIndentifier" {
    const input = "five ";
    var lex = Lexer.init(input);
    lex.readChar();
    var ident = lex.readIdentifier();
    try testing.expectEqualStrings("five", ident);
}
