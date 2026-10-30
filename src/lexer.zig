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
        const lexer = Lexer{ .input = source };
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
            '\"' => blk: {
                const literal = self.readString() orelse break :blk .{ .Type = .ILLEGAL, .Literal = token.ILLEGAL };
                break :blk .{ .Type = .STRING, .Literal = literal };
            },
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
            'a'...'z', 'A'...'Z', '_' => blk: {
                const literal = self.readIdentifier();
                const _type = token.getKeyword(literal);
                break :blk .{ .Type = _type, .Literal = literal };
            },
            '0'...'9' => blk: {
                const literal = self.readNumber();
                break :blk .{ .Type = .INT, .Literal = literal };
            },
            else => .{ .Type = .ILLEGAL, .Literal = token.ILLEGAL },
        };
    }

    /// Returns null when the input ends before the closing '"'.
    pub inline fn readString(self: *Lexer) ?[]const u8 {
        const position = self.position + 1;

        self.readChar(); // consume opening '"'
        while (self.ch != '\"') {
            if (self.ch == 0) return null;
            self.readChar();
        }

        return self.input[position..self.position];
    }

    pub inline fn readChar(self: *Lexer) void {
        if (self.peekPosition >= self.input.len) {
            self.ch = 0;
        } else {
            self.position = self.peekPosition;
            self.peekPosition += 1;
            self.ch = self.input[self.position];
        }
    }

    // Both readers leave `position` on the token's last character, like readChar does.
    // `input` is 0-terminated, so peeking at `peekPosition` is always in bounds.
    pub inline fn readNumber(self: *Lexer) []const u8 {
        const position = self.position;
        while (ascii.isDigit(self.input[self.peekPosition])) {
            self.readChar();
        }
        return self.input[position .. self.position + 1];
    }

    pub inline fn readIdentifier(self: *Lexer) []const u8 {
        const position = self.position;
        while (isIdentifierChar(self.input[self.peekPosition])) {
            self.readChar();
        }
        return self.input[position .. self.position + 1];
    }

    inline fn isIdentifierChar(c: u8) bool {
        return ascii.isAlphanumeric(c) or c == '_';
    }

    inline fn skipWhiteSpace(self: *Lexer) void {
        if (self.peekPosition > self.input.len) {
            return;
        }

        while (ascii.isWhitespace(self.ch)) {
            self.readChar();
        }
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
    const tok = lex.nextToken();

    try testing.expect(.LET == tok.Type);
    try testing.expectEqualStrings("let", tok.Literal);
}

test "ReadNumber" {
    const input = "523 ";
    var lex = Lexer.init(input);
    lex.readChar();
    const num = lex.readNumber();
    try testing.expectEqualStrings("523", num);
}

test "ReadIndentifier" {
    const input = "five ";
    var lex = Lexer.init(input);
    lex.readChar();
    const ident = lex.readIdentifier();
    try testing.expectEqualStrings("five", ident);
}
