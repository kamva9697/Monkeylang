// Token package

const std = @import("std");
const Expression = @import("ast.zig").Expression;
const testing = std.testing;
const mem = std.mem;
const debug = std.debug;

pub const TokenType = enum {
    // Operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    EQ,
    NOT_EQ,
    LT,
    GT,

    // BitWise
    AND,
    OR,
    XOR,

    // identifiers and literals
    IDENT,
    INT,
    STRING,

    //Delimiters
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    //Keywords
    FUNCTION,
    LET,
    IF,
    ELSE,
    RETURN,
    TRUE,
    FALSE,

    ILLEGAL,
    EOF,
};

pub const Token = struct {
    Type: TokenType,
    Literal: []const u8,

    pub inline fn isEqual(self: Token, other: TokenType) bool {
        return self.Type == other;
    }
};

pub const keywords = std.ComptimeStringMap(TokenType, .{
    .{ "fn", .FUNCTION },
    .{ "let", .LET },
    .{ "if", .IF },
    .{ "else", .ELSE },
    .{ "return", .RETURN },
    .{ "true", .TRUE },
    .{ "false", .FALSE },
});

pub fn getKeyword(string: []const u8) TokenType {
    return keywords.get(string) orelse .IDENT;
}

test "Token getKeyword" {
    try testing.expectEqual(TokenType.FUNCTION, keywords.get("fn").?);
}

pub const ILLEGAL = "ILLEGAL";
pub const EOF = "";
// identifiers and literals
pub const IDENT = "IDENT";
pub const INT = "INT";

//Operators
pub const ASSIGN = "=";
pub const PLUS = "+";
pub const MINUS = "-";
pub const BANG = "!";
pub const ASTERISK = "*";
pub const SLASH = "/";
pub const EQ = "==";
pub const NOT_EQ = "!=";

pub const LT = "<";
pub const GT = ">";

// Bitwise
pub const AND = "&";
pub const OR = "|";
pub const XOR = "^";

//Delimiters
pub const COMMA = ",";
pub const SEMICOLON = ";";

pub const LPAREN = "(";
pub const RPAREN = ")";
pub const LBRACE = "{";
pub const RBRACE = "}";
pub const STRING = "STRING";
