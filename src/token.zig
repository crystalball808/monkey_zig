const debug = @import("std").debug;

pub const Token = union(enum) {
    Illegal,
    EOF,
    // Identifiers + literals
    Identifier: []const u8,
    Int: i32,
    String: []const u8,
    True,
    False,

    // Operators
    Assign,
    Plus,
    Minus,
    Equals,
    NotEquals,
    Bang,
    Asterisk,
    Slash,
    GreaterThan,
    LessThan,

    // Delimiters
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Keywords
    Function,
    Let,
    Return,
    If,
    Else,

    pub fn isInfixOperator(self: *const Token) bool {
        return switch (self.*) {
            .Equals, .NotEquals, .GreaterThan, .LessThan, .Plus, .Minus, .Asterisk, .Slash => true,
            else => false,
        };
    }
    pub fn getPrecedence(self: *const Token) u4 {
        return switch (self.*) {
            .Equals, .NotEquals => 0,
            .GreaterThan, .LessThan => 1,
            .Plus, .Minus => 2,
            .Asterisk, .Slash => 3,
            else => unreachable,
        };
    }
};
pub fn prettyPrint(token: *const Token) void {
    switch (token.*) {
        .Identifier => |ident| {
            debug.print("Identifier: {s}\n", .{ident});
        },
        .String => |string| {
            debug.print("String literal: {s}\n", .{string});
        },
        else => {
            debug.print("{}\n", .{token});
        },
    }
}
