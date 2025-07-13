const debug = @import("std").debug;

pub const Token = union(enum) {
    Illegal: void,
    EOF: void,
    // Identifiers + literals
    Identifier: []const u8,
    Int: i32,
    String: []const u8,
    True: void,
    False: void,

    // Operators
    Assign: void,
    Plus: void,
    Minus: void,
    Equals: void,
    NotEquals: void,
    Bang: void,
    Asterisk: void,
    Slash: void,
    GreaterThan: void,
    LessThan: void,

    // Delimiters
    Comma: void,
    Semicolon: void,

    LParen: void,
    RParen: void,
    LBrace: void,
    RBrace: void,
    LBracket: void,
    RBracket: void,

    // Keywords
    Function: void,
    Let: void,
    Return: void,
    If: void,
    Else: void,
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
