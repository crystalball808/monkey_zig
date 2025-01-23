const token = @import("token.zig");
const Token = token.Token;
const std = @import("std");
const testing = std.testing;

const Lexer = struct {
    input: []const u8,
    position: u32,
    read_position: u32,
    // TODO: I don't like this property. Can it be computable?
    ch: u8,

    fn new(input: []const u8) Lexer {
        var l = Lexer{
            .input = input,
            .position = 0,
            .read_position = 0,
            .ch = undefined,
        };
        l.readChar();
        return l;
    }

    fn getNextToken(self: *Lexer) Token {
        const t = switch (self.ch) {
            '=' => Token{ .Assign = undefined },
            ';' => Token{ .Semicolon = undefined },
            '(' => Token{ .LParen = undefined },
            ')' => Token{ .RParen = undefined },
            '{' => Token{ .LBrace = undefined },
            '}' => Token{ .RBrace = undefined },
            ',' => Token{ .Comma = undefined },
            '+' => Token{ .Plus = undefined },
            0 => Token{ .EOF = undefined },
            else => Token{ .Illegal = undefined },
        };
        self.readChar();
        return t;
    }

    fn readChar(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
            self.position = self.read_position;
            self.read_position += 1;
        }
    }
};

test "lex the input" {
    const input = "=+(){}";

    const expected_tokens = [_]Token{
        Token{ .Assign = undefined },
        Token{ .Plus = undefined },
        Token{ .LParen = undefined },
        Token{ .RParen = undefined },
        Token{ .LBrace = undefined },
        Token{ .RBrace = undefined },
    };

    var lexer = Lexer.new(input);

    for (expected_tokens) |expected_token| {
        const next_token = lexer.getNextToken();
        try std.testing.expect(std.meta.activeTag(expected_token) == std.meta.activeTag(next_token));

        switch (expected_token) {
            .Identifier => |expected_identifier| {
                const identifier = next_token.Identifier;

                try testing.expect(std.mem.eql(u8, expected_identifier, identifier));
            },
            .Int => |expected_number| {
                const number = next_token.Int;

                try testing.expectEqual(expected_number, number);
            },
            else => {},
        }
    }
}
