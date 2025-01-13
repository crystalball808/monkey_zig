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
        std.debug.print("[readChar] state: {}\n", .{self});
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

    // const my_insects = [_]Insect{
    //     Insect{ .ant = Ant{ .still_alive = true } },
    //     Insect{ .bee = Bee{ .flowers_visited = 17 } },
    //     Insect{ .grasshopper = Grasshopper{ .distance_hopped = 32 } },
    // };
    const expected_tokens = [_]Token{
        Token{ .Assign = undefined }, Token{ .Plus = undefined }, Token{ .LParen = undefined }, Token{ .RParen = undefined }, Token{ .LBrace = undefined }, Token{ .RBrace = undefined },
    };

    const lexer = Lexer.new(input);

    for (expected_tokens) |expected_token| {
        const next_token = lexer.getNextToken();
        std.debug.print("next_token active tag: {}\n", .{std.meta.activeTag(next_token)});
        std.debug.print("expected_token active tag: {}\n", .{std.meta.activeTag(expected_token)});
        try std.testing.expect(std.meta.activeTag(expected_token) == std.meta.activeTag(next_token));

        switch (expected_token) {
            .Identifier => |expected_identifier| {
                const identifier = next_token.Identifier;
                // const identifier: []const u8 = switch (next_token) {
                //     .Identifier => |identifier| identifier,
                //     else => unreachable,
                // };
                // TODO: or this?
                // const identifier = switch (token) {
                //     inline else => |identifier| {
                //         return identifier;
                //     }
                // };

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
