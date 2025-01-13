const token = @import("token.zig");
const Token = token.Token;
const std = @import("std");
const testing = std.testing;

test "Lexer.nextToken" {
    const input: []const u8 = "=+(){}";

    const expected_tokens = [_]Token{
        Token{.Assign}, Token{.Plus}, Token{.LParen}, Token{.RParen}, Token{.LBrace}, Token{.RBrace},
    };

    const lexer = Lexer.new(input);

    for (expected_tokens, 0..) |expected_token, i| {
        const token = lexer.nextToken();
        std.testing.expect(std.meta.activeTag(expected_token) == std.meta.activeTag(token));
        switch (expected_token) {
            .Identifier => |expected_identifier| {
                const identifier = switch (token) {
                    .Identifier => |identifier| {
                        return identifier;
                    },
                    else => unreachable,
                };

                std.testing.expect(std.mem.eql(expected_identifier, identifier));
            },
            .Int => |expected_number| {
                const number = switch (token) {
                    .Int => |number| {
                        return number;
                    },
                    else => unreachable,
                };
                testing.expect(expected_number == number);
            },
            else => {},
        }
    }
}

