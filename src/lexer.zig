const token = @import("token.zig");
const Token = token.Token;
const std = @import("std");
const testing = std.testing;

const Error = std.fmt.ParseIntError;

const Lexer = struct {
    input: []const u8,
    position: u32,
    read_position: u32,

    fn new(input: []const u8) Lexer {
        const l = Lexer{
            .input = input,
            .position = 0,
            .read_position = 0,
        };
        return l;
    }

    fn readInt(self: *Lexer) Error!Token {
        self.readChar();

        while (self.input[self.read_position] >= '0' and self.input[self.read_position] <= '9') {
            self.read_position += 1;
        }
        const integer_string = self.input[self.position..self.read_position];
        // std.debug.print("integer_string: {any}\n", .{integer_string});
        const integer = try std.fmt.parseInt(u32, integer_string, 10);
        // std.debug.print("integer: {}\n", .{integer});
        return Token{ .Int = integer };
    }

    fn skipWhitespaces(self: *Lexer) void {
        while (self.input[self.read_position] == ' ' or self.input[self.read_position] == '\n') {
            self.readChar();
        }
    }

    fn getNextToken(self: *Lexer) Error!Token {
        // std.debug.print("ch: {c}\n", .{ch1});
        self.skipWhitespaces();
        const ch = self.input[self.read_position];
        // std.debug.print("ch: {c}\n", .{ch});
        const t = switch (ch) {
            '0'...'9' => return self.readInt(),
            '=' => blk: {
                if (self.input[self.read_position + 1] == '=') {
                    self.readChar();
                    break :blk Token{ .Equals = undefined };
                }
                break :blk Token{ .Assign = undefined };
            },
            ';' => Token{ .Semicolon = undefined },
            '(' => Token{ .LParen = undefined },
            ')' => Token{ .RParen = undefined },
            '{' => Token{ .LBrace = undefined },
            '}' => Token{ .RBrace = undefined },
            ',' => Token{ .Comma = undefined },
            '+' => Token{ .Plus = undefined },
            '-' => Token{ .Minus = undefined },
            '!' => blk: {
                if (self.input[self.read_position + 1] == '=') {
                    self.readChar();
                    break :blk Token{ .NotEquals = undefined };
                }
                break :blk Token{ .Bang = undefined };
            },
            '*' => Token{ .Asterisk = undefined },
            '/' => Token{ .Slash = undefined },
            '<' => Token{ .LessThan = undefined },
            '>' => Token{ .GreaterThan = undefined },
            0 => Token{ .EOF = undefined },
            else => Token{ .Illegal = undefined },
        };
        self.readChar();
        return t;
    }

    fn readChar(self: *Lexer) void {
        if (self.read_position >= self.input.len) {} else {
            self.position = self.read_position;
            self.read_position += 1;
        }
    }
};

test "operators" {
    const input =
        \\=+-!*/<>
        \\10 == 10;
        \\9 != 10;
    ;

    const expected_tokens = [_]Token{ Token{ .Assign = undefined }, Token{ .Plus = undefined }, Token{ .Minus = undefined }, Token{ .Bang = undefined }, Token{ .Asterisk = undefined }, Token{ .Slash = undefined }, Token{ .LessThan = undefined }, Token{ .GreaterThan = undefined }, Token{ .Int = 10 }, Token{ .Equals = undefined }, Token{ .Int = 10 }, Token{ .Semicolon = undefined }, Token{ .Int = 9 }, Token{ .NotEquals = undefined }, Token{ .Int = 10 }, Token{ .Semicolon = undefined } };

    var lexer = Lexer.new(input);

    for (expected_tokens) |expected_token| {
        const next_token = try lexer.getNextToken();
        // std.debug.print("{}\n", .{next_token});
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
