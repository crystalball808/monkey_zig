const std = @import("std");
const testing = std.testing;
pub const Error = std.fmt.ParseIntError;

const token = @import("token.zig");
const Token = token.Token;

const keyword_map = std.StaticStringMap(Token).initComptime(.{
    .{ "let", Token{ .Let = undefined } },
    .{ "fn", Token{ .Function = undefined } },
    .{ "true", Token{ .True = undefined } },
    .{ "false", Token{ .False = undefined } },
    .{ "return", Token{ .Return = undefined } },
    .{ "if", Token{ .If = undefined } },
    .{ "else", Token{ .Else = undefined } },
});
fn lookupKeyword(word: []const u8) Token {
    return keyword_map.get(word) orelse Token{ .Identifier = word };
}

const Lexer = struct {
    input: []const u8,
    position: u32,
    read_position: u32,

    pub fn new(input: []const u8) Lexer {
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
        const integer = try std.fmt.parseInt(u32, integer_string, 10);
        return Token{ .Int = integer };
    }
    fn readWord(self: *Lexer) Error![]const u8 {
        self.readChar();
        while (self.input[self.read_position] >= 'A' and self.input[self.read_position] <= 'z') {
            self.read_position += 1;
        }
        return self.input[self.position..self.read_position];
    }
    fn readString(self: *Lexer) Token {
        self.readChar();
        self.readChar();

        while (self.input[self.read_position] != '"') {
            self.read_position += 1;
        }
        const string = self.input[self.position..self.read_position];
        self.read_position += 1;
        return Token{ .String = string };
    }

    fn skipWhitespaces(self: *Lexer) void {
        while (self.read_position < self.input.len and (self.input[self.read_position] == ' ' or self.input[self.read_position] == '\n')) {
            self.readChar();
        }
    }

    pub fn getNextToken(self: *Lexer) Error!?Token {
        self.skipWhitespaces();
        if (self.read_position >= self.input.len) {
            return null;
        }

        const ch = self.input[self.read_position];
        const t = switch (ch) {
            '"' => return self.readString(),
            '0'...'9' => return try self.readInt(),
            'A'...'Z' => return lookupKeyword(try self.readWord()),
            'a'...'z' => return lookupKeyword(try self.readWord()),
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
            '[' => Token{ .LBracket = undefined },
            ']' => Token{ .RBracket = undefined },
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

fn testLexer(lexer: *Lexer, expected_tokens: []const Token) !void {
    for (expected_tokens) |expected_token| {
        const next_token = (try lexer.getNextToken()).?;

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
    try testing.expect(try lexer.getNextToken() == null);
}

test "operators" {
    const input =
        \\=+-!*/<>
        \\10 == 10;
        \\9 != 10;
    ;

    const expected_tokens = [_]Token{ Token{ .Assign = undefined }, Token{ .Plus = undefined }, Token{ .Minus = undefined }, Token{ .Bang = undefined }, Token{ .Asterisk = undefined }, Token{ .Slash = undefined }, Token{ .LessThan = undefined }, Token{ .GreaterThan = undefined }, Token{ .Int = 10 }, Token{ .Equals = undefined }, Token{ .Int = 10 }, Token{ .Semicolon = undefined }, Token{ .Int = 9 }, Token{ .NotEquals = undefined }, Token{ .Int = 10 }, Token{ .Semicolon = undefined } };

    var lexer = Lexer.new(input);

    try testLexer(&lexer, &expected_tokens);
}

test "basic set" {
    const input =
        \\let five = 5;
        \\let name = "Roman";
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\  x + y
        \\};
        \\let result = add(five, ten);
        \\[1, 2];
    ;

    const expected_tokens = [_]Token{
        Token{ .Let = undefined },
        Token{ .Identifier = "five" },
        Token{ .Assign = undefined },
        Token{ .Int = 5 },
        Token{ .Semicolon = undefined },
        Token{ .Let = undefined },
        Token{ .Identifier = "name" },
        Token{ .Assign = undefined },
        Token{ .String = "Roman" },
        Token{ .Semicolon = undefined },
        Token{ .Let = undefined },
        Token{ .Identifier = "ten" },
        Token{ .Assign = undefined },
        Token{ .Int = 10 },
        Token{ .Semicolon = undefined },
        Token{ .Let = undefined },
        Token{ .Identifier = "add" },
        Token{ .Assign = undefined },
        Token{ .Function = undefined },
        Token{ .LParen = undefined },
        Token{ .Identifier = "x" },
        Token{ .Comma = undefined },
        Token{ .Identifier = "y" },
        Token{ .RParen = undefined },
        Token{ .LBrace = undefined },
        Token{ .Identifier = "x" },
        Token{ .Plus = undefined },
        Token{ .Identifier = "y" },
        Token{ .RBrace = undefined },
        Token{ .Semicolon = undefined },
        Token{ .Let = undefined },
        Token{ .Identifier = "result" },
        Token{ .Assign = undefined },
        Token{ .Identifier = "add" },
        Token{ .LParen = undefined },
        Token{ .Identifier = "five" },
        Token{ .Comma = undefined },
        Token{ .Identifier = "ten" },
        Token{ .RParen = undefined },
        Token{ .Semicolon = undefined },
        Token{ .LBracket = undefined },
        Token{ .Int = 1 },
        Token{ .Comma = undefined },
        Token{ .Int = 2 },
        Token{ .RBracket = undefined },
        Token{ .Semicolon = undefined },
    };

    var lexer = Lexer.new(input);

    try testLexer(&lexer, &expected_tokens);
}
