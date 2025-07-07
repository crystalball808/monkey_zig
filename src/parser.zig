const std = @import("std");
const Allocator = std.mem.Allocator;
const expect = std.testing.expect;
const print = std.debug.print;

const ast = @import("ast.zig");
const Statement = ast.Statement;
const Expression = ast.Expression;
const lexer_mod = @import("lexer.zig");
const Lexer = lexer_mod.Lexer;
const LexerError = lexer_mod.Error;
const token_mod = @import("token.zig");
const Token = token_mod.Token;

const StatementList = std.ArrayList(Statement);

const Error = error{ UnfinishedStatement, NoTokenToParse, ExpectedSemicolon } || LexerError || Allocator.Error;

const ParseState = enum { start, infix, index };
const Parser = struct {
    lexer: Lexer,
    arena: Allocator,

    fn init(lexer: Lexer, arena: Allocator) Parser {
        return Parser{ .lexer = lexer, .arena = arena };
    }

    fn parseSingleExpression(self: *Parser) !Expression {
        switch (try self.lexer.next() orelse return Error.NoTokenToParse) {
            Token.Int => |integer| return Expression{ .IntLiteral = integer },
            Token.True => return Expression{ .Boolean = true },
            Token.False => return Expression{ .Boolean = false },
            Token.String => |string| return Expression{ .StringLiteral = string },
            Token.Identifier => |ident| return Expression{ .Identifier = ident },
            else => unreachable,
        }
    }
    fn parseExpression(self: *Parser) !Expression {
        const expr = self.parseSingleExpression();

        blk: while (try self.lexer.peek()) |peeked_token| {
            switch (peeked_token) {
                // infix expression
                .Plus, .Minus, .Asterisk, .Slash => unreachable,
                // index expression
                .LBracket => unreachable,
                else => break :blk,
            }
        }

        return expr;
    }

    fn parseStatements(self: *Parser) Error![]Statement {
        var statements = StatementList.init(self.arena);

        while (try self.lexer.peek()) |token| {
            switch (token) {
                .Let => {
                    _ = try self.lexer.next();
                    const ident = blk: {
                        const name = try self.lexer.next() orelse return Error.UnfinishedStatement;
                        switch (name) {
                            .Identifier => |ident| {
                                break :blk ident;
                            },
                            else => return Error.UnfinishedStatement,
                        }
                    };

                    const eq = try self.lexer.next() orelse return Error.UnfinishedStatement;
                    if (eq != .Assign) {
                        return Error.UnfinishedStatement;
                    }

                    const expr = try self.parseExpression();

                    const statement = Statement{ .Let = .{ .expr = expr, .name = ident } };
                    try statements.append(statement);

                    if (try self.lexer.peek()) |peeked_token| {
                        switch (peeked_token) {
                            Token.Semicolon => {
                                _ = try self.lexer.next();
                            },
                            Token.RBrace => {},
                            else => return Error.ExpectedSemicolon,
                        }
                    }
                },
                .Return => {},
                // Encountered the end of block
                .RBrace => {
                    break;
                },
                else => {
                    const expr = try self.parseExpression();
                    try statements.append(Statement{ .Expression = expr });

                    if (try self.lexer.peek()) |peeked_token| {
                        switch (peeked_token) {
                            Token.Semicolon => {
                                _ = try self.lexer.next();
                            },
                            Token.RBrace => {},
                            else => return Error.ExpectedSemicolon,
                        }
                    }
                },
            }
        }

        return statements.items;
    }
};

fn eqlExpressions(a: *const Expression, b: *const Expression) bool {
    if (std.meta.activeTag(a.*) != std.meta.activeTag(b.*)) {
        return false;
    }
    switch (a.*) {
        Expression.IntLiteral => |integer| return (integer == b.*.IntLiteral),
        Expression.Boolean => |value| return value == b.*.Boolean,
        Expression.Identifier => |ident| return std.mem.eql(u8, ident, b.Identifier),
        Expression.StringLiteral => |string| return std.mem.eql(u8, string, b.StringLiteral),
        else => return true,
    }
}
fn testParser(statements: []const Statement, expected_statements: []const Statement) !void {
    expect(statements.len == expected_statements.len) catch |err| {
        print("expected length: {}\ngot: {}\n", .{ expected_statements.len, statements.len });
        return err;
    };

    for (expected_statements, statements) |expected_statement, statement| {
        try expect(std.meta.activeTag(expected_statement) == std.meta.activeTag(statement));

        switch (expected_statement) {
            .Let => |expected_let_statement| {
                const let_statement = statement.Let;

                // compare identifier
                try expect(std.mem.eql(u8, expected_let_statement.name, let_statement.name));

                // compare expression
                try expect(eqlExpressions(&expected_let_statement.expr, &let_statement.expr));
            },
            .Expression => |expected_expression| {
                const expression = statement.Expression;

                try expect(eqlExpressions(&expected_expression, &expression));
            },
            // TODO: Cover all statements
            else => unreachable,
        }
    }

    // for (expected_statements) |expected_token| {
    //     const next_token = (try lexer.getNextToken()).?;
    //
    //     try std.testing.expect(std.meta.activeTag(expected_token) == std.meta.activeTag(next_token));
    //
    //     switch (expected_token) {
    //         .Identifier => |expected_identifier| {
    //             const identifier = next_token.Identifier;
    //
    //             try testing.expect(std.mem.eql(u8, expected_identifier, identifier));
    //         },
    //         .Int => |expected_number| {
    //             const number = next_token.Int;
    //
    //             try testing.expectEqual(expected_number, number);
    //         },
    //         else => {},
    //     }
    // }
}

test "let statement" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();

    const arena = arena_allocator.allocator();

    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = "bazquaz";
    ;
    const lexer = Lexer.new(input);
    var parser = Parser.init(lexer, arena);

    const statements = try parser.parseStatements();
    const expected_statements = [_]Statement{ Statement{ .Let = .{ .name = "x", .expr = Expression{ .IntLiteral = 5 } } }, Statement{ .Let = .{ .name = "y", .expr = Expression{ .IntLiteral = 10 } } }, Statement{ .Let = .{ .name = "foobar", .expr = Expression{ .StringLiteral = "bazquaz" } } } };

    try testParser(statements, &expected_statements);
}
test "expression_statement" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();

    const arena = arena_allocator.allocator();
    const input =
        \\foobar;
        \\5;
    ;

    const lexer = Lexer.new(input);
    var parser = Parser.init(lexer, arena);

    const statements = try parser.parseStatements();
    // const expected_ast = Program::new(vec![
    //     Statement::Expression(Identifier("foobar")),
    //     Statement::Expression(IntLiteral(5)),
    // ]);
    const expected_statements = [_]Statement{ Statement{ .Expression = Expression{ .Identifier = "foobar" } }, Statement{ .Expression = Expression{ .IntLiteral = 5 } } };

    try testParser(statements, &expected_statements);
}
