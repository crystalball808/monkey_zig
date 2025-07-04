const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Statement = ast.Statement;
const Expression = ast.Expression;
const lexer_mod = @import("lexer.zig");
const Lexer = lexer_mod.Lexer;
const LexerError = lexer_mod.Error;
const Token = @import("token.zig").Token;

const StatementList = std.ArrayList(Statement);

const Error = error{UnfinishedStatement} || LexerError;

const Parser = struct {
    lexer: Lexer,
    allocator: Allocator,

    fn init(lexer: Lexer, allocator: Allocator) Parser {
        return Parser{ lexer, allocator };
    }

    /// Returns a pointer to a heap-allocated Expression
    fn parseExpression(self: *Parser) *Expression {}

    // TODO: Should we init the Parser with an allocator???
    fn parseStatements(self: *Parser) Error![]Statement {
        const statements = StatementList.init(self.allocator);

        while (try self.lexer.getNextToken()) |token| {
            switch (token) {
                .Let => {
                    const ident = blk: {
                        const name = try self.lexer.getNextToken() orelse return Error.UnfinishedStatement;
                        switch (name) {
                            .Identifier => |ident| {
                                break :blk ident;
                            },
                            else => return Error.UnfinishedStatement,
                        }
                    };

                    const eq = try self.lexer.getNextToken() orelse return Error.UnfinishedStatement;
                    if (eq != .Assign) {
                        return Error.UnfinishedStatement;
                    }

                    const expr = self.parseExpression();

                    const statement = Statement{ .Let = .{ .expr = expr, .name = ident } };
                    statements.append(statement);
                },
                .Return => {},
                // Encountered the end of block
                .RBrace => {
                    break;
                },
                else => {},
            }
        }

        return statements.items;
    }
};

const expect = std.testing.expect;

fn eqlExpressions(a: *const Expression, b: *const Expression) bool {}
fn testParser(statements: []const Statement, expected_statements: []const Statement) !void {
    try expect(statements.len == expected_statements.len);

    for (expected_statements, statements) |expected_statement, statement| {
        expect(std.meta.activeTag(expected_statement) == std.meta.activeTag(statement));

        switch (expected_statement) {
            .Let => |expected_let_statement| {
                const let_statement = statement.Let;

                // compare identifier
                expect(std.mem.eql(u8, expected_let_statement.name, let_statement.name));

                // compare expression
                expect(eqlExpressions(expected_let_statement.expr, let_statement.expr));
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
    const gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer gpa.deinit();

    const gpa_allocator = gpa.allocator();
    const arena = std.heap.ArenaAllocator.init(gpa_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = "bazquaz";
    ;
    const lexer = Lexer.new();
    const parser = Parser.init(lexer, allocator);

    const statements = try parser.parseStatements();
}
