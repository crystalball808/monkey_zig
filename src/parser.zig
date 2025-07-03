const std = @import("std");

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

    fn new(lexer: Lexer) Parser {
        return Parser{lexer};
    }

    fn parseStatements(self: *Parser, allocator: std.mem.Allocator) Error!StatementList {
        const statements = StatementList.init(allocator);

        var maybe_token = try self.lexer.getNextToken();
        while (maybe_token) |token| {
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
            maybe_token = try self.lexer.getNextToken();
        }

        return statements;
    }
};
