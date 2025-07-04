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
