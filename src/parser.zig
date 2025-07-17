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

    fn parseSingleExpression(self: *Parser) Error!Expression {
        switch (try self.lexer.next() orelse return Error.NoTokenToParse) {
            Token.Int => |integer| return Expression{ .int_literal = integer },
            Token.True => return Expression{ .boolean = true },
            Token.False => return Expression{ .boolean = false },
            Token.String => |string| return Expression{ .string_literal = string },
            Token.Identifier => |ident| return Expression{ .identifier = ident },
            Token.Minus => {
                const expr = try self.arena.create(Expression);
                expr.* = try self.parseExpression();

                return Expression{ .negative = expr };
            },
            Token.Bang => {
                const expr = try self.arena.create(Expression);
                expr.* = try self.parseExpression();

                return Expression{ .not = expr };
            },
            else => unreachable,
        }
    }
    fn parseExpression(self: *Parser) Error!Expression {
        var expr = try self.parseSingleExpression();

        blk: while (try self.lexer.peek()) |peeked_token| {
            switch (peeked_token) {
                // infix expression
                .Plus,
                .Minus,
                .Asterisk,
                .Slash,
                .Equals,
                .NotEquals,
                .LessThan,
                .GreaterThan,
                => {
                    expr = try self.infixParse(expr, false);
                },
                // index expression
                .LBracket => unreachable,
                else => break :blk,
            }
        }

        return expr;
    }
    fn infixParse(self: *Parser, left_expr: Expression, left_boosted: bool) Error!Expression {
        var mut_left_expr = left_expr;
        const operation_token = try self.lexer.next() orelse unreachable;
        switch (operation_token) {
            .Plus, .Minus, .Asterisk, .Slash, .Equals, .NotEquals, .LessThan, .GreaterThan => {},
            else => std.debug.assert(false),
        }

        if (left_expr.isInfix() and !left_boosted) {
            if (operation_token.getPrecedence() > left_expr.getPrecedence()) {
                switch (mut_left_expr) {
                    .add, .subtract, .multiply, .divide, .greater_than, .less_than, .equals, .not_equals => |*children| {
                        const left = children.right;
                        const right = try self.arena.create(Expression);
                        right.* = try self.parseSingleExpression();

                        const new = try self.arena.create(Expression);
                        new.* = switch (operation_token) {
                            .Plus => Expression{ .add = .{ .left = left, .right = right } },
                            .Minus => Expression{ .subtract = .{ .left = left, .right = right } },
                            .Asterisk => Expression{ .multiply = .{ .left = left, .right = right } },
                            .Slash => Expression{ .divide = .{ .left = left, .right = right } },
                            .Equals => Expression{ .equals = .{ .left = left, .right = right } },
                            .NotEquals => Expression{ .not_equals = .{ .left = left, .right = right } },
                            .LessThan => Expression{ .less_than = .{ .left = left, .right = right } },
                            .GreaterThan => Expression{ .greater_than = .{ .left = left, .right = right } },
                            else => unreachable, // should be infix operator
                        };

                        children.right = new;
                        return mut_left_expr;
                    },
                    else => unreachable,
                }
            }
        }

        const exprs = try self.arena.alloc(Expression, 2);
        exprs[0] = left_expr;
        exprs[1] = try self.parseSingleExpression();

        return switch (operation_token) {
            .Plus => Expression{ .add = .{ .left = &exprs[0], .right = &exprs[1] } },
            .Minus => Expression{ .subtract = .{ .left = &exprs[0], .right = &exprs[1] } },
            .Asterisk => Expression{ .multiply = .{ .left = &exprs[0], .right = &exprs[1] } },
            .Slash => Expression{ .divide = .{ .left = &exprs[0], .right = &exprs[1] } },
            .Equals => Expression{ .equals = .{ .left = &exprs[0], .right = &exprs[1] } },
            .NotEquals => Expression{ .not_equals = .{ .left = &exprs[0], .right = &exprs[1] } },
            .LessThan => Expression{ .less_than = .{ .left = &exprs[0], .right = &exprs[1] } },
            .GreaterThan => Expression{ .greater_than = .{ .left = &exprs[0], .right = &exprs[1] } },
            else => unreachable, // should be infix operator
        };
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
                .Return => {
                    _ = try self.lexer.next();
                    const expr = try self.parseExpression();
                    const statement = Statement{ .Return = expr };
                    try statements.append(statement);
                    if (try self.lexer.next()) |tok| {
                        switch (tok) {
                            Token.Semicolon => {},
                            else => return Error.ExpectedSemicolon,
                        }
                    } else {
                        return Error.ExpectedSemicolon;
                    }
                },
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
        Expression.int_literal => |integer| return (integer == b.*.int_literal),
        Expression.boolean => |value| return value == b.*.boolean,
        Expression.identifier => |ident| return std.mem.eql(u8, ident, b.identifier),
        Expression.string_literal => |string| return std.mem.eql(u8, string, b.string_literal),
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
            .Return => |expected_expression| {
                const expression = statement.Return;

                try expect(eqlExpressions(&expected_expression, &expression));
            },
        }
    }
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
    const expected_statements = [_]Statement{ Statement{ .Let = .{ .name = "x", .expr = Expression{ .int_literal = 5 } } }, Statement{ .Let = .{ .name = "y", .expr = Expression{ .int_literal = 10 } } }, Statement{ .Let = .{ .name = "foobar", .expr = Expression{ .string_literal = "bazquaz" } } } };

    try testParser(statements, &expected_statements);
}
test "expression statement" {
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
    const expected_statements = [_]Statement{ Statement{ .Expression = Expression{ .identifier = "foobar" } }, Statement{ .Expression = Expression{ .int_literal = 5 } } };

    try testParser(statements, &expected_statements);
}
test "return statement" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();

    const arena = arena_allocator.allocator();
    const input =
        \\return 5;
        \\return 993322;
        \\return foobar;
    ;

    const lexer = Lexer.new(input);
    var parser = Parser.init(lexer, arena);

    const statements = try parser.parseStatements();
    const expected_statements = [_]Statement{ Statement{ .Return = Expression{ .int_literal = 5 } }, Statement{ .Return = Expression{ .int_literal = 993322 } }, Statement{ .Return = Expression{ .identifier = "foobar" } } };

    try testParser(statements, &expected_statements);
}

test "infix precedence" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();

    const arena = arena_allocator.allocator();

    const input =
        \\5 + 10 / 2;
    ;
    const lexer = Lexer.new(input);
    var parser = Parser.init(lexer, arena);

    const statements = try parser.parseStatements();
    var five = Expression{ .int_literal = 5 };
    var ten = Expression{ .int_literal = 10 };
    var two = Expression{ .int_literal = 2 };

    var divided = Expression{ .divide = .{ .left = &ten, .right = &two } };

    const expected_statements = [_]Statement{Statement{ .Expression = Expression{ .add = .{ .left = &five, .right = &divided } } }};

    try testParser(statements, &expected_statements);
}
test "prefix expression" {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();

    const arena = arena_allocator.allocator();

    const input =
        \\-foobar;
        \\!10;
        \\!x;
    ;
    const lexer = Lexer.new(input);
    var parser = Parser.init(lexer, arena);

    const statements = try parser.parseStatements();

    const expected_statements = [_]Statement{ Statement{ .Expression = Expression{ .negative = &Expression{ .identifier = "foobar" } } }, Statement{ .Expression = Expression{ .not = &Expression{ .int_literal = 10 } } }, Statement{ .Expression = Expression{ .not = &Expression{ .identifier = "x" } } } };

    try testParser(statements, &expected_statements);
}
