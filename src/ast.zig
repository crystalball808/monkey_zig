// pub const ExpressionType = enum { int_literal, boolean, string_literal, identifier, not, negative, equals, not_equals, add, subtract, multiply, divide, greater_than, less_than, if_else, func, call };
pub const Expression = union(enum) {
    const InfixChildren = struct { left: *Expression, right: *Expression };
    const IfChildren = struct { condition: *Expression, then: *Expression, alternative: ?*Expression };

    IntLiteral: i32,
    Boolean: bool,
    StringLiteral: []const u8,
    Identifier: []const u8,

    //  prefix operator
    Not: *Expression, // index of another expression
    Negative: *Expression, // index of another expression

    // infix operator
    Equals: InfixChildren,
    NotEquals: InfixChildren,
    Add: InfixChildren,
    Subtract: InfixChildren,
    Multiply: InfixChildren,
    Divide: InfixChildren,
    GreaterThan: InfixChildren,
    LessThan: InfixChildren,

    If: IfChildren,
    Func: struct {
        args: [][]const u8,
        statements: []const Statement,
    },
    Call: struct { func: *Expression, args: []const Expression },

    pub fn isInfix(self: *const Expression) bool {
        return switch (self.*) {
            .Equals, .NotEquals, .Add, .Subtract, .Multiply, .Divide, .GreaterThan, .LessThan => true,
            else => false,
        };
    }
    pub fn getPrecedence(self: *const Expression) u4 {
        return switch (self.*) {
            .Equals, .NotEquals => 0,
            .GreaterThan, .LessThan => 1,
            .Add, .Subtract => 2,
            .Multiply, .Divide => 3,
        };
    }
};

pub const Statement = union(enum) { Expression: Expression, Return: Expression, Let: struct { name: []const u8, expr: Expression } };
