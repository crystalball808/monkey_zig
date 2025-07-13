pub const ExpressionType = enum { int_literal, boolean, string_literal, identifier, not, negative, equals, not_equals, add, subtract, multiply, divide, greater_than, less_than, if_else, func, call };
pub const Expression = union(enum) {
    const InfixChildren = struct { left: *Expression, right: *Expression };
    const IfChildren = struct { condition: *Expression, then: *Expression, alternative: ?*Expression };

    int_literal: i32,
    boolean: bool,
    string_literal: []const u8,
    identifier: []const u8,

    //  prefix operator
    not: *Expression, // index of another expression
    negative: *Expression, // index of another expression

    // infix operator
    equals: InfixChildren,
    equals_boosted: InfixChildren,
    not_equals: InfixChildren,
    not_equals_boosted: InfixChildren,
    add: InfixChildren,
    add_boosted: InfixChildren,
    subtract: InfixChildren,
    subtract_boosted: InfixChildren,
    multiply: InfixChildren,
    multiply_boosted: InfixChildren,
    divide: InfixChildren,
    divide_boosted: InfixChildren,
    greater_than: InfixChildren,
    greater_than_boosted: InfixChildren,
    less_than: InfixChildren,
    less_than_boosted: InfixChildren,

    if_else: IfChildren,
    func: struct {
        args: [][]const u8,
        statements: []const Statement,
    },
    call: struct { func: *Expression, args: []const Expression },

    pub fn isInfix(self: *const Expression) bool {
        return switch (self.*) {
            .equals, .not_equals, .add, .subtract, .multiply, .divide, .greater_than, .less_than => true,
            else => false,
        };
    }
    pub fn getPrecedence(self: *const Expression) u4 {
        return switch (self.*) {
            .equals, .NotEquals => 0,
            .greater_than, .less_than => 1,
            .add, .subtract => 2,
            .multiply, .divide => 3,
        };
    }
};

pub const Statement = union(enum) { Expression: Expression, Return: Expression, Let: struct { name: []const u8, expr: Expression } };
