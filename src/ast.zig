pub const ExpressionType = enum { int_literal, boolean, string_literal, identifier, not, negative, equals, not_equals, add, subtract, multiply, divide, greater_than, less_than, if_else, func, call };
pub const Expression = union(enum) {
    const InfixChildren = struct { left: *const Expression, right: *const Expression };
    const IfChildren = struct { condition: *const Expression, then: *const Expression, alternative: ?*const Expression };

    int_literal: i32,
    boolean: bool,
    string_literal: []const u8,
    identifier: []const u8,

    //  prefix operator
    not: *const Expression, // index of another expression
    negative: *const Expression, // index of another expression

    // infix operator
    equals: InfixChildren,
    not_equals: InfixChildren,
    add: InfixChildren,
    subtract: InfixChildren,
    multiply: InfixChildren,
    divide: InfixChildren,
    greater_than: InfixChildren,
    less_than: InfixChildren,

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
            .equals, .not_equals => 0,
            .greater_than, .less_than => 1,
            .add, .subtract => 2,
            .multiply, .divide => 3,
            else => unreachable,
        };
    }
};

pub const Statement = union(enum) { expression: Expression, @"return": Expression, let: struct { name: []const u8, expr: Expression } };
