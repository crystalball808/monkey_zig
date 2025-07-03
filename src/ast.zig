pub const Expression = union(enum) {
    const InfixChildren = struct { left: *Expression, right: *Expression };
    const IfChildren = struct { condition: *Expression, then: *Expression, alternative: ?*Expression };

    IntLiteral: i32,
    Boolean: bool,
    Identifier: []const u8,

    // prefix operator
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
};

pub const Statement = union(enum) { Expression: Expression, Return: Expression, Let: struct { name: []const u8, expr: *Expression } };
