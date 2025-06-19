pub const Token = union(enum) {
    Illegal: void,
    EOF: void,
    // Identifiers + literals
    Identifier: []const u8,
    Int: u32,

    // Operators
    Assign: void,
    Plus: void,
    Minus: void,
    Equals: void,
    NotEquals: void,
    Bang: void,
    Asterisk: void,
    Slash: void,
    GreaterThan: void,
    LessThan: void,

    // Delimiters
    Comma: void,
    Semicolon: void,

    LParen: void,
    RParen: void,
    LBrace: void,
    RBrace: void,

    // Keywords
    Function: void,
    Let: void,
};
