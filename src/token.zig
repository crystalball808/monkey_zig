pub const Token = union(enum) {
    Illegal: void,
    EOF: void,
    // Identifiers + literals
    Identifier: []const u8,
    Int: u32,

    // Operators
    Assign: void,
    Plus: void,

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
