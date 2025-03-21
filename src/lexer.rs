#![allow(dead_code)]
#![allow(redundant_semicolons)]

use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

pub enum TokenTypes {
    // Keywords
    KeywordFn,
    KeywordImmut,
    KeywordHeap,
    KeywordFree,
    KeywordReturn,
    KeywordConstruct,
    KeywordIf,
    KeywordElse,
    KeywordFor,
    KeywordWhile,
    KeywordBreak,
    KeywordContinue,
    KeywordImport,
    KeywordAsync,
    KeywordAwait,
    KeywordCase,
    KeywordEnum,
    KeywordClass,
    KeywordStruct,
    KeywordOut,
    KeywordIsOk,
    KeywordIsErr,
    KeywordNew,
    // Data Types
    KeywordInt,
    KeywordUint,
    KeywordInt8,
    KeywordUint8,
    KeywordInt16,
    KeywordUint16,
    KeywordInt32,
    KeywordUint32,
    KeywordInt64,
    KeywordUint64,
    KeywordInt128,
    KeywordUint128,
    KeywordFloat,
    KeywordFloat32,
    KeywordFloat64,
    KeywordString,
    KeywordChar,
    KeywordBool,
    // Identifiers & Literals
    Identifier,
    IntegerLiteral,
    FloatLiteral,
    StringLiteral,
    CharLiteral,
    BooleanLiteral,
    // Operators & Symbols
    OperatorPlus,
    OperatorMinus,
    OperatorStar,
    OperatorSlash,
    OperatorPercent,
    OperatorAssign,
    OperatorEqual,
    OperatorNotEqual,
    OperatorLess,
    OperatorGreater,
    OperatorLessEqual,
    OperatorGreaterEqual,
    OperatorAnd,
    OperatorOr,
    OperatorNot,
    OperatorDot,
    OperatorComma,
    OperatorColon,
    OperatorSemicolon,
    OperatorArrow,
    OperatorPipe,
    OperatorAmpersand,
    OperatorTilde,
    OperatorCaret,
    OperatorLeftShift,
    OperatorRightShift,
    // Punctuation
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    BracketOpen,
    BracketClose,
    // Special Tokens
    CommentSingle,
    CommentBlock,
    EOF,
    Unknown,
}
pub struct Token {
    pub token_type: TokenTypes,
    pub value: String,
    pub line: u32,
    pub column: u32
}

pub fn tokenize(file_path: &str) -> Vec<Token> {
    let mut tokens = Vec::new();

    let path = Path::new(file_path);
    let mut file = File::open(&path).expect("Error: Could not open file");
    let mut content = String::new();
    file.read_to_string(&mut content).expect("Error: Could not read file");

    // Tokenization logic

    tokens.push(Token {
        token_type: TokenTypes::EOF,
        value: "".to_string(),
        line: 0,
        column: 0
    });

    tokens
}
