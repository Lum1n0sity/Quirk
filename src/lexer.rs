#![allow(dead_code)]
#![allow(redundant_semicolons)]

use std::ffi::OsStr;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use strum_macros::Display;
use crate::error_handler::*;
use std::collections::HashMap;
use once_cell::sync::Lazy;

static KEYWORDS: Lazy<HashMap<&'static str, TokenType>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert("fn", TokenType::KeywordFn);
    m.insert("immut", TokenType::KeywordImmut);
    m.insert("let", TokenType::KeywordLet);
    m.insert("heap!", TokenType::KeywordHeap);
    m.insert("return", TokenType::KeywordReturn);
    m.insert("construct", TokenType::KeywordConstruct);
    m.insert("if", TokenType::KeywordIf);
    m.insert("elif", TokenType::KeywordElIf);
    m.insert("else", TokenType::KeywordElse);
    m.insert("for", TokenType::KeywordFor);
    m.insert("while", TokenType::KeywordWhile);
    m.insert("break", TokenType::KeywordBreak);
    m.insert("continue", TokenType::KeywordContinue);
    m.insert("import", TokenType::KeywordImport);
    m.insert("async", TokenType::KeywordAsync);
    m.insert("await", TokenType::KeywordAwait);
    m.insert("enum", TokenType::KeywordEnum);
    m.insert("class", TokenType::KeywordClass);
    m.insert("struct", TokenType::KeywordStruct);
    m.insert("this", TokenType::KeywordThis);
    m
});

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display)]
pub enum TokenType {
    // Keywords
    KeywordFn, KeywordImmut, KeywordLet, KeywordHeap, KeywordReturn,
    KeywordIf, KeywordElse, KeywordElIf, KeywordFor, KeywordWhile, KeywordBreak,
    KeywordContinue, KeywordEnum, KeywordStruct,
    KeywordClass, KeywordConstruct,  KeywordThis, KeywordImport, KeywordAsync, KeywordAwait,
    // Data Types
    DataTypeInt, DataTypeUint, DataTypeInt8, DataTypeUint8, DataTypeInt16, DataTypeUint16,
    DataTypeInt32, DataTypeUint32, DataTypeInt64, DataTypeUint64, DataTypeInt128, DataTypeUint128,
    DataTypeFloat, DataTypeFloat32, DataTypeFloat64, DataTypeString, DataTypeChar, DataTypeBool,
    DataTypeVoid, DataTypeAny,
    // Identifiers & Literals
    Identifier, IntegerLiteral, FloatLiteral, StringLiteral, CharLiteral, BooleanLiteral, NoneLiteral,
    // Operators & Symbols
    OperatorPlus, OperatorMinus, OperatorMultiply, OperatorDivide, OperatorModulo,
    OperatorAssign, OperatorEqual, OperatorNotEqual, OperatorLess, OperatorGreater,
    OperatorLessEqual, OperatorGreaterEqual, OperatorAnd, OperatorOr, OperatorNot, OperatorOptional, OperatorFallback,
    OperatorSemicolon,
    OperatorPipe, OperatorAmpersand, OperatorTilde, OperatorCaret,
    OperatorIncrease, OperatorDecrease,
    // Punctuation
    PunctuationParenOpen, PunctuationParenClose, PunctuationBraceOpen, PunctuationBraceClose, PunctuationBracketOpen, PunctuationBracketClose,
    PunctuationDot, PunctuationComma, PunctuationColon,
    // Special Tokens
    EOL, EOF, Unknown,
    // AST Generator Helpers
    Root, CodeBlock
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
    pub line: u32,
    pub column: u32,
}

/// Reads and tokenizes a Quirk source file (.qk), generating a vector of Tokens.
///
/// # Arguments
///
/// * `file_path` - A string slice containing the path to the .qk source file
///
/// # Returns
///
/// * `Vec<Token>` - Returns a vector of Token structs, each containing:
///   - token_type: The classified TokenType
///   - value: The actual text value of the token
///   - line: Line number where the token appears
///   - column: Column number where the token appears
///
/// # Errors
///
/// Returns an error if:
/// - The file doesn't have a .qk extension
/// - The file cannot be read
/// - The file contains invalid tokens
///
/// # Example
///
/// ```
/// let tokens = get_tokens("example.qk");
/// match tokens {
///     Ok(token_vec) => println!("Successfully tokenized file"),
///     Err(e) => println!("Error tokenizing file: {}", e),
/// }
/// ```
pub fn get_tokens(file_path: &str) -> Vec<Token> {
    let path = Path::new(file_path);

    // Check if file ends with .qk
    if !path.file_name().unwrap_or_else(|| OsStr::new("")).to_str().unwrap().ends_with(".qk") {
        let _err = Err::new(
            ErrorType::Syntax,
            "Invalid file type, file must end with .qk",
            0,
            0
        ).with_file(file_path).panic();
    }

    // Get file content
    let mut file_: File = File::open(path).expect("Unable to open file");
    let mut content: String = String::new();
    file_.read_to_string(&mut content).expect("Unable to read file");

    let mut tokens = Vec::new();
    let mut word: String = String::new();
    let mut in_comment: bool = false;
    let mut in_block_comment: bool = false;
    let mut in_string: bool = false;
    let mut line: u32 = 1;
    let mut column: u32 = 1;
    let chars: Vec<char> = content.chars().collect();

    let mut i: usize = 0;
    while i < chars.len() {
        let c = chars[i];

        if c != '\n' && !c.is_whitespace() {
            column += 1;
        }

        if c == '\n' {
            tokens.push(Token{ token_type: TokenType::EOL, value: "\n".to_string(), line, column});
            line += 1;
            column = 1;
        }

        if in_string {
            if c == '"' {
                in_string = false;
                tokens.push(Token{ token_type: TokenType::StringLiteral, value: word.clone(), line, column});
                word.clear();
            } else {
                word.push(c);
            }

            i += 1;
            continue;
        }

        if in_block_comment {
            if c == '*' && i + 1 < chars.len() && chars[i + 1] == '/' {
                in_block_comment = false;
                column += 1;
                i += 2;
                continue;
            } else if c == '\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
            i += 1;
            continue;
        }

        if in_comment {
            if c == '\n' {
                line += 1;
                column = 1;
                in_comment = false;
            } else {
                column += 1;
            }

            i += 1;
            continue;
        }

        if c.is_whitespace() {
            if !word.is_empty() {
                tokens.push(Token {token_type: classify_word(&word), value: word.clone(), line, column});
                word.clear();
            }

            if c != '\n' {
                column += 1;
            }
            i += 1;
            continue;
        }

        if c == '/' && i + 1 < chars.len() && chars[i + 1] == '*' {
            in_block_comment = true;
            i += 2;
            column += 2;
            continue;
        }

        if c == '"' {
            in_string = true;
            i += 1;
            continue;
        }

        if c == '/' && i + 1 < chars.len() && chars[i + 1] == '/' {
            in_comment = true;
            i += 2;
            continue;
        }

        if c.is_ascii_punctuation() {
            if !word.is_empty() && word == "heap" && c == '!' {
                word.push(c);
                i += 1;
                column += 1;
                continue;
            }

            if !word.is_empty() {
                tokens.push(Token {token_type: classify_word(&word), value: word.clone(), line, column});
                word.clear();
            }

            if let Some(punc_type) = match_punctuation(&c.to_string()) {
                tokens.push(Token {token_type: punc_type, value: c.to_string(), line, column});
                i += 1;
                continue;
            }

            let op = match_operator(&chars[i..]);
            if op.0 != TokenType::Unknown {
                tokens.push(Token {token_type: op.0, value: op.1.clone(), line, column});
                i += op.1.len();
                continue;
            }

            // Create a new error and panic when the character cannot be tokenized and is therefor TokenType::Unknown
            let _err = Err::new(
                ErrorType::Syntax,
                format!("Unexpected token '{}'", c),
                line,
                column
            ).with_file(file_path).panic();

            i += 1;
        }

        word.push(c);
        i += 1;
    }

    if !word.is_empty() {
        tokens.push(Token {token_type: classify_word(&word), value: word.clone(), line, column});
    }

    tokens.push(Token{token_type: TokenType::EOF, value: "".to_string(), line, column});
    tokens
}

/// Looks ahead in the character array without advancing the current position.
///
/// # Arguments
///
/// * `chars` - A slice of characters to look ahead in
/// * `i` - The current position in the character slice
/// * `n` - The number of positions to look ahead
///
/// # Returns
///
/// * `Option<char>` - Returns Some(char) if a character exists at position i + n,
///                    None if the position is out of bounds
///
/// # Examples
///
/// ```
/// let chars = vec!['a', 'b', 'c'];
/// assert_eq!(peek(&chars, 0, 1), Some('b')); // Peek one character ahead
/// assert_eq!(peek(&chars, 0, 2), Some('c')); // Peek two characters ahead
/// assert_eq!(peek(&chars, 1, 2), None);      // Peek beyond the end
/// ```
///
/// This function is particularly useful for:
/// - Looking ahead to make decisions about token classification
/// - Checking for multi-character operators or tokens
/// - Preventing buffer overflows by safely handling bounds checking
fn peek(chars: &[char], i: usize, n: usize) -> Option<char> {
    if i + n < chars.len() {
        chars.get(i + n).copied()
    } else {
        None
    }
}

/// Classifies a word into its corresponding TokenType.
///
/// # Arguments
///
/// * `word` - A string slice containing the word to classify
///
/// # Returns
///
/// * `TokenType` - Returns the appropriate TokenType for the word:
///   - A keyword TokenType if the word matches a predefined keyword
///   - A literal TokenType if the word matches a literal pattern
///   - TokenType::Identifier if no other matches are found
///
/// # Examples
///
/// ```
/// // Keywords
/// assert_eq!(classify_word("if"), TokenType::KeywordIf);
///
/// // Literals
/// assert_eq!(classify_word("123"), TokenType::IntegerLiteral);
/// assert_eq!(classify_word("true"), TokenType::BooleanLiteral);
///
/// // Identifiers
/// assert_eq!(classify_word("myVariable"), TokenType::Identifier);
/// ```
///
/// This function first checks if the word is a predefined keyword using the KEYWORDS map.
/// If not a keyword, it attempts to match the word as a literal using match_literal().
/// If neither a keyword nor a literal, it defaults to classifying the word as an Identifier.
fn classify_word(word: &str) -> TokenType {
    if let Some(t) = KEYWORDS.get(word) {
        return *t;
    }

    match_literal(word).unwrap_or(TokenType::Identifier)
}

/// Attempts to classify a string as a literal token type.
///
/// # Arguments
/// * `word` - A string slice to be classified
///
/// # Returns
/// * `Option<TokenType>` - Some(TokenType) if the word matches a literal pattern,
///                        None if no literal pattern is matched
///
/// # Examples
/// ```
/// assert_eq!(match_literal("123"), Some(TokenType::IntegerLiteral));
/// assert_eq!(match_literal("3.14"), Some(TokenType::FloatLiteral));
/// assert_eq!(match_literal("true"), Some(TokenType::BooleanLiteral));
/// assert_eq!(match_literal("None"), Some(TokenType::NoneLiteral));
/// assert_eq!(match_literal("hello"), None);
/// ```
///
/// # Description
/// Checks the input string in the following order:
/// 1. Attempts to parse as an integer (i64)
/// 2. Attempts to parse as a float (f64)
/// 3. Checks if it's a boolean literal ("true" or "false")
/// 4. Checks if it's the None literal ("None")
/// Returns None if none of these patterns match.
fn match_literal(word: &str) -> Option<TokenType> {
    if word.parse::<i64>().is_ok() { return Some(TokenType::IntegerLiteral) }
    if word.parse::<f64>().is_ok() { return Some(TokenType::FloatLiteral) }
    if word == "true" || word == "false" { return Some(TokenType::BooleanLiteral) }
    if word == "None" { return Some(TokenType::NoneLiteral) }
    None
}

/// Matches and classifies operator tokens from a character slice.
///
/// # Arguments
/// * `chars` - A slice of characters to be analyzed
///
/// # Returns
/// * A tuple containing:
///   - TokenType: The type of operator token identified
///   - String: The string representation of the operator
///
/// # Description
/// First attempts to match two-character operators (==, !=, >=, etc.).
/// If no two-character operator is matched, falls back to single-character
/// operators (+, -, *, etc.). Returns Unknown token type for unrecognized
/// operators.
fn match_operator(chars: &[char]) -> (TokenType, String) {
    let op_str = chars.iter().take(2).collect::<String>();
    match op_str.as_str() {
        "==" => return (TokenType::OperatorEqual, "==".to_string()),
        "!=" => return (TokenType::OperatorNotEqual, "!=".to_string()),
        ">=" => return (TokenType::OperatorGreaterEqual, ">=".to_string()),
        "<=" => return (TokenType::OperatorLessEqual, "<=".to_string()),
        "&&" => return (TokenType::OperatorAnd, "&&".to_string()),
        "||" => return (TokenType::OperatorOr, "||".to_string()),
        "++" => return (TokenType::OperatorIncrease, "++".to_string()),
        "--" => return (TokenType::OperatorDecrease, "--".to_string()),
        "??" => return (TokenType::OperatorFallback, "??".to_string()),
        _ => {},
    }

    let op_str = chars[0].to_string();
    match op_str.as_str() {
        "+" => (TokenType::OperatorPlus, "+".to_string()),
        "-" => (TokenType::OperatorMinus, "-".to_string()),
        "*" => (TokenType::OperatorMultiply, "*".to_string()),
        "%" => (TokenType::OperatorModulo, "%".to_string()),
        "/" => (TokenType::OperatorDivide, "/".to_string()),
        "=" => (TokenType::OperatorAssign, "=".to_string()),
        ">" => (TokenType::OperatorGreater, ">".to_string()),
        "<" => (TokenType::OperatorLess, "<".to_string()),
        "!" => (TokenType::OperatorNot, "!".to_string()),
        "|" => (TokenType::OperatorPipe, "|".to_string()),
        "?" => (TokenType::OperatorOptional, "?".to_string()),
        "&" => (TokenType::OperatorAmpersand, "&".to_string()),
        "~" => (TokenType::OperatorTilde, "~".to_string()),
        "^" => (TokenType::OperatorCaret, "^".to_string()),
        _ => (TokenType::Unknown, chars[0].to_string()),
    }
}

/// Matches a string of characters against a set of punctuation tokens.
///
/// # Parameters
///
/// - `chars`: The string of characters to match.
///
/// # Returns
///
/// A `TokenType` representing the matched punctuation token, or `None` if no match is found.
fn match_punctuation(chars: &str) -> Option<TokenType> {
    match chars {
        "(" => Some(TokenType::PunctuationParenOpen),
        ")" => Some(TokenType::PunctuationParenClose),
        "{" => Some(TokenType::PunctuationBraceOpen),
        "}" => Some(TokenType::PunctuationBraceClose),
        "[" => Some(TokenType::PunctuationBracketOpen),
        "]" => Some(TokenType::PunctuationBracketClose),
        "," => Some(TokenType::PunctuationComma),
        "." => Some(TokenType::PunctuationDot),
        ":" => Some(TokenType::PunctuationColon),
        ";" => Some(TokenType::OperatorSemicolon),
        _ => None
    }
}