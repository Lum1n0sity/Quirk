#![allow(dead_code)]
#![allow(redundant_semicolons)]

use std::ffi::OsStr;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use strum_macros::Display;
use crate::error_handler::*;

#[derive(PartialEq, Debug, Clone, Display)]
pub enum TokenType {
    // Keywords
    KeywordFn, KeywordImmut, KeywordLet, KeywordHeap, KeywordFree, KeywordReturn,
    KeywordIf, KeywordElse, KeywordElIf, KeywordFor, KeywordWhile, KeywordBreak,
    KeywordContinue, KeywordConstruct, KeywordImport, KeywordAsync, KeywordAwait, KeywordEnum,
    KeywordClass, KeywordStruct, KeywordOut, KeywordNew,
    // Data Types
    DataTypeInt, DataTypeUint, DataTypeInt8, DataTypeUint8, DataTypeInt16, DataTypeUint16,
    DataTypeInt32, DataTypeUint32, DataTypeInt64, DataTypeUint64, DataTypeInt128, DataTypeUint128,
    DataTypeFloat, DataTypeFloat32, DataTypeFloat64, DataTypeString, DataTypeChar, DataTypeBool,
    DataTypeVoid,
    // Identifiers & Literals
    Identifier, IntegerLiteral, FloatLiteral, StringLiteral, CharLiteral, BooleanLiteral,
    // Operators & Symbols
    OperatorPlus, OperatorMinus, OperatorStar, OperatorSlash, OperatorPercent,
    OperatorAssign, OperatorEqual, OperatorNotEqual, OperatorLess, OperatorGreater,
    OperatorLessEqual, OperatorGreaterEqual, OperatorAnd, OperatorOr, OperatorNot,
    OperatorSemicolon, OperatorArrow,
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
                format!("Unknown character '{}'", c),
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

/// Classifies a word into its appropriate TokenType based on its content and format.
///
/// # Arguments
///
/// * `word` - A string slice that holds the word to be classified
///
/// # Returns
///
/// * `TokenType` - Returns the appropriate TokenType based on the word's content:
///   - Keywords (if, else, while, etc.)
///   - Data types (int, float, string, etc.)
///   - Literals (numbers, strings, booleans)
///   - Identifiers (variable names, function names)
///
/// # Example
///
/// ```
/// let token_type = classify_word("42");
/// assert_eq!(token_type, TokenType::IntegerLiteral);
/// ```
fn classify_word(word: &str) -> TokenType {
    match_keyword(word)
        .or_else(|| match_literal(word))
        .or_else(|| Some(TokenType::Identifier))
        .unwrap()
}

/// Matches a given word against predefined keywords in the language and returns the corresponding TokenType.
///
/// # Arguments
///
/// * `word` - A string slice that holds the word to be matched against keywords
///
/// # Returns
///
/// * `TokenType` - Returns the matching keyword TokenType if found, otherwise returns TokenType::Identifier
///
/// # Example
///
/// ```
/// let token_type = match_keyword("if");
/// assert_eq!(token_type, TokenType::KeywordIf);
/// ```
fn match_keyword(word: &str) -> Option<TokenType> {
    match word {
        "fn" => Some(TokenType::KeywordFn),
        "immut" => Some(TokenType::KeywordImmut),
        "let" => Some(TokenType::KeywordLet),
        "heap!" => Some(TokenType::KeywordHeap),
        "free" => Some(TokenType::KeywordFree),
        "return" => Some(TokenType::KeywordReturn),
        "construct" => Some(TokenType::KeywordConstruct),
        "if" => Some(TokenType::KeywordIf),
        "elif" => Some(TokenType::KeywordElIf),
        "else" => Some(TokenType::KeywordElse),
        "for" => Some(TokenType::KeywordFor),
        "while" => Some(TokenType::KeywordWhile),
        "break" => Some(TokenType::KeywordBreak),
        "continue" => Some(TokenType::KeywordContinue),
        "import" => Some(TokenType::KeywordImport),
        "async" => Some(TokenType::KeywordAsync),
        "await" => Some(TokenType::KeywordAwait),
        "enum" => Some(TokenType::KeywordEnum),
        "class" => Some(TokenType::KeywordClass),
        "struct" => Some(TokenType::KeywordStruct),
        "out" => Some(TokenType::KeywordOut),
        "new" => Some(TokenType::KeywordNew),
        // Data Types
        "int" => Some(TokenType::DataTypeInt),
        "uint" => Some(TokenType::DataTypeUint),
        "int8" => Some(TokenType::DataTypeInt8),
        "uint8" => Some(TokenType::DataTypeUint8),
        "int16" => Some(TokenType::DataTypeInt16),
        "uint16" => Some(TokenType::DataTypeUint16),
        "int32" => Some(TokenType::DataTypeInt32),
        "uint32" => Some(TokenType::DataTypeUint32),
        "int64" => Some(TokenType::DataTypeInt64),
        "uint64" => Some(TokenType::DataTypeUint64),
        "int128" => Some(TokenType::DataTypeInt128),
        "uint128" => Some(TokenType::DataTypeUint128),
        "float" => Some(TokenType::DataTypeFloat),
        "float32" => Some(TokenType::DataTypeFloat32),
        "float64" => Some(TokenType::DataTypeFloat64),
        "string" => Some(TokenType::DataTypeString),
        "char" => Some(TokenType::DataTypeChar),
        "bool" => Some(TokenType::DataTypeBool),
        "void" => Some(TokenType::DataTypeVoid),
        _ => None
    }
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
/// assert_eq!(match_literal("hello"), None);
/// ```
///
/// # Description
/// Checks the input string in the following order:
/// 1. Attempts to parse as an integer (i64)
/// 2. Attempts to parse as a float (f64)
/// 3. Checks if it's a boolean literal ("true" or "false")
/// Returns None if none of these patterns match.
fn match_literal(word: &str) -> Option<TokenType> {
    if word.parse::<i64>().is_ok() { return Some(TokenType::IntegerLiteral) }
    if word.parse::<f64>().is_ok() { return Some(TokenType::FloatLiteral) }
    if word == "true" || word == "false" { return Some(TokenType::BooleanLiteral) }
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
        "->" => return (TokenType::OperatorArrow, "->".to_string()),
        // "<<" => (TokenType::OperatorLeftShift),
        // ">>" => (TokenType::OperatorRightShift),
        "++" => return (TokenType::OperatorIncrease, "++".to_string()),
        "--" => return (TokenType::OperatorDecrease, "--".to_string()),
        _ => {},
    }
    
    let op_str = chars[0].to_string();
    match op_str.as_str() {
        "+" => (TokenType::OperatorPlus, "+".to_string()),
        "-" => (TokenType::OperatorMinus, "-".to_string()),
        "*" => (TokenType::OperatorStar, "*".to_string()),
        "%" => (TokenType::OperatorPercent, "%".to_string()),
        "/" => (TokenType::OperatorSlash, "/".to_string()),
        "=" => (TokenType::OperatorAssign, "=".to_string()),
        ">" => (TokenType::OperatorGreater, ">".to_string()),
        "<" => (TokenType::OperatorLess, "<".to_string()),
        "!" => (TokenType::OperatorNot, "!".to_string()),
        "|" => (TokenType::OperatorPipe, "|".to_string()),
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