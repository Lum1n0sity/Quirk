#![allow(dead_code)]
#![allow(redundant_semicolons)]

use std::ffi::OsStr;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::cmp::PartialEq;

#[derive(PartialEq, Debug)]
pub enum TokenType {
    // Keywords
    KeywordFn, KeywordImmut, KeywordLet, KeywordHeap, KeywordFree, KeywordReturn,
    KeywordConstruct, KeywordIf, KeywordElse, KeywordElseIf, KeywordFor, KeywordWhile, KeywordBreak,
    KeywordContinue, KeywordImport, KeywordAsync, KeywordAwait, KeywordCase, KeywordEnum,
    KeywordClass, KeywordStruct, KeywordOut, KeywordIsOk, KeywordIsErr, KeywordNew,
    // Data Types
    KeywordInt, KeywordUint, KeywordInt8, KeywordUint8, KeywordInt16, KeywordUint16,
    KeywordInt32, KeywordUint32, KeywordInt64, KeywordUint64, KeywordInt128, KeywordUint128,
    KeywordFloat, KeywordFloat32, KeywordFloat64, KeywordString, KeywordChar, KeywordBool,
    KeywordVoid,
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
    CommentSingle, CommentBlockStart, CommentBlockEnd, EOF, Unknown
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
    pub line: u32,
    pub column: u32,
}

pub fn get_tokens(file_path: &str) -> Vec<Token> {
    let path = Path::new(file_path);

    // Check if file ends with .qk
    if !path.file_name().unwrap_or_else(|| OsStr::new("")).to_str().unwrap().ends_with(".qk") {
        panic!("File must end with .qk");
    }

    // Get file content
    let mut file_: File = File::open(path).expect("Unable to open file");
    let mut content: String = String::new();
    file_.read_to_string(&mut content).expect("Unable to read file");

    // println!("Content: {}", content);

    let mut tokens = Vec::new();
    let mut word: String = String::new();
    let mut in_comment: bool = false;
    let mut in_string: bool = false;
    let mut line: u32 = 1;
    let mut column: u32 = 1;
    let chars: Vec<char> = content.chars().collect();

    let mut i: usize = 0;
    while i < chars.len() {
        let c = chars[i];

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

        if in_comment {
            if c == '\n' {
                line += 1;
                column = 1;
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
            if c == '\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
            i += 1;
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
            if !word.is_empty() {
                tokens.push(Token {token_type: classify_word(&word), value: word.clone(), line, column});
                word.clear();
            }

            if let Some(punc_type) = match_punctuation(&c.to_string()) {
                tokens.push(Token {token_type: punc_type, value: c.to_string(), line, column});
                i += 1;
                continue;
            }

            println!("{}", chars[i..].iter().take(2).collect::<String>().as_str());
            let op = match_operator(&chars[i..]);
            if op.0 != TokenType::Unknown {
                tokens.push(Token {token_type: op.0, value: op.1.clone(), line, column});
                i += op.1.len();
                continue;
            }

            tokens.push(Token {
                token_type: TokenType::Unknown,
                value: c.to_string(),
                line,
                column,
            });
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

fn classify_word(word: &str) -> TokenType {
    println!("Word: {}", word);
    match_keyword(word)
        .or_else(|| match_literal(word))
        // .or_else(|| match_punctuation(word))
        .or_else(|| Some(TokenType::Identifier))
        .unwrap()
}

fn match_keyword(word: &str) -> Option<TokenType> {
    match word {
        "fn" => Some(TokenType::KeywordFn),
        "immut" => Some(TokenType::KeywordImmut),
        "let" => Some(TokenType::KeywordLet),
        "heap!" => Some(TokenType::KeywordHeap),
        "free()" => Some(TokenType::KeywordFree),
        "return" => Some(TokenType::KeywordReturn),
        "construct" => Some(TokenType::KeywordConstruct),
        "if" => Some(TokenType::KeywordIf),
        "elif" => Some(TokenType::KeywordElseIf),
        "else" => Some(TokenType::KeywordElse),
        "for" => Some(TokenType::KeywordFor),
        "while" => Some(TokenType::KeywordWhile),
        "break" => Some(TokenType::KeywordBreak),
        "case" => Some(TokenType::KeywordCase),
        "continue" => Some(TokenType::KeywordContinue),
        "import" => Some(TokenType::KeywordImport),
        "async" => Some(TokenType::KeywordAsync),
        "await" => Some(TokenType::KeywordAwait),
        "enum" => Some(TokenType::KeywordEnum),
        "class" => Some(TokenType::KeywordClass),
        "struct" => Some(TokenType::KeywordStruct),
        "out" => Some(TokenType::KeywordOut),
        "is_ok" => Some(TokenType::KeywordIsOk),
        "is_err" => Some(TokenType::KeywordIsErr),
        "new" => Some(TokenType::KeywordNew),
        _ => None
    }
}

fn match_literal(word: &str) -> Option<TokenType> {
    if word.parse::<i64>().is_ok() { return Some(TokenType::IntegerLiteral) }
    if word.parse::<f64>().is_ok() { return Some(TokenType::FloatLiteral) }
    if word == "true" || word == "false" { return Some(TokenType::BooleanLiteral) }
    if word == "void" { return Some(TokenType::KeywordVoid) }
    None
}

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

fn match_punctuation(chars: &str) -> Option<TokenType> {
    match chars {
        "(" => Some(TokenType::PunctuationParenOpen),
        ")" => Some(TokenType::PunctuationParenClose),
        "{" => Some(TokenType::PunctuationBraceOpen),
        "}" => Some(TokenType::PunctuationBraceClose),
        "[" => Some(TokenType::PunctuationBracketOpen),
        "]" => Some(TokenType::PunctuationBraceClose),
        "," => Some(TokenType::PunctuationComma),
        "." => Some(TokenType::PunctuationDot),
        ":" => Some(TokenType::PunctuationColon),
        ";" => Some(TokenType::OperatorSemicolon),
        _ => None
    }
}