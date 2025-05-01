use std::fmt::Formatter;
use strum_macros::Display;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::process;

#[derive(Clone, Copy, Debug, Display)]
pub enum ErrorType {
    Syntax,
    Type,
    Runtime,
    Undefined,
    Other,
}

pub struct Err {
    pub error_type: ErrorType,
    pub message: String,
    pub line: u32,
    pub column: u32,
    pub file: String,
    pub source: Option<Box<dyn std::error::Error>>,
    pub pointer_msg: Option<String>,
}

impl Err {
    pub fn new(error_type: ErrorType, message: impl Into<String>, line: u32, column: u32, file_path: impl Into<String>) -> Self {
        Err {
            error_type,
            message: message.into(),
            line,
            column,
            file: file_path.into(),
            source: None,
            pointer_msg: None,
        }
    }

    pub fn with_source(mut self, source: impl std::error::Error + 'static) -> Self {
        self.source = Some(Box::new(source));
        self
    }

    pub fn with_pointer_msg(mut self, pointer_msg: impl Into<String>) -> Self {
        self.pointer_msg = Some(pointer_msg.into());
        self
    }

    pub fn panic(&self) {
        eprintln!("{}", self);
        process::exit(1);
    }

    pub fn print(&self) {
        println!("{}", self);
    }

    pub fn into_err<T>(self) -> Result<T, Self> {
        Err(self)
    }
}

impl std::fmt::Display for Err {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: {} at line {} column {}\n {} | {}\nin file '{}'",
            self.error_type, self.message, self.line, self.column, self.line, get_error_line(self.file.as_ref(), self.line).unwrap_or("".to_string()), self.file
        )?;

        if let Some(pointer_msg) = &self.pointer_msg {
            for _i in 0..self.column {
                write!(f, " ")?;
            }
            write!(f, "^---{}\n", pointer_msg)?;
        }

        // if let Some(file) = &self.file {
        //     write!(f, "in file '{}'", file)?;
        // }

        if let Some(source) = &self.source {
            write!(f, "\nCaused by: {}", source)?;
        }

        Ok(())
    }
}

fn get_error_line(file: &str, line: u32) -> Option<String> {
    let file = File::open(file).ok()?;
    let reader = BufReader::new(file);

    reader.lines().nth((line - 1) as usize)?.ok()
}