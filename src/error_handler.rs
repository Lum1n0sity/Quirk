use std::fmt::Formatter;
use strum_macros::Display;

#[derive(Copy, Debug, Display)]
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
    pub file: Option<String>,
    pub source: Option<Box<dyn std::error::Error>>
}

impl Err {
    pub fn new(error_type: ErrorType, message: impl Into<String>, line: u32, column: u32) -> Self {
        Err {
            error_type,
            message,
            line,
            column,
            file: None,
            source: None
        }
    }

    pub fn with_file(mut self, file: impl Into<String>) -> Self {
        self.file = Some(file.into());
        self
    }

    pub fn with_source(mut self, source: impl std::error::Error + 'static) -> Self {
        self.source = Some(Box::new(source));
        self
    }

    pub fn panic(&self) {
        panic!("{}", self);
    }

    pub fn into_err<T>(self) -> Result<T, Self> {
        Err(self)
    }
}

impl std::fmt::Display for Err {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: {} at line {} column {}",
            self.error_type, self.message, self.line, self.column
        )?;

        if let Some(file) = &self.file {
            write!(f, " in file '{}'", file)?;
        }

        if let Some(source) = &self.source {
            write!(f, "\nCaused by: {}", source)?;
        }

        Ok(())
    }
}