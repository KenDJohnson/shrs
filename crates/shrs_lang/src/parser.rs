//! Generated parser

use std::fmt;

use crate::{
    ast, grammar,
    lexer::{Error as LexError, Lexer, Token},
    position::{self, ByteRange, PosInfo, RangeInfo, SourceLines},
};
use annotate_snippets::{Level, Renderer, Snippet};
use lalrpop_util::ParseError as LalrpopError;

#[derive(Debug)]
pub enum ParserError<I> {
    InvalidToken {
        pos: PosInfo,
        error_input: I,
        error_lines: I,
    },
    UnrecognizedEof {
        pos: PosInfo,
        expected: Vec<String>,
        error_lines: I,
    },
    UnrecognizedToken {
        range: RangeInfo,
        token: Token<I>,
        expected: Vec<String>,
        error_input: I,
        error_lines: I,
    },
    ExtraToken {
        range: RangeInfo,
        token: Token<I>,
        error_input: I,
        error_lines: I,
    },
    UnrecognizedChar {
        range: RangeInfo,
        error_input: I,
        error_lines: I,
    },
}

impl<I: AsRef<str>> fmt::Display for ParserError<I> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let start = self.start_line_num();
        let title = self.title();
        let (input, lines) = self.src();
        let label = self.label();
        let msg =
            Level::Error
                .title(title)
                .snippet(lines_snippet(start, input, lines, label.as_deref()));

        Renderer::styled().render(msg).fmt(f)?;
        Ok(())
    }
}

impl<I: fmt::Debug + AsRef<str>> std::error::Error for ParserError<I> {}

fn expected_label(expected: &[String]) -> Result<String, fmt::Error> {
    use fmt::Write;
    if expected.len() > 1 {
        let mut label = format!("expected one of ");
        for e in expected.iter().take(1) {
            write!(&mut label, "`{e}`")?;
        }
        for e in expected.iter().skip(1) {
            write!(&mut label, ", `{e}`")?;
        }
        Ok(label)
    } else {
        Ok(format!("expected token `{}`", expected[0]))
    }
}

fn lines_snippet<'a>(
    start: usize,
    input: &'a str,
    lines: &'a str,
    label: Option<&'a str>,
) -> Snippet<'a> {
    let offset = lines.find(input).unwrap();
    let relative = offset..(offset + input.len());
    Snippet::source(lines).line_start(start).annotation({
        let annotation = Level::Error.span(relative);
        if let Some(label) = label {
            annotation.label(label)
        } else {
            annotation
        }
    })
}

impl<I: AsRef<str>> ParserError<I> {
    fn src(&self) -> (&str, &str) {
        match self {
            Self::InvalidToken {
                error_input,
                error_lines,
                ..
            }
            | Self::UnrecognizedToken {
                error_input,
                error_lines,
                ..
            }
            | Self::ExtraToken {
                error_input,
                error_lines,
                ..
            }
            | Self::UnrecognizedChar {
                error_input,
                error_lines,
                ..
            } => (error_input.as_ref(), error_lines.as_ref()),
            Self::UnrecognizedEof { error_lines, .. } => {
                (error_lines.as_ref(), error_lines.as_ref())
            },
        }
    }
}
impl<I> ParserError<I> {
    fn label(&self) -> Option<String> {
        match self {
            Self::UnrecognizedEof { expected, .. } | Self::UnrecognizedToken { expected, .. } => {
                Some(expected_label(expected).ok()?)
            },
            _ => None,
        }
    }
    fn title(&self) -> &str {
        match self {
            Self::InvalidToken { .. } => "invalid token",
            Self::UnrecognizedEof { .. } => "unexpected end of file",
            Self::UnrecognizedToken { .. } => "unrecognized token",
            Self::ExtraToken { .. } => "extra token",
            Self::UnrecognizedChar { .. } => "unrecognized character",
        }
    }
    fn start_line_num(&self) -> usize {
        match self {
            Self::InvalidToken { pos, .. } | Self::UnrecognizedEof { pos, .. } => {
                pos.cursor.line as usize + 1
            },
            Self::UnrecognizedToken { range, .. }
            | Self::ExtraToken { range, .. }
            | Self::UnrecognizedChar { range, .. } => range.cursor.start.line as usize + 1,
        }
    }
}

impl ParserError<&str> {
    pub fn into_owned(self) -> ParserError<String> {
        match self {
            Self::InvalidToken {
                pos,
                error_input,
                error_lines,
            } => ParserError::InvalidToken {
                pos,
                error_input: error_input.to_owned(),
                error_lines: error_lines.to_owned(),
            },
            Self::UnrecognizedEof {
                pos,
                expected,
                error_lines,
            } => ParserError::UnrecognizedEof {
                pos,
                expected,
                error_lines: error_lines.to_owned(),
            },
            Self::UnrecognizedToken {
                range,
                token,
                expected,
                error_input,
                error_lines,
            } => ParserError::UnrecognizedToken {
                range,
                token: token.into_owned(),
                expected,
                error_input: error_input.to_owned(),
                error_lines: error_lines.to_owned(),
            },
            Self::ExtraToken {
                range,
                token,
                error_input,
                error_lines,
            } => ParserError::ExtraToken {
                range,
                token: token.into_owned(),
                error_input: error_input.to_owned(),
                error_lines: error_lines.to_owned(),
            },
            Self::UnrecognizedChar {
                range,
                error_input,
                error_lines,
            } => ParserError::UnrecognizedChar {
                range,
                error_input: error_input.to_owned(),
                error_lines: error_lines.to_owned(),
            },
        }
    }
}

#[derive(Default)]
pub struct Parser {}

impl Parser {
    pub fn parse<'input>(
        &self,
        lexer: Lexer<'input>,
    ) -> Result<ast::Command, ParserError<&'input str>> {
        let input = lexer.input();
        let lines = SourceLines::new(input);
        let range_info = |start, end| lines.range_info(ByteRange::new(start, end));
        let src = |range| (&input[range], &input[position::line_range(input, range)]);
        grammar::ProgramParser::new()
            .parse(lexer.input(), lexer)
            .map_err(|inner| match inner {
                LalrpopError::InvalidToken { location } => {
                    let pos = lines.position_info(location);
                    let pos_range = position::offset_range(input, pos.bytes);
                    let (error_input, error_lines) = src(pos_range);
                    ParserError::InvalidToken {
                        pos,
                        error_input,
                        error_lines,
                    }
                },
                LalrpopError::UnrecognizedEOF { location, expected } => {
                    let pos = lines.position_info(location);
                    let pos_range = position::offset_range(input, pos.bytes);
                    let (_error_input, error_lines) = src(pos_range);
                    ParserError::UnrecognizedEof {
                        pos,
                        expected,
                        error_lines,
                    }
                },
                LalrpopError::UnrecognizedToken {
                    token: (start, token, end),
                    expected,
                } => {
                    let range = range_info(start, end);
                    let (error_input, error_lines) = src(range.bytes);
                    ParserError::UnrecognizedToken {
                        range,
                        token,
                        expected,
                        error_input,
                        error_lines,
                    }
                },
                LalrpopError::ExtraToken {
                    token: (start, token, end),
                } => {
                    let range = range_info(start, end);
                    let (error_input, error_lines) = src(range.bytes);
                    ParserError::ExtraToken {
                        range,
                        token,
                        error_input,
                        error_lines,
                    }
                },
                LalrpopError::User {
                    error: LexError::UnrecognizedChar(start, _, end),
                } => {
                    let range = range_info(start, end);
                    let (error_input, error_lines) = src(range.bytes);
                    ParserError::UnrecognizedChar {
                        range,
                        error_input,
                        error_lines,
                    }
                },
            })
    }
}

/*
#[cfg(test)]
mod tests {

    use super::grammar;

    #[test]
    fn parse() {
        let res = grammar::ProgramParser::new().parse("ls home | grep downloads");
        println!("{:?}", res);
    }

    #[test]
    fn and_or() {
        let res = grammar::ProgramParser::new().parse("ls home || grep downloads");
        println!("{:?}", res);
    }
}
*/
