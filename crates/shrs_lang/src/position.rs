use std::ops::Index;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ByteRange {
    pub start: usize,
    pub end: usize,
}

impl ByteRange {
    #[inline]
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
    #[inline]
    pub const fn start(&self) -> usize {
        self.start
    }
    #[inline]
    pub const fn end(&self) -> usize {
        self.end
    }
}

impl From<ByteRange> for std::ops::Range<usize> {
    #[inline]
    fn from(range: ByteRange) -> std::ops::Range<usize> {
        range.start()..range.end()
    }
}

impl Index<ByteRange> for String {
    type Output = str;
    #[inline]
    fn index(&self, index: ByteRange) -> &str {
        &self[std::ops::Range::<usize>::from(index)]
    }
}

impl Index<ByteRange> for str {
    type Output = str;
    #[inline]
    fn index(&self, index: ByteRange) -> &str {
        &self[std::ops::Range::<usize>::from(index)]
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct RangeInfo {
    // pub bytes: std::ops::Range<usize>,
    pub bytes: ByteRange,
    pub cursor: CursorRange,
}

impl Index<RangeInfo> for String {
    type Output = str;
    #[inline]
    fn index(&self, index: RangeInfo) -> &str {
        Index::index(self, index.bytes)
    }
}

impl Index<RangeInfo> for str {
    type Output = str;
    #[inline]
    fn index(&self, index: RangeInfo) -> &str {
        Index::index(self, index.bytes)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PosInfo {
    pub bytes: usize,
    pub cursor: CursorPosition,
}

/// Position within text as a line/column pair (zero-based).
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct CursorPosition {
    pub line: u32,
    pub column: u32,
}

impl CursorPosition {
    #[allow(dead_code)]
    fn locate(&self, text: &str) -> Option<usize> {
        let mut lines = text.lines();
        let mut offset = 0;
        for _ in 0..self.line {
            dbg!(offset);
            let line = lines.next()?;
            offset += line.len() + 1;
        }
        dbg!(offset);
        let line = lines.next()?;
        dbg!(line.len());
        dbg!(self.column);
        if line.len() >= self.column as usize {
            Some(offset + self.column as usize)
        } else {
            None
        }
    }
}

/// Range within text with start and end (exclusive) line/column pairs.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct CursorRange {
    pub start: CursorPosition,
    pub end: CursorPosition,
}

impl CursorRange {
    #[allow(dead_code)]
    fn locate(&self, text: &str) -> Option<ByteRange> {
        let start = self.start.locate(text)?;
        let end = self.end.locate(text)?;
        Some(ByteRange::new(start, end))
    }
    pub fn lines(&self) -> u32 {
        (self.end.line - self.start.line) + 1
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLines {
    pub(crate) newline_offsets: Vec<usize>,
}

impl SourceLines {
    pub fn new(source: &str) -> Self {
        let iter = source.match_indices('\n');
        let num_lines = match iter.size_hint() {
            (_, Some(high)) => high + 1,
            (low, None) => low + 1,
        };
        let mut newline_offsets = Vec::with_capacity(num_lines);
        newline_offsets.push(0);
        newline_offsets.extend(iter.map(|(idx, _)| idx + 1));
        Self { newline_offsets }
    }

    pub fn cursor_pos(&self, offset: usize) -> CursorPosition {
        let res = self.newline_offsets.binary_search(&offset);
        match res {
            Ok(0) => CursorPosition {
                line: 0,
                column: usize::from(offset) as u32,
            },
            Ok(i) => CursorPosition {
                line: i as u32,
                column: 0,
            },
            Err(0) => CursorPosition {
                line: 0,
                column: usize::from(offset) as u32,
            },
            Err(i) => CursorPosition {
                line: i as u32 - 1,
                column: usize::from(offset - self.newline_offsets[i - 1]) as u32,
            },
        }
    }

    pub fn position_info(&self, offset: usize) -> PosInfo {
        PosInfo {
            bytes: offset,
            cursor: self.cursor_pos(offset),
        }
    }

    pub fn cursor_range(&self, range: ByteRange) -> CursorRange {
        CursorRange {
            start: self.cursor_pos(range.start),
            end: self.cursor_pos(range.end),
        }
    }

    pub fn range_info(&self, range: ByteRange) -> RangeInfo {
        RangeInfo {
            bytes: range,
            cursor: self.cursor_range(range),
        }
    }

    pub fn text_range(&self, range: CursorRange) -> ByteRange {
        let start = self.text_offset(range.start);
        let end = self.text_offset(range.end);
        ByteRange::new(start, end)
    }

    pub fn text_offset(&self, line_column: CursorPosition) -> usize {
        let line_offset = self.newline_offsets[line_column.line as usize];
        line_offset + (line_column.column as usize)
    }
}

pub fn line_range(src: &str, range: ByteRange) -> ByteRange {
    let mut line_range = range;

    while line_range.start() != 0 {
        if src.is_char_boundary(line_range.start()) && src[line_range.start()..].starts_with('\n') {
            line_range = ByteRange::new(line_range.start() + 1, line_range.end());
            break;
        }
        line_range = ByteRange::new(line_range.start() - 1, line_range.end());
    }

    while line_range.end() <= src.len() {
        if src.is_char_boundary(line_range.end()) {
            let slice = &src[ByteRange::new(0, line_range.end())];
            if slice.ends_with('\n') {
                line_range = ByteRange::new(line_range.start(), line_range.end() - 1);
                break;
            } else if slice.len() == src.len() {
                break;
            }
        }

        line_range = ByteRange::new(line_range.start(), line_range.end() + 1);
    }

    line_range
}

pub fn offset_range(src: &str, offset: usize) -> ByteRange {
    let mut range = ByteRange::new(offset, offset + 1);
    while range.end <= src.len() {
        if src.is_char_boundary(range.end) {
            break;
        } else {
            range.end += 1;
        }
    }
    range
}
