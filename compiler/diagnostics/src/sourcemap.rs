//! Source Map
//!
//! Maps byte positions to line/column information for error reporting.

use std::collections::HashMap;

/// A unique identifier for a source file
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct FileId(pub usize);

/// A source file with its content and line information
#[derive(Debug, Clone)]
pub struct SourceFile {
    /// The file name (path)
    pub name: String,
    /// The file content
    pub content: String,
    /// Byte positions where each line starts
    pub line_starts: Vec<usize>,
}

impl SourceFile {
    /// Create a new source file
    pub fn new(name: impl Into<String>, content: impl Into<String>) -> Self {
        let name = name.into();
        let content = content.into();
        let line_starts = Self::compute_line_starts(&content);

        Self {
            name,
            content,
            line_starts,
        }
    }

    /// Compute the byte positions where each line starts
    fn compute_line_starts(content: &str) -> Vec<usize> {
        let mut starts = vec![0];
        for (i, c) in content.char_indices() {
            if c == '\n' {
                starts.push(i + 1);
            }
        }
        starts
    }

    /// Get the number of lines in the file
    pub fn line_count(&self) -> usize {
        self.line_starts.len()
    }

    /// Convert a byte position to a line number (1-indexed)
    pub fn byte_to_line(&self, byte_pos: usize) -> usize {
        match self.line_starts.binary_search(&byte_pos) {
            Ok(idx) => idx + 1,
            Err(idx) => idx,
        }
    }

    /// Convert a byte position to a (line, column) pair (both 1-indexed)
    pub fn byte_to_position(&self, byte_pos: usize) -> (usize, usize) {
        let line = self.byte_to_line(byte_pos);
        let line_start = if line > 0 {
            self.line_starts.get(line - 1).copied().unwrap_or(0)
        } else {
            0
        };

        // Calculate column by counting characters, not bytes
        let line_text = &self.content[line_start..byte_pos.min(self.content.len())];
        let column = line_text.chars().count() + 1;

        (line, column)
    }

    /// Get the text of a specific line (1-indexed)
    pub fn get_line(&self, line: usize) -> Option<&str> {
        if line == 0 || line > self.line_starts.len() {
            return None;
        }

        let start = self.line_starts[line - 1];
        let end = self
            .line_starts
            .get(line)
            .copied()
            .unwrap_or(self.content.len());

        // Trim trailing newline if present (handles both \n and \r\n)
        let line_text = &self.content[start..end.min(self.content.len())];
        if let Some(stripped) = line_text.strip_suffix("\r\n") {
            Some(stripped)
        } else if let Some(stripped) = line_text.strip_suffix('\n') {
            Some(stripped)
        } else if let Some(stripped) = line_text.strip_suffix('\r') {
            Some(stripped)
        } else {
            Some(line_text)
        }
    }

    /// Get the byte range for a specific line (1-indexed)
    pub fn get_line_range(&self, line: usize) -> Option<(usize, usize)> {
        if line == 0 || line > self.line_starts.len() {
            return None;
        }

        let start = self.line_starts[line - 1];
        let end = self
            .line_starts
            .get(line)
            .copied()
            .unwrap_or(self.content.len());

        Some((start, end))
    }

    /// Get the name of the file
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get the content of the file
    pub fn content(&self) -> &str {
        &self.content
    }
}

/// Maps byte positions to line/column across multiple source files
#[derive(Debug, Clone, Default)]
pub struct SourceMap {
    files: Vec<SourceFile>,
    name_to_id: HashMap<String, FileId>,
}

impl SourceMap {
    /// Create a new empty source map
    pub fn new() -> Self {
        Self {
            files: Vec::new(),
            name_to_id: HashMap::new(),
        }
    }

    /// Add a file to the source map
    pub fn add_file(&mut self, name: impl Into<String>, content: impl Into<String>) -> FileId {
        let name = name.into();
        let file = SourceFile::new(&name, content);
        let id = FileId(self.files.len());

        self.name_to_id.insert(name, id);
        self.files.push(file);

        id
    }

    /// Get a file by its ID
    pub fn get_file(&self, file: FileId) -> Option<&SourceFile> {
        self.files.get(file.0)
    }

    /// Get a file by its name
    pub fn get_file_by_name(&self, name: &str) -> Option<(FileId, &SourceFile)> {
        self.name_to_id
            .get(name)
            .and_then(|id| self.files.get(id.0).map(|f| (*id, f)))
    }

    /// Get the file name for a file ID
    pub fn get_file_name(&self, file: FileId) -> Option<&str> {
        self.files.get(file.0).map(|f| f.name())
    }

    /// Look up the (line, column) for a byte position in a file
    pub fn lookup_position(&self, file: FileId, byte_pos: usize) -> Option<(usize, usize)> {
        self.files.get(file.0).map(|f| f.byte_to_position(byte_pos))
    }

    /// Look up the line number for a byte position in a file
    pub fn lookup_line(&self, file: FileId, byte_pos: usize) -> Option<usize> {
        self.files.get(file.0).map(|f| f.byte_to_line(byte_pos))
    }

    /// Get the text of a specific line in a file
    pub fn get_line(&self, file: FileId, line: usize) -> Option<&str> {
        self.files.get(file.0).and_then(|f| f.get_line(line))
    }

    /// Get the number of files in the source map
    pub fn file_count(&self) -> usize {
        self.files.len()
    }

    /// Get all files
    pub fn files(&self) -> &[SourceFile] {
        &self.files
    }

    /// Create a source map from a single file (convenience method)
    pub fn from_single_file(name: impl Into<String>, content: impl Into<String>) -> (Self, FileId) {
        let mut map = Self::new();
        let id = map.add_file(name, content);
        (map, id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_file_line_starts() {
        let file = SourceFile::new("test.jet", "line1\nline2\nline3");
        assert_eq!(file.line_starts, vec![0, 6, 12]);
        assert_eq!(file.line_count(), 3);
    }

    #[test]
    fn test_source_file_empty() {
        let file = SourceFile::new("test.jet", "");
        assert_eq!(file.line_starts, vec![0]);
        assert_eq!(file.line_count(), 1);
    }

    #[test]
    fn test_source_file_no_trailing_newline() {
        let file = SourceFile::new("test.jet", "line1\nline2");
        assert_eq!(file.line_starts, vec![0, 6]);
        assert_eq!(file.line_count(), 2);
    }

    #[test]
    fn test_byte_to_line() {
        let file = SourceFile::new("test.jet", "line1\nline2\nline3");
        assert_eq!(file.byte_to_line(0), 1);
        assert_eq!(file.byte_to_line(5), 1);
        assert_eq!(file.byte_to_line(6), 2);
        assert_eq!(file.byte_to_line(10), 2);
        assert_eq!(file.byte_to_line(12), 3);
    }

    #[test]
    fn test_byte_to_position() {
        let file = SourceFile::new("test.jet", "line1\nline2\nline3");
        assert_eq!(file.byte_to_position(0), (1, 1));
        assert_eq!(file.byte_to_position(5), (1, 6));
        assert_eq!(file.byte_to_position(6), (2, 1));
        assert_eq!(file.byte_to_position(11), (2, 6));
    }

    #[test]
    fn test_byte_to_position_multibyte() {
        // Test with multibyte UTF-8 characters
        let file = SourceFile::new("test.jet", "αβγ\nδεζ");
        // α is 2 bytes, β is 2 bytes, γ is 2 bytes
        assert_eq!(file.byte_to_position(0), (1, 1)); // α
        assert_eq!(file.byte_to_position(2), (1, 2)); // β
        assert_eq!(file.byte_to_position(4), (1, 3)); // γ
        assert_eq!(file.byte_to_position(6), (1, 4)); // \n
        assert_eq!(file.byte_to_position(7), (2, 1)); // δ
    }

    #[test]
    fn test_get_line() {
        let file = SourceFile::new("test.jet", "line1\nline2\nline3");
        assert_eq!(file.get_line(1), Some("line1"));
        assert_eq!(file.get_line(2), Some("line2"));
        assert_eq!(file.get_line(3), Some("line3"));
        assert_eq!(file.get_line(4), None);
        assert_eq!(file.get_line(0), None);
    }

    #[test]
    fn test_get_line_with_crlf() {
        let file = SourceFile::new("test.jet", "line1\r\nline2\r\n");
        assert_eq!(file.get_line(1), Some("line1"));
        assert_eq!(file.get_line(2), Some("line2"));
    }

    #[test]
    fn test_source_map_add_file() {
        let mut map = SourceMap::new();
        let id1 = map.add_file("file1.jet", "content1");
        let id2 = map.add_file("file2.jet", "content2");

        assert_eq!(id1, FileId(0));
        assert_eq!(id2, FileId(1));
        assert_eq!(map.file_count(), 2);
    }

    #[test]
    fn test_source_map_get_file() {
        let mut map = SourceMap::new();
        let id = map.add_file("test.jet", "content");

        let file = map.get_file(id).unwrap();
        assert_eq!(file.name(), "test.jet");
        assert_eq!(file.content(), "content");
    }

    #[test]
    fn test_source_map_lookup_position() {
        let mut map = SourceMap::new();
        let id = map.add_file("test.jet", "line1\nline2\nline3");

        assert_eq!(map.lookup_position(id, 0), Some((1, 1)));
        assert_eq!(map.lookup_position(id, 6), Some((2, 1)));
        assert_eq!(map.lookup_position(id, 12), Some((3, 1)));
    }

    #[test]
    fn test_source_map_get_line() {
        let mut map = SourceMap::new();
        let id = map.add_file("test.jet", "line1\nline2\nline3");

        assert_eq!(map.get_line(id, 1), Some("line1"));
        assert_eq!(map.get_line(id, 2), Some("line2"));
        assert_eq!(map.get_line(id, 3), Some("line3"));
    }

    #[test]
    fn test_source_map_from_single_file() {
        let (map, id) = SourceMap::from_single_file("test.jet", "content");
        assert_eq!(map.file_count(), 1);
        assert_eq!(id, FileId(0));
        assert_eq!(map.get_file_name(id), Some("test.jet"));
    }

    #[test]
    fn test_get_file_by_name() {
        let mut map = SourceMap::new();
        let id = map.add_file("test.jet", "content");

        let (found_id, file) = map.get_file_by_name("test.jet").unwrap();
        assert_eq!(found_id, id);
        assert_eq!(file.content(), "content");
        assert!(map.get_file_by_name("nonexistent.jet").is_none());
    }

    #[test]
    fn test_get_line_range() {
        let file = SourceFile::new("test.jet", "line1\nline2\nline3");
        assert_eq!(file.get_line_range(1), Some((0, 6)));
        assert_eq!(file.get_line_range(2), Some((6, 12)));
        assert_eq!(file.get_line_range(3), Some((12, 17)));
        assert_eq!(file.get_line_range(4), None);
    }
}
