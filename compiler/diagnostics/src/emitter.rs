//! Diagnostic Emitter
//!
//! Renders diagnostics to various outputs with source highlighting,
//! color support, and suggestion display.

use crate::{Diagnostic, LabelStyle, Level, SourceMap, Span, Suggestion};
use std::io;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

/// Configuration for diagnostic emission
#[derive(Debug, Clone)]
pub struct EmitConfig {
    /// Whether to use colors in output
    pub colors: bool,
    /// Number of context lines to show around errors
    pub context_lines: usize,
    /// Maximum width of the output
    pub max_width: usize,
}

impl Default for EmitConfig {
    fn default() -> Self {
        Self {
            colors: true,
            context_lines: 2,
            max_width: 80,
        }
    }
}

/// Renders diagnostics to various outputs
pub struct Emitter {
    source_map: SourceMap,
    config: EmitConfig,
}

/// Color palette for diagnostic rendering
struct ColorPalette {
    error: ColorSpec,
    warning: ColorSpec,
    note: ColorSpec,
    help: ColorSpec,
    primary: ColorSpec,
    secondary: ColorSpec,
    code: ColorSpec,
    line_number: ColorSpec,
    source_border: ColorSpec,
}

impl ColorPalette {
    fn new() -> Self {
        let mut error = ColorSpec::new();
        error.set_fg(Some(Color::Red)).set_bold(true);

        let mut warning = ColorSpec::new();
        warning.set_fg(Some(Color::Yellow)).set_bold(true);

        let mut note = ColorSpec::new();
        note.set_fg(Some(Color::Blue)).set_bold(true);

        let mut help = ColorSpec::new();
        help.set_fg(Some(Color::Cyan)).set_bold(true);

        let mut primary = ColorSpec::new();
        primary.set_fg(Some(Color::Red));

        let mut secondary = ColorSpec::new();
        secondary.set_fg(Some(Color::Blue));

        let mut code = ColorSpec::new();
        code.set_fg(Some(Color::Magenta));

        let mut line_number = ColorSpec::new();
        line_number.set_fg(Some(Color::Blue));

        let mut source_border = ColorSpec::new();
        source_border.set_fg(Some(Color::Blue));

        Self {
            error,
            warning,
            note,
            help,
            primary,
            secondary,
            code,
            line_number,
            source_border,
        }
    }

    fn level_color(&self, level: Level) -> &ColorSpec {
        match level {
            Level::Error => &self.error,
            Level::Warning => &self.warning,
            Level::Note => &self.note,
            Level::Help => &self.help,
        }
    }
}

impl Emitter {
    /// Create a new emitter with the given source map
    pub fn new(source_map: SourceMap) -> Self {
        Self {
            source_map,
            config: EmitConfig::default(),
        }
    }

    /// Create a new emitter with custom configuration
    pub fn with_config(source_map: SourceMap, config: EmitConfig) -> Self {
        Self { source_map, config }
    }

    /// Emit a single diagnostic to stderr
    pub fn emit(&self, diagnostic: &Diagnostic) {
        let stderr = StandardStream::stderr(if self.config.colors {
            ColorChoice::Auto
        } else {
            ColorChoice::Never
        });
        let mut writer = stderr.lock();
        self.write_diagnostic(&mut writer, diagnostic)
            .expect("failed to write diagnostic");
    }

    /// Emit multiple diagnostics to stderr
    pub fn emit_many(&self, diagnostics: &[Diagnostic]) {
        for diagnostic in diagnostics {
            self.emit(diagnostic);
        }
    }

    /// Format a diagnostic as a string (for testing)
    pub fn format_diagnostic(&self, diagnostic: &Diagnostic) -> String {
        let mut buf = termcolor::Buffer::no_color();
        self.write_diagnostic(&mut buf, diagnostic)
            .expect("failed to format diagnostic");
        String::from_utf8(buf.into_inner()).expect("invalid UTF-8 in diagnostic output")
    }

    /// Write a diagnostic to a writer
    fn write_diagnostic(
        &self,
        writer: &mut dyn WriteColor,
        diagnostic: &Diagnostic,
    ) -> io::Result<()> {
        let colors = ColorPalette::new();

        // Write header: error[E0001]: message
        self.write_header(writer, diagnostic, &colors)?;

        // Write location: --> file:line:column
        self.write_location(writer, diagnostic, &colors)?;

        // Write source code with highlighting
        self.write_source(writer, diagnostic, &colors)?;

        // Write suggestions
        for suggestion in &diagnostic.suggestions {
            self.write_suggestion(writer, suggestion, diagnostic.span.file, &colors)?;
        }

        // Write notes
        for note in &diagnostic.notes {
            self.write_note(writer, note, &colors)?;
        }

        // Write footer separator
        writeln!(writer)?;

        Ok(())
    }

    /// Write the diagnostic header
    fn write_header(
        &self,
        writer: &mut dyn WriteColor,
        diagnostic: &Diagnostic,
        colors: &ColorPalette,
    ) -> io::Result<()> {
        writer.set_color(colors.level_color(diagnostic.level))?;
        write!(writer, "{}", diagnostic.level.as_str())?;
        writer.reset()?;

        if let Some(code) = &diagnostic.error_code {
            writer.set_color(&colors.code)?;
            write!(writer, "[{code}]")?;
            writer.reset()?;
        }

        writeln!(writer, ": {}", diagnostic.message)?;
        Ok(())
    }

    /// Write the location line
    fn write_location(
        &self,
        writer: &mut dyn WriteColor,
        diagnostic: &Diagnostic,
        colors: &ColorPalette,
    ) -> io::Result<()> {
        if let Some(file) = self.source_map.get_file(diagnostic.span.file) {
            let (line, column) = file.byte_to_position(diagnostic.span.start);
            writer.set_color(&colors.source_border)?;
            write!(writer, "  -->")?;
            writer.reset()?;
            writeln!(writer, " {}:{}:{}", file.name(), line, column)?;
        }
        Ok(())
    }

    /// Write source code with highlighting
    fn write_source(
        &self,
        writer: &mut dyn WriteColor,
        diagnostic: &Diagnostic,
        colors: &ColorPalette,
    ) -> io::Result<()> {
        let Some(file) = self.source_map.get_file(diagnostic.span.file) else {
            return Ok(());
        };

        // Collect all spans (primary + labels)
        let mut all_spans = vec![(diagnostic.span, LabelStyle::Primary, "")];
        for label in &diagnostic.labels {
            all_spans.push((label.span, label.style, label.message.as_str()));
        }

        // Find the line range we need to display
        let (start_line, _) =
            file.byte_to_position(all_spans.iter().map(|s| s.0.start).min().unwrap_or(0));
        let (end_line, _) = file.byte_to_position(
            all_spans
                .iter()
                .map(|s| s.0.end.saturating_sub(1))
                .max()
                .unwrap_or(0),
        );

        let context_start = start_line.saturating_sub(self.config.context_lines).max(1);
        let context_end = (end_line + self.config.context_lines).min(file.line_count());

        // Calculate gutter width based on the highest line number
        let gutter_width = context_end.to_string().len().max(2);

        // Write source border
        writer.set_color(&colors.source_border)?;
        write!(writer, "{}", " ".repeat(gutter_width + 1))?;
        writeln!(writer, "|")?;
        writer.reset()?;

        // Write each line
        for line_num in context_start..=context_end {
            self.write_source_line(writer, file, line_num, gutter_width, &all_spans, colors)?;
        }

        // Write source border
        writer.set_color(&colors.source_border)?;
        write!(writer, "{}", " ".repeat(gutter_width + 1))?;
        writeln!(writer, "|")?;
        writer.reset()?;

        Ok(())
    }

    /// Write a single source line with highlighting
    fn write_source_line(
        &self,
        writer: &mut dyn WriteColor,
        file: &crate::sourcemap::SourceFile,
        line_num: usize,
        gutter_width: usize,
        spans: &[(Span, LabelStyle, &str)],
        colors: &ColorPalette,
    ) -> io::Result<()> {
        // Write gutter with line number
        writer.set_color(&colors.line_number)?;
        write!(writer, "{:>width$} ", line_num, width = gutter_width)?;
        writer.reset()?;

        writer.set_color(&colors.source_border)?;
        write!(writer, "|")?;
        writer.reset()?;

        // Get the line content
        let Some(line_text) = file.get_line(line_num) else {
            writeln!(writer)?;
            return Ok(());
        };

        // Find spans that overlap with this line
        let line_start = file.line_starts.get(line_num - 1).copied().unwrap_or(0);
        let line_end = line_start + line_text.len();

        let line_spans: Vec<_> = spans
            .iter()
            .filter(|(span, _, _)| span.start < line_end && span.end > line_start)
            .copied()
            .collect();

        if line_spans.is_empty() {
            // No spans on this line, just print the text
            writeln!(writer, " {}", line_text)?;
        } else {
            // Print the line with highlighting
            self.write_highlighted_line(writer, line_text, line_start, &line_spans, colors)?;
        }

        // Write underline/annotation line if there are spans
        let spans_on_line: Vec<_> = spans
            .iter()
            .filter(|(span, _, _)| {
                let (span_line, _) = file.byte_to_position(span.start);
                span_line == line_num
            })
            .collect();

        if !spans_on_line.is_empty() {
            self.write_annotation_line(
                writer,
                file,
                line_num,
                gutter_width,
                &spans_on_line,
                colors,
            )?;
        }

        Ok(())
    }

    /// Write a line with highlighted regions
    fn write_highlighted_line(
        &self,
        writer: &mut dyn WriteColor,
        line_text: &str,
        line_start: usize,
        spans: &[(Span, LabelStyle, &str)],
        colors: &ColorPalette,
    ) -> io::Result<()> {
        write!(writer, " ")?;

        let chars = line_text.char_indices();
        let mut in_span = false;
        let mut current_style = LabelStyle::Primary;

        for (byte_idx, ch) in chars {
            let byte_pos = line_start + byte_idx;

            // Check if we're entering or leaving a span
            let was_in_span = in_span;
            in_span = false;

            for (span, style, _) in spans.iter() {
                if span.start <= byte_pos && byte_pos < span.end {
                    in_span = true;
                    current_style = *style;
                    break;
                }
            }

            // Update color if state changed
            if in_span != was_in_span || (in_span && was_in_span) {
                if in_span {
                    match current_style {
                        LabelStyle::Primary => writer.set_color(&colors.primary)?,
                        LabelStyle::Secondary => writer.set_color(&colors.secondary)?,
                    }
                } else {
                    writer.reset()?;
                }
            }

            write!(writer, "{}", ch)?;
        }

        writer.reset()?;
        writeln!(writer)?;
        Ok(())
    }

    /// Write the annotation line with underlines
    fn write_annotation_line(
        &self,
        writer: &mut dyn WriteColor,
        file: &crate::sourcemap::SourceFile,
        line_num: usize,
        gutter_width: usize,
        spans: &[&(Span, LabelStyle, &str)],
        colors: &ColorPalette,
    ) -> io::Result<()> {
        // Write gutter
        write!(writer, "{}", " ".repeat(gutter_width + 1))?;

        writer.set_color(&colors.source_border)?;
        write!(writer, "|")?;
        writer.reset()?;

        write!(writer, " ")?;

        let _line_start = file.line_starts.get(line_num - 1).copied().unwrap_or(0);
        let line_text = file.get_line(line_num).unwrap_or("");

        // Build underline string
        let mut underline = String::new();

        // Sort spans by start position
        let mut sorted_spans: Vec<_> = spans.to_vec();
        sorted_spans.sort_by_key(|(span, _, _)| span.start);

        // Determine the primary style for this line
        let primary_style = sorted_spans
            .iter()
            .find(|(_, style, _)| *style == LabelStyle::Primary)
            .map(|(_, _, _)| LabelStyle::Primary)
            .unwrap_or(LabelStyle::Secondary);

        for (span, _style, _message) in &sorted_spans {
            let (span_line, span_col) = file.byte_to_position(span.start);
            if span_line != line_num {
                continue;
            }

            let (_, end_col) = file.byte_to_position(span.end);

            // Calculate visual column (accounting for multibyte chars)
            let start_col = span_col;
            let visual_start = line_text
                .chars()
                .take(start_col.saturating_sub(1))
                .map(|c| c.width().unwrap_or(1))
                .sum::<usize>();

            // Pad to the start of this span
            while underline.len() < visual_start {
                underline.push(' ');
            }

            // Calculate visual width of the span
            let span_text = &line_text
                [start_col.saturating_sub(1)..end_col.saturating_sub(1).min(line_text.len())];
            let span_width = span_text.width();

            // Add underline characters
            for _ in 0..span_width.max(1) {
                underline.push('^');
            }
        }

        // Print the underline with appropriate color
        match primary_style {
            LabelStyle::Primary => writer.set_color(&colors.primary)?,
            LabelStyle::Secondary => writer.set_color(&colors.secondary)?,
        }

        write!(writer, "{}", underline)?;
        writer.reset()?;

        // Print the first label message if any
        for (span, style, message) in &sorted_spans {
            if !message.is_empty() {
                let (_, span_col) = file.byte_to_position(span.start);
                let start_col = span_col;
                let visual_start = line_text
                    .chars()
                    .take(start_col.saturating_sub(1))
                    .map(|c| c.width().unwrap_or(1))
                    .sum::<usize>();

                // Pad to position
                while underline.len() < visual_start {
                    underline.push(' ');
                }

                match style {
                    LabelStyle::Primary => writer.set_color(&colors.primary)?,
                    LabelStyle::Secondary => writer.set_color(&colors.secondary)?,
                }
                write!(writer, " {}", message)?;
                writer.reset()?;
                break; // Only show first message
            }
        }

        writeln!(writer)?;
        Ok(())
    }

    /// Write a suggestion
    fn write_suggestion(
        &self,
        writer: &mut dyn WriteColor,
        suggestion: &Suggestion,
        _file_id: crate::sourcemap::FileId,
        colors: &ColorPalette,
    ) -> io::Result<()> {
        writer.set_color(&colors.help)?;
        write!(writer, "help")?;
        writer.reset()?;
        writeln!(writer, ": {}", suggestion.message)?;

        // Show the replacement
        writeln!(writer, "  {}", suggestion.replacement)?;

        Ok(())
    }

    /// Write a note
    fn write_note(
        &self,
        writer: &mut dyn WriteColor,
        note: &str,
        colors: &ColorPalette,
    ) -> io::Result<()> {
        writer.set_color(&colors.note)?;
        write!(writer, "note")?;
        writer.reset()?;
        writeln!(writer, ": {}", note)?;
        Ok(())
    }

    /// Get the source map
    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
    }

    /// Get the emit config
    pub fn config(&self) -> &EmitConfig {
        &self.config
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Diagnostic, Label, Span, Suggestion};

    fn create_test_emitter(source: &str) -> (Emitter, crate::sourcemap::FileId) {
        let (source_map, file_id) = SourceMap::from_single_file("test.jet", source);
        let emitter = Emitter::new(source_map);
        (emitter, file_id)
    }

    #[test]
    fn test_format_simple_error() {
        let (emitter, file_id) = create_test_emitter("fn main():\n    let x = 5\n");

        let diagnostic = Diagnostic::error("unexpected token", Span::new_in_file(file_id, 0, 2));

        let output = emitter.format_diagnostic(&diagnostic);
        assert!(output.contains("error"));
        assert!(output.contains("unexpected token"));
        assert!(output.contains("test.jet"));
    }

    #[test]
    fn test_format_error_with_code() {
        use crate::ErrorCode;
        let (emitter, file_id) = create_test_emitter("fn main():\n    let x = 5\n");

        let diagnostic = Diagnostic::error("undefined variable", Span::new_in_file(file_id, 3, 7))
            .with_error_code(ErrorCode::UnresolvedName);

        let output = emitter.format_diagnostic(&diagnostic);
        assert!(output.contains("error[E0200]"));
        assert!(output.contains("undefined variable"));
    }

    #[test]
    fn test_format_warning() {
        let (emitter, file_id) = create_test_emitter("fn main():\n    let x = 5\n");

        let diagnostic = Diagnostic::warning("unused variable", Span::new_in_file(file_id, 18, 19));

        let output = emitter.format_diagnostic(&diagnostic);
        assert!(output.contains("warning"));
        assert!(output.contains("unused variable"));
    }

    #[test]
    fn test_format_with_label() {
        let (emitter, file_id) =
            create_test_emitter("fn add(x: int, y: int) -> int:\n    return x + z\n");

        let diagnostic =
            Diagnostic::error("undefined variable `z`", Span::new_in_file(file_id, 41, 42))
                .with_label(Label::secondary(
                    Span::new_in_file(file_id, 7, 8),
                    "did you mean `x`?",
                ));

        let output = emitter.format_diagnostic(&diagnostic);
        assert!(output.contains("undefined variable"));
        assert!(output.contains("did you mean"));
    }

    #[test]
    fn test_format_with_note() {
        let (emitter, file_id) = create_test_emitter("fn main():\n    let x = 5\n");

        let diagnostic = Diagnostic::error("type mismatch", Span::new_in_file(file_id, 0, 2))
            .with_note("expected `int`, found `string`");

        let output = emitter.format_diagnostic(&diagnostic);
        assert!(output.contains("note:"));
        assert!(output.contains("expected `int`"));
    }

    #[test]
    fn test_format_with_suggestion() {
        let (emitter, file_id) = create_test_emitter("fn main():\n    let x = 5\n");

        let diagnostic =
            Diagnostic::error("typo in variable name", Span::new_in_file(file_id, 18, 19))
                .with_suggestion(Suggestion::new(
                    "try this instead",
                    "y",
                    Span::new_in_file(file_id, 18, 19),
                ));

        let output = emitter.format_diagnostic(&diagnostic);
        assert!(output.contains("help:"));
        assert!(output.contains("try this instead"));
    }

    #[test]
    fn test_emit_config_default() {
        let config = EmitConfig::default();
        assert!(config.colors);
        assert_eq!(config.context_lines, 2);
        assert_eq!(config.max_width, 80);
    }

    #[test]
    fn test_emit_many() {
        let (emitter, file_id) = create_test_emitter("fn main():\n    let x = 5\n");

        let diagnostics = vec![
            Diagnostic::error("error 1", Span::new_in_file(file_id, 0, 2)),
            Diagnostic::warning("warning 1", Span::new_in_file(file_id, 3, 5)),
        ];

        // Should not panic
        emitter.emit_many(&diagnostics);
    }
}
