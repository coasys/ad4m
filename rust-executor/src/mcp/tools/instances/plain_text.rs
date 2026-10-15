//! HTML to plain text for an LLM (#1446).
//!
//! `TextContent.text` may hold HTML (Flux and We store message bodies as
//! editor HTML) or Markdown. A transcript is read by a model, so markup is
//! noise: tags are dropped, block elements become line breaks, list items
//! become `- ` lines and entities are decoded. Markdown is left as it is: it
//! is already plain text a model reads well.
//!
//! Deliberately small and without dependencies: this is not an HTML parser,
//! only enough to read editor output. Only the HTML elements listed here are
//! tags; any other `<…>` is text, so a Markdown autolink
//! (`<https://example.org>`), a generic (`Vec<String>`) or `a < b` stays as
//! written. Text with no such tag is returned unchanged. Line and paragraph
//! breaks in the text are kept; other runs of whitespace collapse to one
//! space, as a browser shows them.

/// Elements whose start and end separate paragraphs.
const PARAGRAPH: &[&str] = &[
    "p",
    "div",
    "section",
    "article",
    "header",
    "footer",
    "blockquote",
    "pre",
    "ul",
    "ol",
    "table",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "hr",
];

/// Elements whose start and end separate lines.
const LINE: &[&str] = &["br", "li", "tr"];

/// Elements whose content is not text.
const SKIPPED: &[&str] = &["script", "style", "head", "template"];

/// Elements that only mark up text: dropped, their content kept.
const INLINE: &[&str] = &[
    "a",
    "abbr",
    "b",
    "body",
    "caption",
    "cite",
    "code",
    "dd",
    "del",
    "details",
    "dl",
    "dt",
    "em",
    "figcaption",
    "figure",
    "font",
    "html",
    "i",
    "img",
    "ins",
    "kbd",
    "label",
    "main",
    "mark",
    "nav",
    "q",
    "s",
    "samp",
    "small",
    "span",
    "strike",
    "strong",
    "sub",
    "summary",
    "sup",
    "tbody",
    "td",
    "tfoot",
    "th",
    "thead",
    "time",
    "u",
    "var",
];

/// Whether `name` is an element this converter reads as a tag.
fn is_element(name: &str) -> bool {
    [PARAGRAPH, LINE, SKIPPED, INLINE]
        .iter()
        .any(|names| names.contains(&name))
}

/// `input` as plain text: HTML converted, anything else unchanged.
pub(crate) fn to_plain_text(input: &str) -> String {
    if !looks_like_html(input) {
        return input.to_string();
    }
    let mut out = Text::default();
    let mut rest = input;
    let mut skipping: Option<String> = None;
    while let Some(lt) = rest.find('<') {
        let (text, after) = rest.split_at(lt);
        if skipping.is_none() {
            out.push_text(&decode_entities(text));
        }
        let Some(tag) = Tag::parse(after) else {
            // A `<` that opens no tag is text.
            if skipping.is_none() {
                out.push_text("<");
            }
            rest = &after[1..];
            continue;
        };
        rest = &after[tag.len..];
        if let Some(skipped) = &skipping {
            if tag.closing && tag.name == *skipped {
                skipping = None;
            }
            continue;
        }
        if SKIPPED.contains(&tag.name.as_str()) && !tag.closing {
            skipping = Some(tag.name.clone());
            continue;
        }
        if tag.name == "pre" {
            out.preformatted = !tag.closing;
        }
        if PARAGRAPH.contains(&tag.name.as_str()) {
            out.break_paragraph();
        } else if LINE.contains(&tag.name.as_str()) {
            out.break_line();
            if tag.name == "li" && !tag.closing {
                out.push_text("- ");
            }
        }
    }
    if skipping.is_none() {
        out.push_text(&decode_entities(rest));
    }
    out.finish()
}

/// At least one tag of a known element, or a comment.
fn looks_like_html(s: &str) -> bool {
    s.match_indices('<')
        .any(|(i, _)| Tag::parse(&s[i..]).is_some())
}

struct Tag {
    /// Lowercased; empty for a comment or doctype.
    name: String,
    closing: bool,
    /// Bytes from the `<` to after the `>`.
    len: usize,
}

impl Tag {
    /// The tag at the start of `s` (which starts with `<`), or `None` when
    /// it opens no tag of a known element: `<https://…>` and `Vec<String>`
    /// are text.
    fn parse(s: &str) -> Option<Tag> {
        if let Some(comment) = s.strip_prefix("<!--") {
            let end = comment.find("-->")? + "<!--".len() + "-->".len();
            return Some(Tag {
                name: String::new(),
                closing: false,
                len: end,
            });
        }
        let body = &s[1..];
        let closing = body.starts_with('/');
        let name_start = if closing { &body[1..] } else { body };
        let name: String = name_start
            .chars()
            .take_while(|c| c.is_ascii_alphanumeric())
            .collect::<String>()
            .to_ascii_lowercase();
        // The name ends at whitespace, `/` or `>`: `<a href=…>`, `<br/>`.
        // `<!DOCTYPE …>` is a tag without a name.
        let after_name = name_start[name.len()..].chars().next();
        let doctype = name_start.starts_with('!');
        if !doctype
            && (!is_element(&name)
                || !after_name.is_some_and(|c| c.is_ascii_whitespace() || c == '/' || c == '>'))
        {
            return None;
        }
        // The `>` that ends the tag, outside quoted attribute values.
        let mut quote: Option<char> = None;
        let mut end = None;
        for (i, c) in body.char_indices() {
            match (quote, c) {
                (Some(q), c) if c == q => quote = None,
                (Some(_), _) => {}
                (None, '"' | '\'') => quote = Some(c),
                (None, '>') => {
                    end = Some(i);
                    break;
                }
                _ => {}
            }
        }
        let end = end?;
        Some(Tag {
            name,
            closing,
            len: 1 + end + 1,
        })
    }
}

/// Plain text being built: runs of whitespace collapse to one space, except
/// inside `<pre>`. A newline in the text is kept as a line break, two or more
/// as a paragraph break: Markdown mixed with a tag keeps its lines.
#[derive(Default)]
struct Text {
    out: String,
    pending_space: bool,
    /// Newlines in the current run of whitespace.
    pending_newlines: usize,
    preformatted: bool,
}

impl Text {
    fn push_text(&mut self, text: &str) {
        for c in text.chars() {
            if self.preformatted {
                self.out.push(c);
            } else if c == '\n' {
                self.pending_newlines += 1;
            } else if c.is_whitespace() {
                self.pending_space = true;
            } else {
                match self.pending_newlines {
                    0 => {
                        if self.pending_space && !self.out.is_empty() && !self.out.ends_with('\n') {
                            self.out.push(' ');
                        }
                    }
                    1 => self.break_line(),
                    _ => self.break_paragraph(),
                }
                self.pending_space = false;
                self.pending_newlines = 0;
                self.out.push(c);
            }
        }
    }

    fn break_line(&mut self) {
        self.trim_end_spaces();
        if !self.out.is_empty() && !self.out.ends_with('\n') {
            self.out.push('\n');
        }
        self.pending_space = false;
        self.pending_newlines = 0;
    }

    fn break_paragraph(&mut self) {
        self.break_line();
        if !self.out.is_empty() && !self.out.ends_with("\n\n") {
            self.out.push('\n');
        }
    }

    fn trim_end_spaces(&mut self) {
        let trimmed = self.out.trim_end_matches([' ', '\t']).len();
        self.out.truncate(trimmed);
    }

    fn finish(self) -> String {
        self.out.trim().to_string()
    }
}

/// Decode the entities editors write: the named ones that matter for text,
/// and numeric ones. An unknown entity is left as written.
fn decode_entities(s: &str) -> String {
    if !s.contains('&') {
        return s.to_string();
    }
    let mut out = String::with_capacity(s.len());
    let mut rest = s;
    while let Some(amp) = rest.find('&') {
        out.push_str(&rest[..amp]);
        let after = &rest[amp..];
        let decoded = after.find(';').filter(|&semi| semi <= 10).and_then(|semi| {
            let entity = &after[1..semi];
            let c = match entity {
                "amp" => Some('&'),
                "lt" => Some('<'),
                "gt" => Some('>'),
                "quot" => Some('"'),
                "apos" => Some('\''),
                "nbsp" => Some(' '),
                _ => {
                    let code = if let Some(hex) = entity
                        .strip_prefix("#x")
                        .or_else(|| entity.strip_prefix("#X"))
                    {
                        u32::from_str_radix(hex, 16).ok()
                    } else {
                        entity.strip_prefix('#').and_then(|d| d.parse().ok())
                    };
                    code.and_then(char::from_u32)
                }
            }?;
            Some((c, semi + 1))
        });
        match decoded {
            Some((c, len)) => {
                out.push(c);
                rest = &after[len..];
            }
            None => {
                out.push('&');
                rest = &after[1..];
            }
        }
    }
    out.push_str(rest);
    out
}

#[cfg(test)]
mod tests {
    use super::to_plain_text;

    #[test]
    fn a_flux_message_with_a_mention_reads_as_its_text() {
        let body = r#"<p>Hey <span data-type="mention" class="mention" data-id="did:key:z6MkData" data-label="data">@data</span>, can you look at <a href="https://example.org/x?a=1&amp;b=2">this</a>?</p>"#;
        assert_eq!(to_plain_text(body), "Hey @data, can you look at this?");
    }

    #[test]
    fn paragraphs_lines_and_lists_keep_their_breaks() {
        assert_eq!(
            to_plain_text("<p>first</p><p>second &amp; third</p>"),
            "first\n\nsecond & third"
        );
        assert_eq!(
            to_plain_text("line<br>break<br/>again"),
            "line\nbreak\nagain"
        );
        assert_eq!(
            to_plain_text(
                "<p>todo:</p><ul><li>one</li><li><strong>two</strong></li></ul><p>done</p>"
            ),
            "todo:\n\n- one\n- two\n\ndone"
        );
        assert_eq!(
            to_plain_text("<h2>Title</h2><div>body</div>"),
            "Title\n\nbody"
        );
    }

    #[test]
    fn whitespace_collapses_outside_pre_and_newlines_stay() {
        assert_eq!(
            to_plain_text("<p>  lots   of\tspace  </p>"),
            "lots of space"
        );
        assert_eq!(to_plain_text("<p>  one \n  two  </p>"), "one\ntwo");
        // Newlines between block tags add no extra break.
        assert_eq!(to_plain_text("<p>a</p>\n<p>b</p>\n"), "a\n\nb");
        assert_eq!(
            to_plain_text("<pre>fn main() {\n    x\n}</pre>"),
            "fn main() {\n    x\n}"
        );
    }

    #[test]
    fn scripts_styles_and_comments_are_dropped() {
        assert_eq!(
            to_plain_text("<style>p { color: red }</style><p>visible</p><script>alert('x')</script><!-- note -->"),
            "visible"
        );
        assert_eq!(to_plain_text("<!DOCTYPE html><p>page</p>"), "page");
    }

    #[test]
    fn entities_decode() {
        assert_eq!(
            to_plain_text("<p>it&#39;s &lt;b&gt; &quot;q&quot; &#x2603;&nbsp;&unknown; &</p>"),
            "it's <b> \"q\" \u{2603} &unknown; &"
        );
    }

    #[test]
    fn markdown_and_plain_text_are_unchanged() {
        for text in [
            "**bold** and `code`, 1 < 3 and 4 > 2",
            "# Heading\n\n- item\n- item",
            "plain words",
            "",
        ] {
            assert_eq!(to_plain_text(text), text);
        }
    }

    #[test]
    fn a_stray_less_than_inside_html_is_text() {
        assert_eq!(to_plain_text("<p>1 < 2</p>"), "1 < 2");
    }

    #[test]
    fn angle_brackets_that_are_not_html_tags_stay() {
        // No tag at all: unchanged.
        for text in [
            "See <https://example.org/x> for details",
            "Use Vec<String> here",
            "mail <data@example.org> or <a@b.c>",
            "multi\nline <https://example.org/x>\n\nnext paragraph",
        ] {
            assert_eq!(to_plain_text(text), text);
        }
        // Next to a real tag: only the tag goes.
        assert_eq!(
            to_plain_text("See <https://example.org/x> and <b>this</b>"),
            "See <https://example.org/x> and this"
        );
        assert_eq!(
            to_plain_text("<p>Use Vec<String> or Option<T></p>"),
            "Use Vec<String> or Option<T>"
        );
        // An element name must end the name: `<bold>` is not `<b>`.
        assert_eq!(to_plain_text("<bold>x</bold> <b>y</b>"), "<bold>x</bold> y");
    }

    #[test]
    fn markdown_with_one_tag_keeps_its_lines_and_paragraphs() {
        assert_eq!(
            to_plain_text("line one\nline two with <b>bold</b>\n\n- item\n- item"),
            "line one\nline two with bold\n\n- item\n- item"
        );
    }
}
