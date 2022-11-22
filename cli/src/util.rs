use crate::types::Perspective;
use anyhow::{anyhow, Result};

pub const DATETIME_FORMAT: &str = "%Y-%m-%dT%H:%M:%S%.fZ";

pub fn maybe_parse_datetime(
    maybe_date: Option<String>,
) -> Result<Option<chrono::naive::NaiveDateTime>> {
    Ok(
        match maybe_date
            .clone()
            .map(|s| chrono::naive::NaiveDateTime::parse_from_str(&s, DATETIME_FORMAT))
        {
            Some(Err(e)) => {
                return Err(anyhow!(e).context(format!(
                    "Couldn't parse datetime '{}' with expected format: {}",
                    maybe_date.unwrap(),
                    DATETIME_FORMAT
                )));
            }
            Some(Ok(x)) => Some(x),
            None => None,
        },
    )
}

pub fn readline_masked(prompt: &str) -> Result<String> {
    use rustyline::completion::Completer;
    use rustyline::config::Configurer;
    use rustyline::highlight::Highlighter;
    use rustyline::hint::Hinter;
    use rustyline::validate::Validator;
    use rustyline::{ColorMode, Editor, Helper};
    use std::borrow::Cow::{self, Borrowed, Owned};

    struct MaskingHighlighter {
        masking: bool,
    }

    impl Highlighter for MaskingHighlighter {
        fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
            use unicode_width::UnicodeWidthStr;
            if self.masking {
                Owned("*".repeat(line.width()))
            } else {
                Borrowed(line)
            }
        }

        fn highlight_char(&self, _line: &str, _pos: usize) -> bool {
            self.masking
        }
    }

    impl Completer for MaskingHighlighter {
        type Candidate = String;
    }
    impl Hinter for MaskingHighlighter {
        type Hint = String;
    }
    impl Validator for MaskingHighlighter {}
    impl Helper for MaskingHighlighter {}

    let mut rl = Editor::new()?;
    let h = MaskingHighlighter { masking: false };
    rl.set_helper(Some(h));
    rl.helper_mut().expect("No helper").masking = true;
    rl.set_color_mode(ColorMode::Forced); // force masking
    rl.set_auto_add_history(false); // make sure password is not added to history
    let passwd = rl.readline(prompt)?;
    Ok(passwd)
}

pub async fn string_2_perspective_snapshot(
    cap_token: String,
    string: String,
) -> Result<Perspective> {
    use rand::distributions::Alphanumeric;
    use rand::{thread_rng, Rng};

    let rand_name: String = thread_rng()
        .sample_iter(&Alphanumeric)
        .take(30)
        .map(char::from)
        .collect();
    let temp_perspective = ad4m_client::perspectives::run_add(cap_token.clone(), rand_name).await?;
    println!("Created temporary perspective: {}", temp_perspective);
    ad4m_client::perspectives::run_add_link(
        cap_token.clone(),
        temp_perspective.clone(),
        "ad4m://self".to_string(),
        format!("literal://string:{}", urlencoding::encode(&string)),
        None,
    )
    .await?;
    println!("Added status link to temporary perspective");

    let snapshot =
        ad4m_client::perspectives::run_snapshot(cap_token.clone(), temp_perspective.clone())
            .await?;
    println!("Created snapshot of temporary perspective");

    ad4m_client::perspectives::run_remove(cap_token.clone(), temp_perspective).await?;

    Ok(snapshot)
}
