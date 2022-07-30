use regex::Regex;

/// ```
/// use malgo::parser::lexer::lexing_rules::is_ident_start;
/// assert!(is_ident_start('a'));
/// assert!(is_ident_start('_'));
/// assert!(is_ident_start('λ'));
/// ```
pub fn is_ident_start(c: char) -> bool {
    let regex = Regex::new(r"[\p{XID_Start}_]").unwrap();
    regex.is_match(&c.to_string())
}

/// ```
/// use malgo::parser::lexer::lexing_rules::is_ident_continue;
/// assert!(is_ident_continue('a'));
/// assert!(is_ident_continue('_'));
/// assert!(is_ident_continue('λ'));
/// assert!(is_ident_continue('0'));
/// ```
pub fn is_ident_continue(c: char) -> bool {
    let regex = Regex::new(r"\p{XID_Continue}").unwrap();
    regex.is_match(&c.to_string())
}

/// ```
/// use malgo::parser::lexer::lexing_rules::is_special;
/// for c in "(),;[]{}`".chars() {
///     assert!(is_special(c));
/// }
/// ```
pub fn is_special(c: char) -> bool {
    let special_chars = "(),;[]{}`";
    special_chars.chars().any(|s| s == c)
}

/// ```
/// use malgo::parser::lexer::lexing_rules::is_symbol;
/// assert!(is_symbol('+'));
/// assert!(is_symbol('（'));
/// ```
pub fn is_symbol(c: char) -> bool {
    let regex = Regex::new(r"\p{Punctuation}|\p{Symbol}").unwrap();
    regex.is_match(&c.to_string())
}
