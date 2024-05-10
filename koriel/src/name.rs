use serde::{Deserialize, Serialize};
use unicode_xid::UnicodeXID;

#[derive(Debug)]
pub struct Name {
    pub name: String,
    pub path: String,
    pub sort: String,
}

impl<'de> Deserialize<'de> for Name {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s: String = Deserialize::deserialize(deserializer)?;
        let prefix_removed = s.trim_start_matches("_M");
        let messages = split_by_run_length(prefix_removed);
        let replaced: Vec<String> = messages.into_iter().map(hex_to_char).collect();

        assert_eq!(replaced.len(), 3);

        Ok(Name {
            name: replaced[0].clone(),
            path: replaced[1].clone(),
            sort: replaced[2].clone(),
        })
    }
}

/// Convert a hex in a string to a char
/// Hex is represented as:
/// - "_xXX"
/// - "_uXXXX"
/// - "_UXXXXXXXX"
fn hex_to_char(s: String) -> String {
    let mut result = String::new();
    let mut iter = s.chars();

    while let Some(c) = iter.next() {
        if c == '_' {
            if let Some(next) = iter.next() {
                match next {
                    'x' => {
                        let hex = iter.by_ref().take(2).collect::<String>();
                        let char = char::from_u32(u32::from_str_radix(&hex, 16).unwrap()).unwrap();
                        result.push(char);
                    }
                    'u' => {
                        let hex = iter.by_ref().take(4).collect::<String>();
                        let char = char::from_u32(u32::from_str_radix(&hex, 16).unwrap()).unwrap();
                        result.push(char);
                    }
                    'U' => {
                        let hex = iter.by_ref().take(8).collect::<String>();
                        let char = char::from_u32(u32::from_str_radix(&hex, 16).unwrap()).unwrap();
                        result.push(char);
                    }
                    _ => {
                        result.push(next);
                    }
                }
            }
        } else {
            result.push(c);
        }
    }

    result
}

fn split_by_run_length(s: &str) -> Vec<String> {
    let mut messages = Vec::new();
    let mut current = String::new();
    let mut count = 0;

    let mut iter = s.chars();

    while let Some(c) = iter.next() {
        if c.is_digit(10) {
            count = count * 10 + c.to_digit(10).unwrap();
        } else {
            current.push(c);
            for _ in 1..count {
                if let Some(next) = iter.next() {
                    current.push(next);
                } else {
                    panic!("Invalid input: {} => {}", s, current);
                }
            }
            count = 0;
            messages.push(current);
            current = String::new();
        }
    }

    messages
}

impl Serialize for Name {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let msgs = vec![self.name.clone(), self.path.clone(), self.sort.clone()];
        let underscore_replaced = msgs
            .iter()
            .map(|c| {
                let mut result = String::new();
                for ch in c.chars() {
                    match ch {
                        '_' => result.push_str("_x5F"),
                        _ => result.push(ch),
                    }
                }
                result
            })
            .collect::<Vec<String>>();
        let other_replaced = underscore_replaced
            .iter()
            .map(char_to_hex)
            .collect::<Vec<String>>();
        let joined = join_by_run_length(other_replaced);

        let prefix_added = format!("_M{}", joined);
        prefix_added.serialize(serializer)
    }
}

fn join_by_run_length(msgs: Vec<String>) -> String {
    let mut result = String::new();

    for msg in msgs {
        let len_msg = msg.len().to_string();
        result.push_str(&len_msg);
        result.push_str(&msg);
    }

    result
}

/// Convert a no-ID_Continue char to hex in a string.
fn char_to_hex(s: &String) -> String {
    let mut result = String::new();
    for c in s.chars() {
        match c {
            _ if c.is_xid_continue() => result.push(c),
            _ if c as u32 <= 0xFF => {
                result.push('_');
                result.push('x');
                result.push_str(&format!("{:02X}", c as u32));
            }
            _ if c as u32 <= 0xFFFF => {
                result.push('_');
                result.push('u');
                result.push_str(&format!("{:04X}", c as u32));
            }
            _ => {
                result.push('_');
                result.push('U');
                result.push_str(&format!("{:08X}", c as u32));
            }
        }
    }

    result
}
