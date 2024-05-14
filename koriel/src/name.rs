use std::{fmt::Display, str::FromStr};

use serde::{Deserialize, Serialize};
use unicode_xid::UnicodeXID;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Sort {
    External,
    Internal,
    Temporal,
    Native,
}

impl ToString for Sort {
    fn to_string(&self) -> String {
        match self {
            Sort::External => "External".to_string(),
            Sort::Internal => "Internal".to_string(),
            Sort::Temporal => "Temporal".to_string(),
            Sort::Native => "Native".to_string(),
        }
    }
}

impl FromStr for Sort {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "External" {
            Ok(Sort::External)
        } else if s == "Native" {
            Ok(Sort::Native)
        } else if s == "Internal" {
            Ok(Sort::Internal)
        } else if s == "Temporal" {
            Ok(Sort::Temporal)
        } else {
            Err(format!("Invalid sort: {}", s))
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Name {
    pub name: String,
    pub path: String,
    pub sort: Sort,
}

pub fn native_name(name: &str) -> Name {
    Name {
        name: name.to_string(),
        path: "".to_string(),
        sort: Sort::Native,
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.sort {
            Sort::External => write!(f, "{}.{}", self.path, self.name),
            Sort::Internal => write!(f, "{}", self.name),
            Sort::Temporal => write!(f, "${}", self.name),
            Sort::Native => write!(f, "{}", self.name),
        }
    }
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

        let sort = Sort::from_str(&replaced[2]).unwrap();
        Ok(Name {
            name: replaced[0].clone(),
            path: replaced[1].clone(),
            sort,
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
        let sort = self.sort.to_string();
        let msgs = vec![self.name.clone(), self.path.clone(), sort];
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
