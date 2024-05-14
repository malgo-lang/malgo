use std::{fmt::Display, str::FromStr};

use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize)]
pub struct Name<T> {
    pub meta: T,
    pub id: Id,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Serialize, Deserialize)]
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize)]
pub struct Id {
    pub name: String,
    pub module_name: ModuleName,
    pub sort: Sort,
}

impl Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.sort {
            Sort::External => write!(f, "{}.{}", self.module_name, self.name),
            Sort::Internal => write!(f, "{}", self.name),
            Sort::Temporal => write!(f, "${}", self.name),
            Sort::Native => write!(f, "{}", self.name),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize)]
#[serde(tag = "tag")]
pub enum ModuleName {
    ModuleName { name: String },
    Artifact { path: ArtifactPath },
}

impl Display for ModuleName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModuleName::ModuleName { name } => write!(f, "{}", name),
            ModuleName::Artifact { path } => write!(f, "{}", path),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Serialize, Deserialize)]
pub struct ArtifactPath {
    pub raw_path: String,
    pub rel_path: String,
}

impl Display for ArtifactPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.rel_path)
    }
}
