use json::JsonValue;
use log::warn;
use std::{borrow::Cow, fmt};

/// The result of a data source or text expansion
#[derive(Debug)]
pub enum Value<'a> {
    Borrow(&'a str),
    Owned(String),
    Float(f64),
    Bool(bool),
    /// This value is not available now, but is expected to be replaced soon
    NotReady,
    Empty,
    /// Use of this value should propagate errors
    Error,
}

#[cfg_attr(not(feature = "pulse"), allow(unused))]
impl<'a> Value<'a> {
    pub fn as_ref(&self) -> Value<'_> {
        match self {
            Value::Borrow(v) => Value::Borrow(v),
            Value::Owned(v) => Value::Borrow(&v[..]),
            Value::Float(f) => Value::Float(*f),
            Value::Bool(b) => Value::Bool(*b),
            Value::NotReady => Value::NotReady,
            Value::Empty => Value::Empty,
            Value::Error => Value::Error,
        }
    }

    pub fn as_str_fast(&self) -> &str {
        match self {
            Value::Borrow(v) => v,
            Value::Owned(v) => v,
            _ => "",
        }
    }

    pub fn into_owned(self) -> Value<'static> {
        match self {
            Value::Borrow(v) => Value::Owned(v.into()),
            Value::Owned(v) => Value::Owned(v),
            Value::Float(f) => Value::Float(f),
            Value::Bool(b) => Value::Bool(b),
            Value::NotReady => Value::NotReady,
            Value::Empty => Value::Empty,
            Value::Error => Value::Error,
        }
    }

    pub fn into_text(self) -> Cow<'a, str> {
        match self {
            Value::Borrow(v) => Cow::Borrowed(v),
            Value::Owned(v) => Cow::Owned(v),
            Value::Float(f) => format!("{}", f).into(),
            Value::Bool(true) => "1".into(),
            Value::Bool(false) => "0".into(),
            Value::NotReady => "".into(),
            Value::Empty => "".into(),
            Value::Error => "".into(),
        }
    }

    pub fn parse_f32(&self) -> Option<f32> {
        match self {
            Value::Borrow(v) => v.parse().ok(),
            Value::Owned(v) => v.parse().ok(),
            Value::Float(f) => Some(*f as f32),
            Value::Bool(true) => Some(1.0),
            Value::Bool(false) => Some(0.0),
            Value::NotReady => None,
            Value::Empty => None,
            Value::Error => None,
        }
    }

    pub fn parse_f64(&self) -> Option<f64> {
        match self {
            Value::Borrow(v) => v.parse().ok(),
            Value::Owned(v) => v.parse().ok(),
            Value::Float(f) => Some(*f),
            Value::Bool(true) => Some(1.0),
            Value::Bool(false) => Some(0.0),
            Value::NotReady => None,
            Value::Empty => None,
            Value::Error => None,
        }
    }

    pub fn parse_bool(&self) -> Option<bool> {
        match self.as_ref() {
            Value::Borrow(v) if v == "1" => Some(true),
            Value::Borrow(v) if v == "0" => Some(false),
            Value::Borrow(v) if v == "true" => Some(true),
            Value::Borrow(v) if v == "false" => Some(false),
            Value::Borrow(_) => None,
            Value::Owned(_) => None,
            Value::Float(f) if f == 0.0 => Some(false),
            Value::Float(f) if f == 1.0 => Some(true),
            Value::Bool(b) => Some(b),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Borrow(v) => !v.is_empty(),
            Value::Owned(v) => !v.is_empty(),
            Value::Float(f) => *f != 0.0,
            Value::Bool(b) => *b,
            _ => false,
        }
    }
}

impl<'a> Default for Value<'a> {
    fn default() -> Self {
        Value::NotReady
    }
}

impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Borrow(v) => v.fmt(fmt),
            Value::Owned(v) => v.fmt(fmt),
            Value::Float(f) => f.fmt(fmt),
            Value::Bool(true) => "1".fmt(fmt),
            Value::Bool(false) => "0".fmt(fmt),
            Value::NotReady => Ok(()),
            Value::Empty => Ok(()),
            Value::Error => Ok(()),
        }
    }
}

impl<'a> Into<JsonValue> for Value<'a> {
    fn into(self) -> JsonValue {
        match self {
            Value::Borrow(v) => v.into(),
            Value::Owned(v) => v.into(),
            Value::Float(f) => f.into(),
            Value::Bool(b) => b.into(),
            _ => JsonValue::Null,
        }
    }
}

impl<'a> From<Value<'a>> for evalexpr::Value {
    fn from(v: Value) -> Self {
        match v {
            Value::Owned(s) => evalexpr::Value::String(s),
            Value::Borrow(s) => evalexpr::Value::String(s.into()),
            Value::Float(f) => evalexpr::Value::Float(f),
            Value::Bool(f) => evalexpr::Value::Boolean(f),
            _ => evalexpr::Value::Empty,
        }
    }
}

impl<'a> From<evalexpr::Value> for Value<'a> {
    fn from(v: evalexpr::Value) -> Self {
        match v {
            evalexpr::Value::String(s) => Value::Owned(s),
            evalexpr::Value::Float(n) => Value::Float(n),
            evalexpr::Value::Int(n) => Value::Float(n as _),
            evalexpr::Value::Boolean(b) => Value::Bool(b),
            evalexpr::Value::Empty => Value::Empty,
            _ => {
                warn!("Ignoring invalid return type from eval");
                Value::Error
            }
        }
    }
}

impl<'a> From<&'a JsonValue> for Value<'a> {
    fn from(v: &'a JsonValue) -> Self {
        match v {
            JsonValue::String(s) => Value::Borrow(s),
            JsonValue::Short(s) => Value::Borrow(s),
            &JsonValue::Number(n) => Value::Float(n.into()),
            &JsonValue::Boolean(b) => Value::Bool(b),
            JsonValue::Object(_) | JsonValue::Array(_) => Value::Error,
            JsonValue::Null => Value::Empty,
        }
    }
}

impl<'a> From<toml::Value> for Value<'a> {
    fn from(v: toml::Value) -> Self {
        match v {
            toml::Value::String(s) => Value::Owned(s),
            toml::Value::Integer(n) => Value::Float(n as f64),
            toml::Value::Float(n) => Value::Float(n),
            toml::Value::Boolean(b) => Value::Bool(b),
            _ => Value::Error,
        }
    }
}

impl<'a> From<Cow<'a, str>> for Value<'a> {
    fn from(v: Cow<'a, str>) -> Self {
        match v {
            Cow::Borrowed(v) => Value::Borrow(v),
            Cow::Owned(v) => Value::Owned(v),
        }
    }
}

impl<'a> From<&'a str> for Value<'a> {
    fn from(v: &'a str) -> Self {
        Value::Borrow(v)
    }
}

impl<'a> From<String> for Value<'a> {
    fn from(v: String) -> Self {
        Value::Owned(v)
    }
}
