pub fn toml_to_string(value : Option<&toml::Value>) -> Option<String> {
    value.and_then(|value| {
        if let Some(v) = value.as_str() {
            Some(v.to_owned())
        } else if let Some(v) = value.as_integer() {
            Some(format!("{}", v))
        } else if let Some(v) = value.as_float() {
            Some(format!("{}", v))
        } else {
            None
        }
    })
}

pub fn toml_to_f64(value : Option<&toml::Value>) -> Option<f64> {
    value.and_then(|value| {
        if let Some(v) = value.as_float() {
            Some(v)
        } else if let Some(v) = value.as_integer() {
            Some(v as f64)
        } else {
            None
        }
    })
}
