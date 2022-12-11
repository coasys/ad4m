use anyhow::{anyhow, Result};

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralValue {
    String(String),
    Number(f64),
    Json(serde_json::Value),
}

pub struct Literal {
    value: Option<LiteralValue>,
    url: Option<String>
}

impl Literal {
    
    pub fn from_url(url: String) -> Result<Self> {
        if url.starts_with("literal://")  {
            Ok(Self {
                value: None,
                url: Some(url)
            })
        } else {
            Err(anyhow!("Not a literal URL"))
        }
    }

    pub fn from_string(string: String) -> Self {
        Self {
            value: Some(LiteralValue::String(string)),
            url: None
        }
    }

    pub fn from_number(number: f64) -> Self {
        Self {
            value: Some(LiteralValue::Number(number)),
            url: None
        }
    }

    pub fn from_json(json: serde_json::Value) -> Self {
        Self {
            value: Some(LiteralValue::Json(json)),
            url: None
        }
    }

    pub fn to_url(&self) -> Result<String> {
        if let Some(url) = &self.url {
            Ok(url.clone())
        } else if let Some(value) = &self.value {
            match value {
                LiteralValue::String(string) => {
                    let quote_encoded = Self::encode_single_quote(&string);
                    let encoded = urlencoding::encode(&quote_encoded);
                    Ok(format!("literal://string:{}", encoded))
                }
                LiteralValue::Number(number) => {
                    Ok(format!("literal://number:{}", number))
                }
                LiteralValue::Json(json) => {
                    let quote_encoded = Self::encode_single_quote(&json.to_string());
                    let encoded = urlencoding::encode(&quote_encoded).to_string();
                    let fixed = encoded.replace("%3A", ":").replace("%2C", ",");
                    Ok(format!("literal://json:{}", fixed))
                }
            }
        } else {
            Err(anyhow!("No value or URL"))
        }
    }

    pub fn parse_url(&self) -> Result<LiteralValue> {
        if let Some(url) = &self.url {
            if url.starts_with("literal://") {
                let literal = Self::decode_single_quote(&url.replace("literal://", ""));
                if literal.starts_with("string:") {
                    let string = literal.replace("string:", "");
                    let decoded = urlencoding::decode(&string)?;
                    Ok(LiteralValue::String(decoded.into()))
                } else if literal.starts_with("number:") {
                    let number = literal.replace("number:", "");
                    let parsed = number.parse::<f64>()?;
                    Ok(LiteralValue::Number(parsed))
                } else if literal.starts_with("json:") {
                    let json = literal.replace("json:", "");
                    let decoded = urlencoding::decode(&json)?;
                    let decoded_json_string = decoded.to_string().replace("\\'", "'");
                    let parsed = serde_json::from_str::<serde_json::Value>(&decoded_json_string)?;
                    Ok(LiteralValue::Json(parsed))
                } else {
                    Err(anyhow!("Unknown literal type"))
                }
            } else {
                Err(anyhow!("Not a literal URL"))
            }
        } else {
            Err(anyhow!("No URL"))
        }
    }

    pub fn get(&self) -> Result<LiteralValue> {
        if let Some(value) = &self.value {
            Ok(value.clone())
        } else if self.url.is_some() {
            self.parse_url()
        } else {
            Err(anyhow!("No value or URL"))
        }
    }

    pub fn convert(&mut self) -> Result<()> {
        if self.value.is_some() {
            self.url = Some(self.to_url()?);
            Ok(())
        } else if self.url.is_some() {
            self.value = Some(self.parse_url()?);
            Ok(())
        } else {
            Err(anyhow!("No value or URL"))
        }
    }

    fn encode_single_quote(string: &str) -> String {
        string.replace("'", "\\'")
    }

    fn decode_single_quote(string: &str) -> String {
        string.replace("\\'", "'")
    }

}

#[cfg(test)]
mod test {
    use serde_json::json;

    #[test]
    fn can_handle_strings() {
        let test_string = "test string";
        let test_url = "literal://string:test%20string";

        let literal = super::Literal::from_string(test_string.into());
        assert_eq!(literal.to_url().unwrap(), test_url);

        let mut literal2 = super::Literal::from_url(test_url.into()).unwrap();
        assert_eq!(literal2.get().unwrap(), super::LiteralValue::String(test_string.into()));

        literal2.convert().expect("Failed to convert");
        assert_eq!(literal2.value.unwrap(), super::LiteralValue::String(test_string.into()));
    }

    #[test]
    fn can_handle_numbers() {
        let test_number = 3.1415;
        let test_url = "literal://number:3.1415";

        let literal = super::Literal::from_number(test_number.into());
        assert_eq!(literal.to_url().unwrap(), test_url);

        let mut literal2 = super::Literal::from_url(test_url.into()).unwrap();
        assert_eq!(literal2.get().unwrap(), super::LiteralValue::Number(test_number));

        literal2.convert().expect("Failed to convert");
        assert_eq!(literal2.value.unwrap(), super::LiteralValue::Number(test_number.into()));
    }

    #[test]
    fn can_handle_objects() {
        let test_object = json!({
            "testNumber": "1337",
            "testString": "test", 
        });
        let test_url = "literal://json:%7B%22testNumber%22:%221337%22,%22testString%22:%22test%22%7D";

        let literal = super::Literal::from_json(test_object.clone());
        assert_eq!(literal.to_url().unwrap(), test_url);

        let mut literal2 = super::Literal::from_url(test_url.into()).unwrap();
        assert_eq!(literal2.get().unwrap(), super::LiteralValue::Json(test_object.clone()));

        literal2.convert().expect("Failed to convert");
        assert_eq!(literal2.value.unwrap(), super::LiteralValue::Json(test_object));
    }
}