//! `Literal` moved to the `ad4m-literal` crate; its public path in this crate must not change.

use ad4m_client::literal::{Literal, LiteralValue};

#[test]
fn literal_keeps_its_ad4m_client_path() {
    let url = Literal::from_string("hello".into()).to_url().unwrap();
    assert_eq!(url, "literal:string:hello");
    let value = Literal::from_url(url).unwrap().get().unwrap();
    assert_eq!(value, LiteralValue::String("hello".into()));
}
