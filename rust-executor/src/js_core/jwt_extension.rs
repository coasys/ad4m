use std::time::{SystemTime, UNIX_EPOCH};

use deno_core::{anyhow::anyhow, error::AnyError, include_js_files, op, Extension};
use jsonwebtoken::{encode, Algorithm, DecodingKey, EncodingKey, Header};
use picky_asn1::bit_string::BitString;
use picky_asn1::wrapper::BitStringAsn1;
use picky_asn1::wrapper::ExplicitContextTag0;
use picky_asn1::wrapper::IntegerAsn1;
use picky_asn1_x509::PrivateKeyValue;
use serde::{Deserialize, Serialize};

use simple_asn1::{to_der, ASN1Block, ASN1Class};

use picky_asn1::wrapper::ExplicitContextTag1;
use picky_asn1::wrapper::OctetStringAsn1;
use picky_asn1::wrapper::OctetStringAsn1Container;
use picky_asn1::wrapper::Optional;
use picky_asn1_x509::private_key_info::ECPrivateKey;
use picky_asn1_x509::private_key_info::PrivateKeyInfo;

use log::debug;

use crate::wallet::Wallet;

#[derive(Debug, Serialize, Deserialize)]
struct Claims {
    iss: String,
    aud: String,
    exp: String,
    iat: u64,
}

#[op]
async fn generate_jwt(
    issuer: String,
    audience: String,
    expiration_time: String,
) -> Result<String, AnyError> {
    // Get the private key
    let wallet = Wallet::instance();
    let wallet_lock = wallet.lock().expect("wallet lock");
    let wallet_ref = wallet_lock.as_ref().expect("wallet instance");
    let name = "main".to_string();

    let secret_key = wallet_ref
        .get_secret_key(&name)
        .ok_or(anyhow!("main key not found. call createMainKey() first"))?;

    let public_key = wallet_ref
        .get_public_key(&name)
        .ok_or(anyhow!("main key not found. call createMainKey() first"))?;

    // let encoded = to_der(&ASN1Block::Sequence(
    //     0,
    //     vec![
    //         ASN1Block::OctetString(0, secret_key),
    //         ASN1Block::OctetString(0, public_key),
    //     ],
    // ))
    // .unwrap();

    let zero: i32 = 0;
    let ec_key: ECPrivateKey = ECPrivateKey {
        version: IntegerAsn1(zero.to_le_bytes().to_vec()),
        private_key: OctetStringAsn1(secret_key),
        parameters: Optional(ExplicitContextTag0(None)),
        public_key: Optional(ExplicitContextTag1(BitStringAsn1(BitString::with_bytes(
            public_key,
        )))),
    };
    let octec_string: OctetStringAsn1Container<ECPrivateKey> = ec_key.into();

    let private_key_value = PrivateKeyValue::EC(octec_string);

    let private_key_info = PrivateKeyInfo {
        version: 0,
        private_key_algorithm: picky_asn1_x509::AlgorithmIdentifier::new_ed25519(),
        private_key: private_key_value,
    };

    let pkcs8_der = picky_asn1_der::to_vec(&private_key_info).unwrap();

    let now = SystemTime::now();
    let unix_timestamp = now
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards")
        .as_secs();

    let payload = Claims {
        iss: issuer,
        aud: audience,
        exp: expiration_time,
        iat: unix_timestamp,
    };

    debug!("payload: {:?}", payload);

    let header = Header::new(Algorithm::EdDSA);
    debug!("header: {:?}", header);
    let token = encode(
        &header,
        &payload,
        &EncodingKey::from_ed_der(pkcs8_der.as_slice()),
    )?;
    debug!("Token: {:?}", token);

    Ok(token)
}

#[op]
async fn verify_jwt(token: String) -> Result<Claims, AnyError> {
    //Get the private key
    let wallet = Wallet::instance();
    let wallet_lock = wallet.lock().expect("wallet lock");
    let wallet_ref = wallet_lock.as_ref().expect("wallet instance");
    let name = "main".to_string();

    let secret_key = wallet_ref
        .get_secret_key(&name)
        .ok_or(anyhow!("main key not found. call createMainKey() first"))?;

    let public_key = wallet_ref
        .get_public_key(&name)
        .ok_or(anyhow!("main key not found. call createMainKey() first"))?;

    // let encoded = to_der(&ASN1Block::Sequence(
    //     0,
    //     vec![
    //         ASN1Block::OctetString(0, secret_key),
    //         ASN1Block::OctetString(0, public_key),
    //     ],
    // ))
    // .unwrap();

    // let private_key_info = PrivateKeyInfo::new(
    //     picky_asn1_x509::AlgorithmIdentifier::new_ed25519(),
    //     &private_key,
    //     Some(public_key),
    // );

    let result = jsonwebtoken::decode::<Claims>(
        &token,
        &DecodingKey::from_ed_der(public_key.as_slice()),
        &jsonwebtoken::Validation::new(Algorithm::EdDSA),
    )?;

    Ok(result.claims)
}

pub fn build() -> Extension {
    Extension::builder("jwt")
        .js(include_js_files!(jwt "jwt_extension.js",))
        .ops(vec![generate_jwt::decl(), verify_jwt::decl()])
        .force_op_registration()
        .build()
}
