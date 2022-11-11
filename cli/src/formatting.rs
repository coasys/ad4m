use anyhow::{bail, Result};
use serde_json::Value;

use crate::types::{Agent, LinkExpression};

pub fn print_prolog_result(result: Value) -> Result<()> {
    match result {
        Value::Object(map) => {
            for (key, value) in map {
                let value = match value {
                    Value::String(string) => string,
                    Value::Number(number) => number.to_string(),
                    Value::Bool(boolean) => boolean.to_string(),
                    Value::Array(_) => bail!("Unexpected nested object value"),
                    Value::Object(_) => bail!("Unexpected nested object value"),
                    Value::Null => "null".to_string(),
                };
                println!("\x1b[36m{}:\x1b[97m {}", key, value);
            }
        }
        _ => bail!("Unexpected non-obhect value in prolog result: {:?}", result),
    }
    Ok(())
}

fn maybe_decode_literal(uri: String) -> String {
    if uri.starts_with("literal://") {
        let literal = uri.replace("literal://", "");
        if literal.starts_with("string:") {
            let string = literal.replace("string:", "");
            urlencoding::decode(&string)
                .map(|s| s.to_string())
                .unwrap_or(string)
        } else if literal.starts_with("number:") {
            literal.replace("number:", "")
        } else if literal.starts_with("json:") {
            let json = literal.replace("json:", "");
            if let Ok(decoded_json) = urlencoding::decode(&json) {
                let decoded_json_string = decoded_json.to_string().replace("\\'", "'");
                match serde_json::from_str::<serde_json::Value>(&decoded_json_string) {
                    Ok(expression) => return expression["data"].to_string(),
                    Err(e) => {
                        println!("Failed to decode json literal: {}", e);
                        return decoded_json.to_string();
                    }
                }
            } else {
                println!("Failed to decode url encoding");
            }
            json
        } else {
            literal
        }
    } else {
        uri
    }
}

pub fn print_link(link: LinkExpression) {
    let source = maybe_decode_literal(link.data.source);
    let target = maybe_decode_literal(link.data.target);

    if let Some(pred) = link.data.predicate {
        let predicate = maybe_decode_literal(pred);
        println!(
            "\x1b[90m[{}] \x1b[35m{} \x1b[97m--\x1b[95m{}\x1b[97m--> \x1b[32m{} \x1b[34m({})",
            link.timestamp, source, predicate, target, link.author
        );
    } else {
        println!(
            "\x1b[90m[{}] \x1b[35m{} \x1b[97m----> \x1b[32m{} \x1b[34m({})",
            link.timestamp, source, target, link.author
        );
    }
}

pub fn print_agent(agent: Agent) {
    println!("\x1b[36mDID: \x1b[97m{}", agent.did);
    println!(
        "\x1b[36mDirect message language: \x1b[97m{}",
        agent
            .direct_message_language
            .unwrap_or("<NOT SET>".to_string())
    );
    println!("\x1b[36mPublic Perspective:");
    if let Some(perspective) = agent.perspective {
        for link in perspective.links {
            print_link(link);
        }
    }
}

pub fn print_logo() {
    println!(
        r#"                                                                                
                                                                                                                               .xXKkd:'                         
                                                                                                                              .oNOccx00x;.                      
                                                                                                                              ;KK;  .ck0Oxdolc;..               
                                                                                                                              lWk:oOK0OxdoxOOO0KOd;.            
                                                                                                                              dWkdkl,.    .o0x;'cxK0l.          
       .,ldxxxxxxoc.         .cdxxxxxxxxxxxdoc,.             'oxo'     ;dx;     .;dxxxo:.               .;odxxx:             .dWk'         .dNx.  'o0O:         
      .xNWNKKKKKXWWK:       'OWWX0000000000KNWWKo.          ,0WNd.     dWWd.    .oWMNXNWKc.            :0WWXNMMk.           :dxX0,      .,cllOXo:oooxkd,        
     .xWWk,......lXMK;      ;XMK:. . .  ....':kNWK;        ,0MNo.      oWWd.    .oWMk':0MNo.          cXMXc'dWMx.          ,0KlxNd. .;dO00kd;cK0c:loxkOko:.     
     cNM0'        oNMk.     ;XM0'             .lXM0'      ;0MNo.       oWWd.    .oWMx. ,0MNl         :KMX:  lWMx.         .dWx.'kkcd0Kxc'.   .kK:    .;dkO0d,   
    ,KMX:         .kWWo.    ;XM0'              .kMNc     ;KMNo.        oWWd.    .oWWd.  ;KMXl       :KMXc   lWMx.         .ONc  ;kX0o.       .xXc     'kk::kKx, 
   .kWWo.          ,KMX:    ;XM0'              .OMNc    ;KMNo.         oWWd.    .oWMd.   ;KMXc     ;KMXc    lWMx.         .ONc.cKKddx:.      .OK;     ,00, .c00c
   lNMO.            lNMO'   ;XM0'             .dNMO.   :KMMKocccccccccl0WM0l;.  .oWMx.    :KMXc   ;0MNl     lWMx.         .dNxlKO,.,x0Ol'    :Kk.     cXk'.:d0x;
  ;KMK;             .xWWd.  ;XMXc..........';o0WWO,   ;0WWWWWWWWWWWWWWWMMMMW0,  .oWWd.     :XMXl,c0MNl.     lWMx.          ,0Xxc'    'lk0ko:;lx:.    ,OKookko,. 
 .kMNo               ,0MXc  .kWMWNNNNNXNNNNNWWXk:.    .,;;;;;;;;;;;;;,:OMWO:'   .oWMd.      :0WWWWWKc.      lWMx.           ;0Kc.      .':okOOOkkxdcck0l,:,.    
 .co:.                ,ll,   .;lllloolllllllc;.                        'll'      'll,        .;cll;.        'll,            ,dOKx,        ;xd:,,;cox0k;         
                                                                                                                           .d0lck0kc,.  ,xKk;..':dOkc.          
                                                                                                                           .dNo..,lddllk0Oxddxkkxl,.            
                                                                                                                            lXk;';cdO0ko,':lc:,.                
                                                                                                                            'x0Okkdl:'.                         
                                                                                                                             .,'..                              
                                                                     
"#
    );
}
