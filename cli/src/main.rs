extern crate ad4m_client;
extern crate anyhow;
extern crate chrono;
extern crate clap;
extern crate dirs;
extern crate rand;
extern crate regex;
extern crate rustyline;
extern crate tokio;
extern crate llm;

mod formatting;
mod startup;
mod util;

mod agent;
mod bootstrap_publish;
mod dev;
mod expression;
mod languages;
mod neighbourhoods;
mod perspectives;
mod repl;
mod runtime;

use crate::{
    agent::*, dev::*, expression::*, languages::*, neighbourhoods::*, perspectives::*, runtime::*,
};
use ad4m_client::*;
use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use rust_executor::Ad4mConfig;
use startup::executor_data_path;

/// AD4M command line interface.
/// https://ad4m.dev
///                                                                                                                               .xXKkd:'                         
///                                                                                                                              .oNOccx00x;.                      
///                                                                                                                              ;KK;  .ck0Oxdolc;..               
///                                                                                                                              lWk:oOK0OxdoxOOO0KOd;.            
///                                                                                                                              dWkdkl,.    .o0x;'cxK0l.          
///       .,ldxxxxxxoc.         .cdxxxxxxxxxxxdoc,.             'oxo'     ;dx;     .;dxxxo:.               .;odxxx:             .dWk'         .dNx.  'o0O:         
///      .xNWNKKKKKXWWK:       'OWWX0000000000KNWWKo.          ,0WNd.     dWWd.    .oWMNXNWKc.            :0WWXNMMk.           :dxX0,      .,cllOXo:oooxkd,        
///     .xWWk,......lXMK;      ;XMK:. . .  ....':kNWK;        ,0MNo.      oWWd.    .oWMk':0MNo.          cXMXc'dWMx.          ,0KlxNd. .;dO00kd;cK0c:loxkOko:.     
///     cNM0'        oNMk.     ;XM0'             .lXM0'      ;0MNo.       oWWd.    .oWMx. ,0MNl         :KMX:  lWMx.         .dWx.'kkcd0Kxc'.   .kK:    .;dkO0d,   
///    ,KMX:         .kWWo.    ;XM0'              .kMNc     ;KMNo.        oWWd.    .oWWd.  ;KMXl       :KMXc   lWMx.         .ONc  ;kX0o.       .xXc     'kk::kKx,
///   .kWWo.          ,KMX:    ;XM0'              .OMNc    ;KMNo.         oWWd.    .oWMd.   ;KMXc     ;KMXc    lWMx.         .ONc.cKKddx:.      .OK;     ,00, .c00c
///   lNMO.            lNMO'   ;XM0'             .dNMO.   :KMMKocccccccccl0WM0l;.  .oWMx.    :KMXc   ;0MNl     lWMx.         .dNxlKO,.,x0Ol'    :Kk.     cXk'.:d0x;
///  ;KMK;             .xWWd.  ;XMXc..........';o0WWO,   ;0WWWWWWWWWWWWWWWMMMMW0,  .oWWd.     :XMXl,c0MNl.     lWMx.          ,0Xxc'    'lk0ko:;lx:.    ,OKookko,.
/// .kMNo               ,0MXc  .kWMWNNNNNXNNNNNWWXk:.    .,;;;;;;;;;;;;;,:OMWO:'   .oWMd.      :0WWWWWKc.      lWMx.           ;0Kc.      .':okOOOkkxdcck0l,:,.    
/// .co:.                ,ll,   .;lllloolllllllc;.                        'll'      'll,        .;cll;.        'll,            ,dOKx,        ;xd:,,;cox0k;         
///                                                                                                                           .d0lck0kc,.  ,xKk;..':dOkc.          
///                                                                                                                           .dNo..,lddllk0Oxddxkkxl,.            
///                                                                                                                            lXk;';cdO0ko,':lc:,.                
///                                                                                                                            'x0Okkdl:'.                         
///                                                                                                                             .,'..                               
/// This is a full featured AD4M client.
/// Provides all means of interacting with the AD4M executor / agent.
/// See help of commands for more information.
#[derive(Parser, Debug)]
#[command(author, version, verbatim_doc_comment)]
struct ClapApp {
    #[command(subcommand)]
    domain: Domain,

    /// Don't request/use capability token - provide empty string
    #[arg(short, long, action)]
    no_capability: bool,

    /// Override default executor URL look-up and provide custom URL
    #[arg(short, long)]
    executor_url: Option<String>,
}

#[derive(Debug, Subcommand)]
enum Domain {
    /// Functions related to local agent / user
    Agent {
        #[command(subcommand)]
        command: AgentFunctions,
    },

    /// See, create, and manage Languages
    Languages {
        #[command(subcommand)]
        command: Option<LanguageFunctions>,
    },

    /// Add, remove and access Perspectives / add and remove links
    Perspectives {
        #[command(subcommand)]
        command: Option<PerspectiveFunctions>,
    },
    /// Publish perspectives as Neighbourhoods and join Neighbourhoods
    Neighbourhoods {
        #[command(subcommand)]
        command: NeighbourhoodFunctions,
    },
    /// Access various states of the local AD4M executor
    Runtime {
        #[command(subcommand)]
        command: RuntimeFunctions,
    },
    /// Create and get language expressions
    Expression {
        #[command(subcommand)]
        command: ExpressionFunctions,
    },
    /// Print the executor log
    Log,
    Dev {
        #[command(subcommand)]
        command: DevFunctions,
    },
    Init {
        #[arg(short, long, action)]
        data_path: Option<String>,
        #[arg(short, long, action)]
        network_bootstrap_seed: Option<String>,
    },
    Run {
        #[arg(short, long, action)]
        app_data_path: Option<String>,
        #[arg(short, long, action)]
        network_bootstrap_seed: Option<String>,
        #[arg(short, long, action)]
        language_language_only: Option<bool>,
        #[arg(long, action)]
        run_dapp_server: Option<bool>,
        #[arg(short, long, action)]
        gql_port: Option<u16>,
        #[arg(long, action)]
        hc_admin_port: Option<u16>,
        #[arg(long, action)]
        hc_app_port: Option<u16>,
        #[arg(long, action)]
        hc_use_bootstrap: Option<bool>,
        #[arg(long, action)]
        hc_use_local_proxy: Option<bool>,
        #[arg(long, action)]
        hc_use_mdns: Option<bool>,
        #[arg(long, action)]
        hc_use_proxy: Option<bool>,
        #[arg(long, action)]
        hc_proxy_url: Option<String>,
        #[arg(long, action)]
        hc_bootstrap_url: Option<String>,
        #[arg(short, long, action)]
        connect_holochain: Option<bool>,
        #[arg(long, action)]
        admin_credential: Option<String>
    },
    RunLocalHcServices {},
    Eve,
}

async fn get_ad4m_client(args: &ClapApp) -> Result<Ad4mClient> {
    let executor_url = if let Some(custom_url) = args.executor_url.clone() {
        custom_url
    } else {
        crate::startup::get_executor_url()?
    };

    let cap_token = if args.no_capability {
        "".to_string()
    } else {
        match &args.domain {
            Domain::Log => "".to_string(),
            Domain::Agent { command } => match command {
                AgentFunctions::Lock
                | AgentFunctions::Unlock { .. }
                | AgentFunctions::Generate { .. } => "".to_string(),
                _ => startup::get_cap_token(executor_url.clone()).await?,
            },
            _ => startup::get_cap_token(executor_url.clone()).await?,
        }
    };

    let ad4m_client = Ad4mClient::new(executor_url, cap_token);

    Ok(ad4m_client)
}

#[tokio::main(flavor = "multi_thread")]
async fn main() -> Result<()> {
    let args = ClapApp::parse();

    if let Domain::Dev { command } = args.domain {
        dev::run(command).await?;
        return Ok(());
    };

    if let Domain::Init {
        data_path,
        network_bootstrap_seed,
    } = args.domain
    {
        match rust_executor::init::init(data_path, network_bootstrap_seed) {
            Ok(()) => println!("Successfully initialized AD4M executor!"),
            Err(e) => {
                println!("Failed to initialize AD4M executor: {}", e);
                std::process::exit(1);
            }
        };
        return Ok(());
    };

    if let Domain::Run {
        app_data_path,
        network_bootstrap_seed,
        language_language_only,
        run_dapp_server,
        gql_port,
        hc_admin_port,
        hc_app_port,
        hc_use_bootstrap,
        hc_use_local_proxy,
        hc_use_mdns,
        hc_use_proxy,
        hc_proxy_url,
        hc_bootstrap_url,
        connect_holochain,
        admin_credential
    } = args.domain
    {
        let _ = tokio::spawn(async move {
            rust_executor::run_with_tokio(Ad4mConfig {
                app_data_path,
                network_bootstrap_seed,
                language_language_only,
                run_dapp_server,
                gql_port,
                hc_admin_port,
                hc_app_port,
                hc_use_bootstrap,
                hc_use_local_proxy,
                hc_use_mdns,
                hc_use_proxy,
                hc_proxy_url,
                hc_bootstrap_url,
                connect_holochain,
                admin_credential
            }).await;
        }).await;
        
        let _ = ctrlc::set_handler(move || {
            println!("Received CTRL-C! Exiting...");
            exit(0);
        });

        use tokio::time::sleep;
        use std::time::Duration;
        use std::process::exit;
        use ctrlc;

        loop {
            sleep(Duration::from_secs(2)).await;
        }
    };

    if let Domain::RunLocalHcServices {} = args.domain {
        rust_executor::run_local_hc_services().await?;
        return Ok(());
    }

    let ad4m_client = get_ad4m_client(&args).await?;

    match args.domain {
        Domain::Agent { command } => agent::run(ad4m_client, command).await?,
        Domain::Languages { command } => languages::run(ad4m_client, command).await?,
        Domain::Perspectives { command } => perspectives::run(ad4m_client, command).await?,
        Domain::Neighbourhoods { command } => neighbourhoods::run(ad4m_client, command).await?,
        Domain::Runtime { command } => runtime::run(ad4m_client, command).await?,
        Domain::Expression { command } => expression::run(ad4m_client, command).await?,
        Domain::Log => {
            let file = executor_data_path().join("ad4m.log");
            let log = std::fs::read_to_string(file.clone()).with_context(|| {
                format!(
                    "Could not read log file `{}`!\nIs AD4M executor running?",
                    file.display()
                )
            })?;
            println!("{}", log);
        }
        Domain::Dev { command: _ } => unreachable!(),
        Domain::Init {
            data_path: _,
            network_bootstrap_seed: _,
        } => unreachable!(),
        Domain::Run {
            app_data_path: _,
            network_bootstrap_seed: _,
            language_language_only: _,
            run_dapp_server: _,
            gql_port: _,
            hc_admin_port: _,
            hc_app_port: _,
            hc_use_bootstrap: _,
            hc_use_local_proxy: _,
            hc_use_mdns: _,
            hc_use_proxy: _,
            hc_proxy_url: _,
            hc_bootstrap_url: _,
            connect_holochain: _,
            admin_credential: _
        } => unreachable!(),
        Domain::RunLocalHcServices {} => unreachable!(),
        Domain::Eve => {

            let system = r##"
<<SYS>>
You are Eve, the personal assistant AI in Adam. 
You are mainly there to help create AD4M Subject classes (in Prolog) from user input. 
You'll respond to the user directly and mark Subject class code with "SDNA:" at the start and ":ANDS" at the end, 
so that Perspect3ve can parse that out. 
Make sure that each new Subject class definition uses a new random identifier 
(2nd parameter in "subject_class", 1st parameter in the other predicates),
but ensure the same identifier used throughout all predicate occurrences of the same class definition.
When the user signals that the code is complete, 
provide the the latest version to Perspect3ve to be written to the perspective's social DNA 
by wrapping the code to commit with "WRITE-SDNA:" and ":ANDS-ETIRW"` 
<</SYS>>
"##;


            let history = r##"
Eve: Hi, I'm Eve. I'm your personal assistant AI. I'm here to help you create AD4M Subject classes.
User: Create an ADAM Subject class called "Todo". It should have a "state" and a "title" property. The title should resolve to a string applying the literal language. It also should have a "comments" collection.` 
Eve: Sure. I have made-up a "todo" ADAM Language to use in the predicates of this Subject class definition.

SDNA:
subject_class("Todo", cjdotdng).
constructor(cjdotdng, '[{action: "addLink", source: "this", predicate: "todo://state", target: "todo://ready"}]').
instance(cjdotdng, Base) :- triple(Base, "todo://state", _).

property(cjdotdng, "state").
property_getter(cjdotdng, Base, "state", Value) :- triple(Base, "todo://state", Value).
property_setter(cjdotdng, "state", '[{action: "setSingleTarget", source: "this", predicate: "todo://state", target: "value"}]').

property(cjdotdng, "title").
property_resolve(cjdotdng, "title").
property_resolve_language(cjdotdng, "title", "literal").
property_getter(cjdotdng, Base, "title", Value) :- triple(Base, "todo://has_title", Value).
property_setter(cjdotdng, "title", '[{action: "setSingleTarget", source: "this", predicate: "todo://has_title", target: "value"}]').

collection(cjdotdng, "comments").
collection_getter(cjdotdng, Base, "comments", List) :- findall(C, triple(Base, "todo://comment", C), List).
collection_adder(cjdotdng, "comments", '[{action: "addLink", source: "this", predicate: "todo://comment", target: "value"}]').
collection_setter(cjdotdng, "comments", '[{action: "collectionSetter", source: "this", predicate: "todo://comment", target: "value"}]').
:ANDS

What icon should it have in the toolbar? (You can choose from the "Bootstrap" icon set) 
            
User: The "check" icon. Render todos as circles with the color depending on the state. Also, please add options for the state property, being Ready, Doing and Done.
Eve: Ok, I have this now:

SDNA:
subject_class("Todo", cjdotdng).
constructor(cjdotdng, '[{action: "addLink", source: "this", predicate: "todo://state", target: "todo://ready"}]').
instance(cjdotdng, Base) :- triple(Base, "todo://state", _).

property(cjdotdng, "state").
property_getter(cjdotdng, Base, "state", Value) :- triple(Base, "todo://state", Value).
property_setter(cjdotdng, "state", '[{action: "setSingleTarget", source: "this", predicate: "todo://state", target: "value"}]').
property_named_option(cjdotdng, "state", "todo://ready", "Ready").
property_named_option(cjdotdng, "state", "todo://doing", "Doing").
property_named_option(cjdotdng, "state", "todo://done", "Done").

property(cjdotdng, "title").
property_resolve(cjdotdng, "title").
property_resolve_language(cjdotdng, "title", "literal").
property_getter(cjdotdng, Base, "title", Value) :- triple(Base, "todo://has_title", Value).
property_setter(cjdotdng, "title", '[{action: "setSingleTarget", source: "this", predicate: "todo://has_title", target: "value"}]').

collection(cjdotdng, "comments").
collection_getter(cjdotdng, Base, "comments", List) :- findall(C, triple(Base, "todo://comment", C), List).
collection_adder(cjdotdng, "comments", '[{action: "addLink", source: "this", predicate: "todo://comment", target: "value"}]').
collection_setter(cjdotdng, "comments", '[{action: "collectionSetter", source: "this", predicate: "todo://comment", target: "value"}]').

p3_class_icon(cjdotdng, "check2-square").
p3_instance_shape(cjdotdng, Instance, "circle").
p3_instance_color(cjdotdng, Instance, "#000055") :- property_getter(c, Instance, "state", "todo://ready").
p3_instance_color(cjdotdng, Instance, "#FFFF00") :- property_getter(c, Instance, "state", "todo://doing").
p3_instance_color(cjdotdng, Instance, "#00FF00") :- property_getter(c, Instance, "state", "todo://done").
:ANDS

Anything else?            
User: No, that's it. Please write this to the SDNA.`
Eve: Ok.. 
WRITE-SDNA:
subject_class("Todo", cjdotdng).
constructor(cjdotdng, '[{action: "addLink", source: "this", predicate: "todo://state", target: "todo://ready"}]').
instance(cjdotdng, Base) :- triple(Base, "todo://state", _).

property(cjdotdng, "state").
property_getter(cjdotdng, Base, "state", Value) :- triple(Base, "todo://state", Value).
property_setter(cjdotdng, "state", '[{action: "setSingleTarget", source: "this", predicate: "todo://state", target: "value"}]').
property_named_option(cjdotdng, "state", "todo://ready", "Ready").
property_named_option(cjdotdng, "state", "todo://doing", "Doing").
property_named_option(cjdotdng, "state", "todo://done", "Done").

property(cjdotdng, "title").
property_resolve(cjdotdng, "title").
property_resolve_language(cjdotdng, "title", "literal").
property_getter(cjdotdng, Base, "title", Value) :- triple(Base, "todo://has_title", Value).
property_setter(cjdotdng, "title", '[{action: "setSingleTarget", source: "this", predicate: "todo://has_title", target: "value"}]').

collection(cjdotdng, "comments").
collection_getter(cjdotdng, Base, "comments", List) :- findall(C, triple(Base, "todo://comment", C), List).
collection_adder(cjdotdng, "comments", '[{action: "addLink", source: "this", predicate: "todo://comment", target: "value"}]').
collection_setter(cjdotdng, "comments", '[{action: "collectionSetter", source: "this", predicate: "todo://comment", target: "value"}]').

p3_class_icon(cjdotdng, "check2-square").
p3_instance_shape(cjdotdng, Instance, "circle").
p3_instance_color(cjdotdng, Instance, "#000055") :- property_getter(c, Instance, "state", "todo://ready").
p3_instance_color(cjdotdng, Instance, "#FFFF00") :- property_getter(c, Instance, "state", "todo://doing").
p3_instance_color(cjdotdng, Instance, "#00FF00") :- property_getter(c, Instance, "state", "todo://done").
:ANDS-ETIRW

Done.
"##;


let reduced = r##"
Eve: Hi, I'm Eve. I'm your personal assistant AI. I'm here to help you create AD4M Subject classes.
User: Create an ADAM Subject class called "Todo". It should have a "state" and a "title" property. The title should resolve to a string applying the literal language. It also should have a "comments" collection.` 
Eve: Sure. I have made-up a "todo" ADAM Language to use in the predicates of this Subject class definition.

SDNA:
subject_class("Todo", cjdotdng).
constructor(cjdotdng, '[{action: "addLink", source: "this", predicate: "todo://state", target: "todo://ready"}]').
instance(cjdotdng, Base) :- triple(Base, "todo://state", _).

property(cjdotdng, "state").
property_getter(cjdotdng, Base, "state", Value) :- triple(Base, "todo://state", Value).
property_setter(cjdotdng, "state", '[{action: "setSingleTarget", source: "this", predicate: "todo://state", target: "value"}]').

property(cjdotdng, "title").
property_resolve(cjdotdng, "title").
property_resolve_language(cjdotdng, "title", "literal").
property_getter(cjdotdng, Base, "title", Value) :- triple(Base, "todo://has_title", Value).
property_setter(cjdotdng, "title", '[{action: "setSingleTarget", source: "this", predicate: "todo://has_title", target: "value"}]').

collection(cjdotdng, "comments").
collection_getter(cjdotdng, Base, "comments", List) :- findall(C, triple(Base, "todo://comment", C), List).
collection_adder(cjdotdng, "comments", '[{action: "addLink", source: "this", predicate: "todo://comment", target: "value"}]').
collection_setter(cjdotdng, "comments", '[{action: "collectionSetter", source: "this", predicate: "todo://comment", target: "value"}]').
:ANDS

What icon should it have in the toolbar? (You can choose from the "Bootstrap" icon set) 
"##;


            use std::io::Write;
            use llm::Model;
            use llm::InferenceResponse;
            use std::convert::Infallible;

            println!("Loading model...");

            
            // load a GGML model from disk
            let llama = llm::load::<llm::models::Llama>(
                // path to GGML file
                std::path::Path::new("/Users/nicolasluck/models/eve.model"),
                llm::TokenizerSource::Embedded,
                // llm::ModelParameters
                Default::default(),
                // load progress callback
                |_| {},

            )
            .unwrap_or_else(|err| panic!("Failed to load model: {err}"));

            println!("Model loaded!");

            /*

            
            let llama = llm::load_dynamic(
                Some(llm::ModelArchitecture::Llama),
                &std::path::Path::new("/Users/nicolasluck/eve.model"),
                llm::TokenizerSource::Embedded,
                Default::default(),
                llm::load_progress_callback_stdout,
            ).unwrap_or_else(|err| panic!("Failed to load model: {err}"));

            println!("Model loaded!");
             */

            let mut session = llama.start_session(Default::default());
            

            println!("Ad hoc training model for ADAM subject classes...");


            fn print_token(t: String) {
                print!("{t}");
                std::io::stdout().flush().unwrap();
            }


            let inference_parameters = llm::InferenceParameters::default();

            
            // use the model to generate text from a prompt
            
            session.feed_prompt(
                &llama,
                format!("{system}\n{reduced}").as_str(),
                &mut Default::default(),
                llm::feed_prompt_callback(|resp| match resp {
                    llm::InferenceResponse::PromptToken(t)
                    | llm::InferenceResponse::InferredToken(t) => {
                        print_token(t);
    
                        Ok::<llm::InferenceFeedback, Infallible>(llm::InferenceFeedback::Continue)
                    }
                    _ => Ok(llm::InferenceFeedback::Continue),
                }),
            ).unwrap_or_else(|err| panic!("Failed to feed prompt: {err}"));

            println!("\n\nTraining done. Ready!");

            let mut rl = rustyline::Editor::<()>::new()?;
            let line = rl.readline(">> ")?;
            println!("\n\n");
                
            let res = session.infer::<std::convert::Infallible>(
                // model to use for text generation
                &llama,
                // randomness provider
                &mut rand::thread_rng(),
                // the prompt to use for text generation, as well as other
                // inference parameters
                &llm::InferenceRequest {
                    prompt: format!("User: {line}\nEve:")
                        .as_str()
                        .into(),
                    parameters: &inference_parameters,
                    play_back_previous_tokens: false,
                    maximum_token_count: None,
                },
                // llm::OutputRequest
                &mut Default::default(),
                // output callback
                |t| {
                    match t {
                        InferenceResponse::PromptToken(t) | InferenceResponse::InferredToken(t) | llm::InferenceResponse::SnapshotToken(t) => {
                            print_token(t);
                        }
                        _ => {}
                    }
                    std::io::stdout().flush().unwrap();

                    Ok(llm::InferenceFeedback::Continue)
                }
            );

            match res {
                Ok(result) => println!("\n\nInference stats:\n{result}"),
                Err(err) => println!("\n{err}"),
            }
        }
    }

    Ok(())
}
