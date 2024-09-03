use anyhow::Result;
use clap::Subcommand;
use llm::InferenceResponse;
use llm::InferenceSession;
use llm::Model;
use std::convert::Infallible;
use std::fs::File;
use std::io::Write;

#[derive(Debug, Subcommand)]
pub enum EveCommands {
    Train,
    Prompt,
}

const SYSTEM: &str = r##"
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

const HISTORY1: &str = r##"
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

What icon should it have in the toolbar? (You can choose from the "Bootstrap" icon set)"##;

const HISTORY2: &str = r##"
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

Anything else?"##;
const HISTORY3: &str = r##"            
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

const REDUCED: &str = r##"
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

pub async fn run(command: EveCommands) -> Result<()> {
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

    let inference_parameters = llm::InferenceParameters::default();

    fn print_token(t: String) {
        print!("{t}");
        std::io::stdout().flush().unwrap();
    }

    match command {
        EveCommands::Train => {
            println!("Training Eve...");

            println!("Ad hoc training model for ADAM subject classes...");

            // use the model to generate text from a prompt
            let mut session = llama.start_session(Default::default());

            for p in vec![SYSTEM, HISTORY1, HISTORY2] {
                session
                    .feed_prompt(
                        &llama,
                        p,
                        &mut Default::default(),
                        llm::feed_prompt_callback(|resp| match resp {
                            llm::InferenceResponse::PromptToken(t)
                            | llm::InferenceResponse::InferredToken(t) => {
                                print_token(t);

                                Ok::<llm::InferenceFeedback, Infallible>(
                                    llm::InferenceFeedback::Continue,
                                )
                            }
                            _ => Ok(llm::InferenceFeedback::Continue),
                        }),
                    )
                    .unwrap_or_else(|err| panic!("Failed to feed prompt: {err}"));
            }

            println!("\n\nTraining done. Ready!");

            unsafe {
                let snapshot: llm::InferenceSnapshotRef<'_> = session.get_snapshot();
                let snapshot_cbor =
                    serde_cbor::to_vec(&snapshot).expect("Failed to serialize snapshot");
                File::create("/Users/nicolasluck/models/eve.snapshot.json")
                    .unwrap()
                    .write_all(snapshot_cbor.as_slice())
                    .unwrap();
                println!("Snapshot saved!");
            }
        }
        EveCommands::Prompt => {
            println!("Prompting Eve...");

            let mut session =
                if let Ok(file) = File::open("/Users/nicolasluck/models/eve.snapshot.json") {
                    let snapshot: llm::InferenceSnapshot =
                        serde_cbor::from_reader(file).expect("Failed to deserialize snapshot");
                    InferenceSession::from_snapshot(snapshot, &llama)
                        .unwrap_or_else(|err| panic!("Failed to load snapshot: {err}"))
                } else {
                    llama.start_session(Default::default())
                };

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
                    prompt: format!("User: {line}\nEve:").as_str().into(),
                    parameters: &inference_parameters,
                    play_back_previous_tokens: false,
                    maximum_token_count: None,
                },
                // llm::OutputRequest
                &mut Default::default(),
                // output callback
                |t| {
                    let mut cont = true;
                    match t {
                        InferenceResponse::PromptToken(t)
                        | InferenceResponse::InferredToken(t)
                        | llm::InferenceResponse::SnapshotToken(t) => {
                            if t == "Eve:" || t == "User:" {
                                cont = false;
                            }
                            print_token(t);
                        }
                        _ => {}
                    }
                    std::io::stdout().flush().unwrap();

                    if cont {
                        Ok(llm::InferenceFeedback::Continue)
                    } else {
                        Ok(llm::InferenceFeedback::Halt)
                    }
                },
            );

            match res {
                Ok(result) => println!("\n\nInference stats:\n{result}"),
                Err(err) => println!("\n{err}"),
            }
        }
    }
    Ok(())
}
