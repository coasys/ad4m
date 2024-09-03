
use std::io::Write;
use anyhow::Result;
use clap::Subcommand;
use kalosm::language::*;

#[derive(Debug, Subcommand)]
pub enum EveCommands {
    Train,
    Prompt,
}

#[derive(Clone, Debug, Parse, Schema)]
pub struct Response {
    topics: Vec<String>,
    changed_subject: String,
    new_subgroup_name: String,
    new_subgroup_summary: String,
    new_conversation_name: String,
    new_conversation_summary: String
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


const EXAMPLE_USER1: &str = r##"
Create an ADAM Subject class called "Todo". It should have a "state" and a "title" property. The title should resolve to a string applying the literal language. It also should have a "comments" collection.` 
Eve: Sure. I have made-up a "todo" ADAM Language to use in the predicates of this Subject class definition.
"##;

const EXAMPLE_EVE1: &str = r##"
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
"##;

const EXAMPLE_USER2: &str = r##"
The "check" icon. Render todos as circles with the color depending on the state. Also, please add options for the state property, being Ready, Doing and Done.
"##;

const EXAMPLE_EVE2: &str = r##"
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
"##;

const EXAMPLE_USER3: &str = r##"
No, that's it. Please write this to the SDNA.
"##;

const EXAMPLE_EVE3: &str = r##"
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

    let llama = Llama::new_chat().await.unwrap();


    println!("Model loaded");

    let task  = Task::builder(SYSTEM)
        .with_example(EXAMPLE_USER1, EXAMPLE_EVE1)
        .with_example(EXAMPLE_USER2, EXAMPLE_EVE2)
        .with_example(EXAMPLE_USER3, EXAMPLE_EVE3)
        .build();

    println!("Feeding examples...");
    task.run("Hello, let's pretend this is a new session. Who are you?", &llama).to_std_out().await.unwrap();

    match command {
        EveCommands::Train => {
            println!("Training Eve...");

            println!("Ad hoc training model for ADAM subject classes...");
        }
        EveCommands::Prompt => {
            println!("Prompting Eve...");


            let mut rl = rustyline::Editor::<()>::new()?;
            let line = rl.readline(">> ")?;
            task.run(line, &llama).to_std_out().await.unwrap();

            // let value: String = stream.all_text().await;

            // println!("Result: {:?}", value);

            println!("\n\n");
        }
    }
    Ok(())
}
