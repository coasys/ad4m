use scryer_prolog::machine::Machine;
use scryer_prolog::machine::streams::*;


pub fn run() {
    std::thread::spawn(|| {
        
        let mut machine = Machine::with_test_streams();
        
        let facts = String::from(r#"
triple("a", "p1", "b").
triple("a", "p2", "b").
"#);

        machine.load_module_string("facts", facts);
        println!("{}", machine.get_user_output());
        println!("Facts loaded");
        
        
        
        //let query = String::from("triple(A,B,C), write(\"A = \"), write(A), nl, write(\"B = \"), write(B), write(\"C = \"), write(C), nl ; write(\"no triple matched\").\n");
        let query = String::from("triple(\"a\",P,\"b\").");
        //let query = String::from("write(\"A = \").");
        //let query = String::from("halt.\n");
        println!("Running query: {}", query);
        let output = machine.run_query(query);
        println!("Output: {:?}", output);

        let query = String::from("triple(\"a\",\"p1\",\"b\").");
        //let query = String::from("write(\"A = \").");
        //let query = String::from("halt.\n");
        println!("Running query: {}", query);
        let output = machine.run_query(query);
        println!("Output: {:?}", output);
    });
}   