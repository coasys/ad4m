use scryer_prolog::machine::Machine;
use scryer_prolog::machine::streams::*;


pub fn run() {
    // Create a new Scryer Prolog machine
    let mut machine = Machine::with_test_streams();
    let machine_st = machine.prelude_view_and_machine_st().1;
    //let &mut arena = &mut ;

    // Example Prolog query to be executed
    let query = String::from("?- append([1, 2], [3, 4], X).");
    
    let stream = Stream::from_owned_string(
        query,
        &mut machine_st.arena,
    );

    machine.load_file(String::from("query.pl").as_str(), stream);
    //let result: Vec<u8> = machine.write_to_vec();

    //println!("Prolog Result: {:?}", result);
}
