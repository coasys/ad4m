setup_file() {
    current_dir=$(pwd)
    echo "Creating test agent 1" >&3
    echo "Initalizing data directory..." >&3
    rm -rf ./tests/ad4m1
    ./target/release/ad4m init --data-path ${current_dir}/tests/ad4m1
    echo "done." >&3
    echo "Starting agent 1..." >&3
    ./target/release/ad4m run --app-data-path ${current_dir}/tests/ad4m1 --gql-port 4000 &
    sleep 5
    echo "done." >&3

    echo "Generating keys and initializing agent..." >&3
    ./target/release/ad4m -n -e http://localhost:4000/graphql agent generate --passphrase "secret"
    echo "done." >&3


    #echo "Creating test agent 2" >&3
    #echo "Initalizing data directory..." >&3
    #rm -rf ./tests/ad4m2
    #./target/release/ad4m init --data-path ${current_dir}/tests/ad4m2
    #echo "done." >&3
    #echo "Starting agent 2..." >&3
    #./target/release/ad4m run --app-data-path ${current_dir}/tests/ad4m2 --gql-port 4001 --ipfs-swarm-port 15000 --hc-admin-port 2337 --hc-app-port 2338 &
    #sleep 5
    #echo "done." >&3
    
    #echo "Generating keys and initializing agent..." >&3
    #./target/release/ad4m -n -e http://localhost:4001/graphql agent generate --passphrase "secret"
    #echo "done." >&3
}

teardown_file() {
    killall ad4m
}

setup() {
    load 'test_helper/bats-support/load'
    load 'test_helper/bats-assert/load'
}

@test "can create perspective, add and query links" {
    perspective_id=`./target/release/ad4m -n -e http://localhost:4000/graphql perspectives add "test"`

    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives
    assert_output --partial $perspective_id

    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives add-link $perspective_id "test://source" "test://target" "test://predicate"
    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives query-links $perspective_id
    assert_output --partial "test://source"
}

@test "can use subjects and run potluck example sdna" {
    skip
    # Create perspective
    perspective_id=`./target/release/ad4m -n -e http://localhost:4000/graphql perspectives add "sdna subject test"`
    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives set-dna $perspective_id ./tests/potluck.pl
    assert_output --partial "SDNA set successfully"

    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-classes $perspective_id
    assert_output --partial "Dish"
    assert_output --partial "Potluck"

    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-construct $perspective_id "Dish" "test://dish1"
    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-get-property $perspective_id "test://dish1" "kind"
    assert_output --partial "liminal://unknown_kind"

    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-set-property $perspective_id "test://dish1" "kind" "liminal://pizza"

    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-construct $perspective_id "Dish" "test://dish2"
    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-set-property $perspective_id "test://dish2" "kind" "liminal://pasta"

    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-construct $perspective_id "Dish" "test://dish3"
    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-set-property $perspective_id "test://dish3" "kind" "liminal://pizza"

    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-construct $perspective_id "Potluck" "test://potluck"
    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-add-collection $perspective_id "test://potluck" "dishes" "test://dish1"
    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-add-collection $perspective_id "test://potluck" "dishes" "test://dish2"
    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-add-collection $perspective_id "test://potluck" "dishes" "test://dish3"

    # Dish1 is the first pizza, so it should be the most unique
    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-get-property $perspective_id "test://dish1" "uniqueness"
    assert_output --partial "1"

    # Dish2 is the only pasta, so it should be the most unique
    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-get-property $perspective_id "test://dish2" "uniqueness"
    assert_output --partial "1"

    # Dish3 is the second pizza, so it gets a score of 0.5
    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-get-property $perspective_id "test://dish3" "uniqueness"
    assert_output --partial "0.5"

    # Let's add a fourth dish 
    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-construct $perspective_id "Dish" "test://dish4"
    # Also a pizze
    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-set-property $perspective_id "test://dish4" "kind" "liminal://pizza"
    # As long as it's on it's own, it should be the most unique
    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-get-property $perspective_id "test://dish4" "uniqueness"
    assert_output --partial "1"
    # But when we add it to the potluck, it is now the 3rd pizza, so it's score should be 0.3333333333333333
    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-add-collection $perspective_id "test://potluck" "dishes" "test://dish4"
    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-get-property $perspective_id "test://dish4" "uniqueness"
    assert_output --partial "0.3333333333333333"

    # But the new pizza should not have changed the score of the ones already in:
    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-get-property $perspective_id "test://dish1" "uniqueness"
    assert_output --partial "1"
    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives subject-get-property $perspective_id "test://dish3" "uniqueness"
    assert_output --partial "0.5"

}

@test "can create neighbourhood, join and share links" {
    skip
    # Create perspective
    perspective_id=`./target/release/ad4m -n -e http://localhost:4000/graphql perspectives add "neighbourhood test"`

    # Create unique link language clone
    link_language_template=`./target/release/ad4m -n -e http://localhost:4000/graphql runtime link-language-templates`
    clone_output=`./target/release/ad4m -n -e http://localhost:4000/graphql languages apply-template-and-publish $link_language_template "{\"uid\":\"test\",\"name\":\"test\",\"description\":\"test\"}"`

    run echo $clone_output
    assert_output --partial "Language template applied and published!"
    assert_line --partial "Address:"
    address_line=`echo $clone_output | grep "Address:"`
    language_address=`echo $address_line | cut -d " " -f 9-`

    # Publish neighbourhood
    neighbourhood_output=`./target/release/ad4m -n -e http://localhost:4000/graphql neighbourhoods create $perspective_id $language_address`
    run echo $neighbourhood_output
    assert_output --partial "Neighbourhood shared as:"

    nh_url=`echo $neighbourhood_output | cut -d " " -f 4`


    # Join neighbourhood
    run ./target/release/ad4m -n -e http://localhost:4001/graphql neighbourhoods join $nh_url
    assert_line --partial "Neighbourhood joined!"

    # Add link
    run ./target/release/ad4m -n -e http://localhost:4000/graphql perspectives add-link $perspective_id "nh_test://source" "nh_test://target" "nh_test://predicate"

    # Query link
    perspectives_output=`./target/release/ad4m -n -e http://localhost:4001/graphql perspectives`
    run echo $perspectives_output
    assert_line --partial "ID:"
    perspective_id_line=`echo $perspectives_output | grep "ID:"`
    perspective_id_2=`echo $perspective_id_line | cut -d " " -f 2`

    perspective_link_output=`run ./target/release/ad4m -n -e http://localhost:4001/graphql perspectives query-links $perspective_id_2`
    run echo $perspective_link_output
    assert_line --partial "nh_test://source"
}

@test "can create and get expressions, using note-ipfs language" {
    skip
    wget https://github.com/perspect3vism/lang-note-ipfs/releases/download/0.0.4/bundle.js -O ./tests/note-ipfs.js
    pwd=`pwd`
    publish_output=`./target/release/ad4m -n -e http://localhost:4000/graphql languages publish $pwd/tests/note-ipfs.js -n "note-ipfs" -d "Stores text expressions in IPFS" -p "", -s "https://github.com/perspect3vism/lang-note-ipfs"`
    echo $publish_output
    run echo $publish_output
    assert_line --partial "Language published with address:"
    language_address=`echo $publish_output | cut -d ":" -f 2`
    echo "Language address: $language_address"
    
    create_output=`./target/release/ad4m -n -e http://localhost:4000/graphql expression create $language_address "test content"`
    echo $create_output
    run echo $create_output
    assert_line --partial "Expression created with url:"
    expression_url=`echo $create_output | awk '{print substr($0,30)}'`
    
    run ./target/release/ad4m -n -e http://localhost:4000/graphql expression get-raw $expression_url
    assert_output --partial "test content"
}