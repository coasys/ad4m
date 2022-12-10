setup_file() {
    echo "Creating test agent 1" >&3
    echo "Initalizing data directory..." >&3
    rm -rf ./tests/ad4m1
    ./host/dist/ad4m-macos-x64 init --dataPath ./tests/ad4m1
    echo "done." >&3
    echo "Starting agent 1..." >&3
    ./host/dist/ad4m-macos-x64 serve --dataPath ./tests/ad4m1 &
    sleep 5
    echo "done." >&3

    echo "Generating keys and initializing agent..." >&3
    ./target/release/ad4m -n -e http://localhost:4000/graphql agent generate --passphrase "secret"
    echo "done." >&3


    echo "Creating test agent 2" >&3
    echo "Initalizing data directory..." >&3
    rm -rf ./tests/ad4m2
    ./host/dist/ad4m-macos-x64 init --dataPath ./tests/ad4m2
    echo "done." >&3
    echo "Starting agent 2..." >&3
    ./host/dist/ad4m-macos-x64 serve --dataPath ./tests/ad4m2 --port 4001 --ipfsPort 15000 --hcAdminPort 2337 --hcAppPort 2338 &
    sleep 5
    echo "done." >&3
    
    echo "Generating keys and initializing agent..." >&3
    ./target/release/ad4m -n -e http://localhost:4001/graphql agent generate --passphrase "secret"
    echo "done." >&3
}

teardown_file() {
    killall ad4m-macos-x64
    killall holochain
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

@test "can create neighbourhood, join and share links" {
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