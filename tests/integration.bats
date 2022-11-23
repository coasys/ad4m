setup_file() {
    echo "Creating test agent 1" >&3
    echo "Initalizing data directory..." >&3
    rm -rf ./tests/ad4m1
    ./host/dist/ad4m-macos-x64 init --dataPath ./tests/ad4m1
    echo "done." >&3
    echo "Starting agent..." >&3
    ./host/dist/ad4m-macos-x64 serve --dataPath ./tests/ad4m1 &
    sleep 5
    echo "done." >&3

    echo "Generating keys and initializing agent..." >&3
    ./target/release/ad4m -n -e http://localhost:4000/graphql agent generate --passphrase "secret"
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