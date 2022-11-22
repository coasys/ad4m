setup() {
    load 'test_helper/bats-support/load'
    load 'test_helper/bats-assert/load'

    rm -rf ./tests/ad4m1

    ./host/dist/ad4m-macos-x64 init --dataPath ./tests/ad4m1
    ./host/dist/ad4m-macos-x64 serve --dataPath ./tests/ad4m1 &
}

teardown() {
    killall ad4m-macos-x64
}

@test "can run ad4m-host" {
    ./host/dist/ad4m-macos-x64
}

get_ad4m_cli_banner() {
    ./target/release/ad4m 2>&1
}

@test "can run ad4m-cli" {
    run get_ad4m_cli_banner
    assert_output --partial 'AD4M command line interface'
}

@test "can create perspective, add and query links" {
    sleep 5

    ./target/release/ad4m -n -e http://localhost:4000/graphql agent generate --passphrase "secret"
    ./target/release/ad4m -n -e http://localhost:4000/graphql agent unlock --passphrase "secret"
    ./target/release/ad4m -n -e http://localhost:4000/graphql agent perspectives add --name "test"
    ./target/release/ad4m -n -e http://localhost:4000/graphql agent perspectives
}