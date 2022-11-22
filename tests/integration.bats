setup() {
    load 'test_helper/bats-support/load'
    load 'test_helper/bats-assert/load'
}

@test "can run ad4m-host" {
    ./host/dist/ad4m-macos-x64
}

get_ad4m_cli_banner() {
    ./cli/target/debug/ad4m 2>&1
}

@test "can run ad4m-cli" {
    run get_ad4m_cli_banner
    assert_output --partial 'AD4M command line interface'
}

@test "can create perspective, add and query links" {
    ./host/dist/ad4m-macos-x64 init --dataPath ./tests/ad4m1
    ./host/dist/ad4m-macos-x64 serve --dataPath ./tests/ad4m1 &

    sleep 5

    ./cli/target/debug/ad4m -n -e http://localhost:4000 agent generate --passphrase "secret"
    ./cli/target/debug/ad4m -n -e http://localhost:4000 agent unlock --passphrase "secret"
    ./cli/target/debug/ad4m -n -e http://localhost:4000 agent perspectives add --name "test"
    ./cli/target/debug/ad4m -n -e http://localhost:4000 agent perspectives
}