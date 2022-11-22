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