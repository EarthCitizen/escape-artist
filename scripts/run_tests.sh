readonly STACK_YAML_NAME='tmp_stack.yaml'

set -x
set -e

function exit_clean() {
    echo 'exit clean'
    rm -f "$STACK_YAML_NAME"
    exit 1
}

trap 'exit_clean' INT

readonly OS=$( uname )
readonly MAC='Darwin'


function usage() {
    echo 'Usage:'
    echo
    echo "$( basename $0 ) <all | default | version <range>>"
    exit 1
}

function get_lts_versions_param() {
    ( eval "echo $1" ) | tr ' ' '\n'
}

function get_lts_versions_all() {
    (
        if [[ $OS != $MAC ]]
        then
            echo 2.{0..22}
        fi
        echo 3.{0..22} \
             4.{0..2} \
             5.{0..18} \
             6.{0..27} \
             7.{0..15}
    ) | tr ' ' '\n'
}

function print_test_header() {
    local -r LTS_VERSION="$1"
    echo
    echo
    echo '============================================='
    echo
    echo "     Running Test for LTS ${LTS_VERSION}"
    echo
    echo '============================================='
    echo
    echo
}

function run_lts_test() {
    local -r LTS_VERSION="$1"
    print_test_header "$LTS_VERSION"
    make_tmp_lts_stack_yaml "$LTS_VERSION"
    STACK_YAML="$STACK_YAML_NAME"
    export STACK_YAML
    stack --no-terminal setup > /dev/null
    stack --no-terminal clean > /dev/null
    stack --no-terminal build --test
    local -r RES=$?
    rm "$STACK_YAML_NAME"
    return $RES
}

function run_tests_default() {
    print_test_header "(stack.yaml)"
    unset STACK_YAML
    stack --no-terminal setup > /dev/null
    stack --no-terminal clean > /dev/null
    stack --no-terminal build --test --coverage
    if [[ ! -z "$COVERALLS_TOKEN" ]]
    then
        curl -L https://github.com/rubik/stack-hpc-coveralls/releases/download/v0.0.4.0/shc-linux-x64-8.0.1.tar.bz2 | tar -xj
        ./shc --partial-coverage "--repo-token=${COVERALLS_TOKEN}" escape-artist escape-artist-spec-test
    fi
}

function run_lts_tests_all() {
    for version in $( get_lts_versions_all )
    do
        run_lts_test $version
    done
}

function run_lts_tests_param() {
    for version in $( get_lts_versions_param "$1" )
    do
        run_lts_test $version
    done
}

function make_tmp_lts_stack_yaml() {
local -r LTS_VERSION="$1"
printf "
resolver: lts-${LTS_VERSION}
packages:
- '.'
extra-deps:
- hspec-2.2.4
- QuickCheck-2.9.2
- silently-1.2.5
- hspec-expectations-0.7.2
- hspec-core-2.2.4
- hspec-discover-2.2.4
- quickcheck-io-0.1.4
" > "$STACK_YAML_NAME"
}

case "$1" in
    all)
        [[ $# -ne "1" ]] && usage
        run_lts_tests_all
        ;;
    default)
        [[ $# -ne "1" ]] && usage
        run_tests_default
        ;;
    version)
        [[ $# -ne "2" ]] && usage
        run_lts_tests_param "$2"
        ;;
    *)
        usage
        ;;
esac
