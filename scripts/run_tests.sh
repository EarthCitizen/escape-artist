readonly STACK_YAML_NAME='tmp_stack.yaml'

# set -x
set -e

trap "exit" INT

readonly OS=$( uname )
readonly MAC='Darwin'

function get_lts_versions() {
    (
        if [[ $OS != $MAC ]]
        then
            echo 2.{0..5}
        fi
        echo 3.{0..5} \
             4.{0..2} \
             5.{0..5} \
             6.{0..5} \
             7.{0..5}
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

function run_default_test() {
    print_test_header "(stack.yaml)"
    unset STACK_YAML
    stack --no-terminal setup > /dev/null
    stack --no-terminal clean > /dev/null
    stack --no-terminal build --test --coverage
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

for version in $( get_lts_versions )
do
    run_lts_test $version
done

run_default_test
