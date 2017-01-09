readonly STACK_YAML_NAME='tmp_stack.yaml'

set -x
set -e

trap "exit" INT

function get_lts_versions() {
    (
        if [[ $( uname ) != 'Darwin' ]]
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

function run_test() {
    make_stack_yaml $1
    STACK_YAML="${STACK_YAML_NAME}"
    export STACK_YAML
    stack clean
    stack setup
    stack test
    local -r RES=$?
    rm "${STACK_YAML_NAME}"
    return $?
}

function make_stack_yaml() {
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
" > "${STACK_YAML_NAME}"
}

for version in $( get_lts_versions )
do
    run_test $version
done
