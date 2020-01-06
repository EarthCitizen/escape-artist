# set -x
set -e

readonly SCRIPTROOT=$( cd $( dirname $0 ); pwd )
readonly PROJROOT=$( cd $SCRIPTROOT/..; pwd )
readonly VAR_STACK_YAML="$PROJROOT/stack.yaml"
readonly STACK_OPTS='--allow-different-user --no-terminal'

function exit_int() {
    exit 1
}

trap 'exit_int' INT

readonly OS=$( uname )
readonly MAC='Darwin'

function usage() {
    echo 'Usage:'
    echo
    echo "$( basename $0 ) <all | default | range <major> <minor_from> <minor_to>>"
    exit 1
}

function get_lts_versions_range() {
    ( eval echo "$1.{$2..$3}" ) | tr ' ' '\n'
}

function get_lts_versions_all() {
    (
        if [[ $OS != $MAC ]]
        then
            echo 2.{0..22}
        fi
        echo 3.{0..22}  \
             4.{0..2}   \
             5.{0..18}  \
             6.{0..30}  \
             7.{0..19}  \
             8.{0..24}  \
             9.{0..21}  \
             10.{0..10} \
             11.{0..22} \
             12.{0..26} \
             13.{0..30} \
             14.{0..20}
    ) | tr ' ' '\n'
}

function print_test_header() {
    echo
    echo
    echo '============================================='
    echo
    echo "     Running Test for LTS $1"
    echo
    echo '============================================='
    echo
    echo
}

function run_test_lts() {
    local -r LTS_VERSION="lts-$1"
    print_test_header "$1"
    STACK_YAML="$VAR_STACK_YAML"
    export STACK_YAML
    stack $STACK_OPTS --resolver "$LTS_VERSION" setup > /dev/null
    stack $STACK_OPTS --resolver "$LTS_VERSION" clean > /dev/null
    stack $STACK_OPTS --resolver "$LTS_VERSION" build --test
}

function run_tests_default() {
    print_test_header "(stack.yaml)"
    unset STACK_YAML
    stack $STACK_OPTS setup > /dev/null
    stack $STACK_OPTS clean > /dev/null
    stack $STACK_OPTS build --test --coverage
    if [[ ! -z "$COVERALLS_TOKEN" ]]
    then
        curl -L https://github.com/rubik/stack-hpc-coveralls/releases/download/v0.0.4.0/shc-linux-x64-8.0.1.tar.bz2 | tar -xj
        ./shc --partial-coverage "--repo-token=$COVERALLS_TOKEN" escape-artist escape-artist-spec-test
    fi
}

function run_tests_lts() {
    for version in $@
    do
        run_test_lts $version
    done
}

case "$1" in
    all)
        [[ $# -ne "1" ]] && usage
        run_tests_lts $( get_lts_versions_all )
        ;;
    default)
        [[ $# -ne "1" ]] && usage
        run_tests_default
        ;;
    range)
        [[ $# -ne "4" ]] && usage
        run_tests_lts $( get_lts_versions_range "$2" "$3" "$4" )
        ;;
    *)
        usage
        ;;
esac
