readonly SETUPROOT=$( cd $( dirname $0 ); pwd )
readonly STACK_YAML="$SETUPROOT/stack.yaml"

cd "$SETUPROOT"

function get_resolver() {
    egrep '^[[:space:]]*resolver' "$STACK_YAML" | sed -E  's/[[:space:]]+//g' | cut -d ':' -f 2
}


function get_dependencies() {
    stack list-dependencies --test | grep -v escape-artist | tr ' ' '-'
}

set -x

stack setup --resolver $( get_resolver )
stack build --resolver $( get_resolver ) $( get_dependencies )
