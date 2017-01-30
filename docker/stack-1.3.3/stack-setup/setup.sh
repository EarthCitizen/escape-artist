readonly SETUPROOT=$( cd $( dirname $0 ); pwd )

function get_dependencies() {
    ( cd "$SETUPROOT"; stack setup --resolver $1; stack list-dependencies --resolver $1 --test ) | grep -v escape-artist | tr ' ' '-'
}

set -x

cd /

for resolver in lts-2.0 lts-3.0 lts-4.0 lts-5.0 lts-6.0 lts-7.0
do
    stack setup --resolver $resolver
    stack build --resolver $resolver $( get_dependencies $resolver )
done
