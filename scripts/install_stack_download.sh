set -x
set -e

if which stack
then
    echo 'Found stack already installed:'
    echo $( which stack )
    stack --version | head -1
    exit 0
fi

curl -L https://www.stackage.org/stack/linux-x86_64 | \
tar xz --wildcards --strip-components=1 -C $HOME/.local/bin '*/stack'
