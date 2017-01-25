set -x
set -e

STACK_GIT_LOC="$HOME/.stack_build"

if which stack && [[ $( stack --version | head -1 ) == 'Version 1.3.3'* ]]
then
    echo 'Found stack already installed:'
    echo $( which stack )
    stack --version | head -1
    exit 0
fi

mkdir -p "$STACK_GIT_LOC"
cd "$STACK_GIT_LOC"

if [[ -e '.git' ]]
then
    git pull
    cabal update
    cabal install
else
    git clone https://github.com/commercialhaskell/stack .
    cabal update
    cabal install
fi

hash -r

echo 'Installed stack:'
echo $( which stack )
stack --version | head -1
