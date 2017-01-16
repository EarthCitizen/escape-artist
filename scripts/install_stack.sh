set -x

STACK_GIT_LOC="$HOME/.stack_build"

if which stack && [[ $( stack --version ) == 'Version 1.3.3 '* ]]
then
    echo 'Found stack already installed:'
    echo $( which stack )
    stack --version
    exit 0
fi

if [[ -d "$STACK_GIT_LOC" ]]
then
    cd "$STACK_GIT_LOC"
    git pull
    cabal update
    cabal install
else
    git clone https://github.com/commercialhaskell/stack "$STACK_GIT_LOC"
    cd "$STACK_GIT_LOC"
    cabal update
    cabal install
fi

hash -r

echo 'Installed stack:'
echo $( which stack )
stack --version
