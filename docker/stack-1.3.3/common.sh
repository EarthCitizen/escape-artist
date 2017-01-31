readonly PROJROOT=$( cd $DOCKERROOT/../..; pwd )
readonly PROJCOPY="$DOCKERROOT/.project-copy"
readonly IMAGENAMEVER='rdgithub/stack-1.3.3:20170130'

function make_project_copy() {
    echo PROJROOT=$PROJROOT
    echo DOCKERROOT=$DOCKERROOT
    echo PROJCOPY=$PROJCOPY

    if [[ ! -e "$PROJCOPY" ]]
    then
        mkdir -p "$PROJCOPY"
    fi

    rm -rf "$PROJCOPY"/*

    cp -R "$PROJROOT/src"       \
          "$PROJROOT/scripts"   \
          "$PROJROOT/test"      \
          "$PROJROOT/LICENSE"   \
          "$PROJROOT/escape-artist.cabal"  \
          "$PROJCOPY"

    cp "$PROJROOT/stack-ghc-7.8.4.yaml" "$PROJCOPY/stack-ghc-7.8.4.yaml"
    cp "$PROJROOT/stack-ghc-7.8.4.yaml" "$PROJCOPY/stack.yaml"
}
