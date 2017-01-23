readonly DOCKERROOT=$( cd $( dirname $0 ); pwd )

. "$DOCKERROOT/setup.sh"

docker run -i -t -v "$PROJCOPY:/escape-artist" ghc-7.8.4
