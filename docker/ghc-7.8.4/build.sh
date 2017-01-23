readonly DOCKERROOT=$( cd $( dirname $0 ); pwd )

. "$DOCKERROOT/setup.sh"

docker build -t ghc-7.8.4 "$DOCKERROOT"
