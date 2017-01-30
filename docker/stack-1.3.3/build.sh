readonly DOCKERROOT=$( cd $( dirname $0 ); pwd )

. "$DOCKERROOT/common.sh"

docker build -t earthcitizen-docker-main.bintray.io/haskell/stack-1.3.3 "$DOCKERROOT"
