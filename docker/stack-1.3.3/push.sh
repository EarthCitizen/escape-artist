set -e

readonly DOCKERROOT=$( cd $( dirname $0 ); pwd )

. "$DOCKERROOT/common.sh"

docker push "$IMAGENAMEVER"
