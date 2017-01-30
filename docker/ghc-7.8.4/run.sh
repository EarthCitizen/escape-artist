set -e

readonly DOCKERROOT=$( cd $( dirname $0 ); pwd )

. "$DOCKERROOT/common.sh"

make_project_copy

docker run -it -v "$PROJCOPY:/escape-artist" "$IMAGENAMEVER"
