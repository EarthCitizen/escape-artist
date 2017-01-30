set -e

readonly DOCKERROOT=$( cd $( dirname $0 ); pwd )

. "$DOCKERROOT/common.sh"

make_project_copy

docker build -t "$IMAGENAMEVER" "$DOCKERROOT"
