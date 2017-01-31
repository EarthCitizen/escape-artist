set -e

readonly DOCKERROOT=$( cd $( dirname $0 ); pwd )

. "$DOCKERROOT/common.sh"

make_project_copy

docker run -t -v "$PROJCOPY:/work" --entrypoint /bin/bash "$IMAGENAMEVER" scripts/run_tests.sh all
