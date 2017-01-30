readonly DOCKERROOT=$( cd $( dirname $0 ); pwd )

. "$DOCKERROOT/common.sh"

docker run -t -v "$PROJCOPY:/work" --entrypoint /bin/bash rdgithub/stack-1.3.3:20170130 scripts/run_tests.sh range 2 0 4
