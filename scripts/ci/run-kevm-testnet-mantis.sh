#!/usr/bin/env bash

set -euxo pipefail

tmpFile=$(mktemp -t aws_ecr_login.XXXXXX)

function cleanup() {
  if test -e $tmpFile; then
    rm $tmpFile
  fi
}
trap cleanup EXIT ERR


# We assume `awscli` is set up.
# Authentication token for the ECR registry expires, so we log into the registry again.
echo '#!/usr/bin/env bash' > $tmpFile
echo >> $tmpFile
aws ecr get-login --region eu-west-1 --no-include-email >> $tmpFile
chmod +x $tmpFile
$tmpFile

docker pull 920648890259.dkr.ecr.eu-west-1.amazonaws.com/mantis-base:latest
docker tag 920648890259.dkr.ecr.eu-west-1.amazonaws.com/mantis-base:latest mantis-base:latest

docker pull 920648890259.dkr.ecr.eu-west-1.amazonaws.com/mantis-dev:latest
docker tag 920648890259.dkr.ecr.eu-west-1.amazonaws.com/mantis-dev:latest mantis-dev:latest

./docker/build.sh

IMAGE_TAG=$(git log -1 --format=%cd.%h --date=short)

docker tag mantis:latest 920648890259.dkr.ecr.eu-west-1.amazonaws.com/mantis:${IMAGE_TAG}
docker push 920648890259.dkr.ecr.eu-west-1.amazonaws.com/mantis:${IMAGE_TAG}
docker tag mantis:latest 920648890259.dkr.ecr.eu-west-1.amazonaws.com/mantis:latest
docker push 920648890259.dkr.ecr.eu-west-1.amazonaws.com/mantis:latest
