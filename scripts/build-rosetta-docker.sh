#!/usr/bin/env bash

# Author's Note: Because the structure of this repo is inconsistent (Dockerfiles and build contexts placed willy-nilly)
# we have to trustlist and configure image builds individually because each one is going to be slightly different.
# This is needed as opposed to trusting the structure of the each project to be consistent for every deployable.

set -eo pipefail

CLEAR='\033[0m'
RED='\033[0;31m'
function usage() {
  if [[ -n "$1" ]]; then
    echo -e "${RED}☞  $1${CLEAR}\n";
  fi
  echo "Usage: $0 [-s service-to-release] [-v service-version] [-n network]"
  echo "  -b, --base-image-version  The base image version to use for the docker image"
  echo "  -v, --version             The version to be used in the docker image tag"
  echo "  -n, --network             The network configuration to use (devnet or mainnet). Default=devnet"
  echo "      --deb-codename        The debian codename (bullseye or focal) to build the docker image from. Default=focal"
  echo "      --no-upload           Do not upload the docker image to the registry"
  echo ""
  echo "Example: $0 --base-image-version 3.0.0-93e0279 --version mf-1 --network mainnet --deb-codename focal"
  exit 1
}

while [[ "$#" -gt 0 ]]; do case $1 in
  --no-upload) NOUPLOAD=1;;
  -b|--base-image-version) BASE_IMAGE_VERSION="$2"; shift;;
  -v|--version) VERSION="$2"; shift;;
  -n|--network) NETWORK="$2"; shift;;
  --deb-codename) DEB_CODENAME="$2"; shift;;
  *) echo "Unknown parameter passed: $1"; exit 1;;
esac; shift; done

# Verify Required Parameters are Present
if [[ -z "$BASE_IMAGE_VERSION" ]]; then usage "Base image version is not set!"; fi;
if [[ -z "$VERSION" ]]; then usage "Version is not set!"; fi;

DOCKERFILE_PATH="dockerfiles/Dockerfile.rosetta"

DOCKER_REGISTRY="gcr.io/o1labs-192920"
TAG="${DOCKER_REGISTRY}/${SERVICE}:${VERSION}${BUILD_FLAG_SUFFIX}"

# If the network is not specified, default to devnet
if [[ -z "$NETWORK" ]]; then
  NETWORK="devnet"
fi

# Set docker build build_mainnet arg
if [[ "$NETWORK" == "mainnet" ]]; then
  BUILD_NETWORK="--build-arg=\"build_mainnet=1\""
else
  BUILD_NETWORK="--build-arg=\"build_mainnet=\""
fi

# If the debian codename is not specified, default to focal
if [[ -z "$DEB_CODENAME" ]]; then
  DEB_CODENAME="focal"
fi

# Set docker build deb_codename arg
BUILD_DEB_CODENAME="--build-arg=\"deb_codename=${DEB_CODENAME}\""

# Set docker build base_image_version arg
BUILD_BASE_IMAGE_VERSION="--build-arg=\"base_image_version=${BASE_IMAGE_VERSION}\""

TAG="minafoundation/mina-rosetta:${BASE_IMAGE_VERSION}-${VERSION}-${DEB_CODENAME}"

docker build "$BUILD_NETWORK" "$BUILD_DEB_CODENAME" "$BUILD_BASE_IMAGE_VERSION" . -t "${TAG}" -f $DOCKERFILE_PATH

if [[ -z "$NOUPLOAD" ]] || [[ "$NOUPLOAD" -eq 0 ]]; then
  docker push "${TAG}"
fi
