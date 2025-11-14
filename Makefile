BASE_DOCKER_FILE=Dockerfile-base
DOCKER_FILE=Dockerfile
BASE_VERSION=latest
TOP_VERSION=latest
TAG_LATEST=1
MAP_SHINYTEST=0
SECRET_PATH=.mysecret
APP_REGISTRY="code-registry.emsl.pnl.gov/multiomics-analyses/pmart_standalone"
BASE_IMAGE_TAG="${APP_REGISTRY}/base:${BASE_VERSION}"
IMAGE_TAG="${APP_REGISTRY}:${TOP_VERSION}"
MAP_SHINYTEST=2
PROFILE=dev
DO_BUILD=false

ifeq ($(DO_BUILD), true)
  BUILD_TARGET=build_top
endif

.PHONY: test
test: 
	Rscript -e "devtools::test()"

.PHONY: build_base
build_base:
	docker build . --secret id=access_tokens,src=${SECRET_PATH} \
	-f ${BASE_DOCKER_FILE} \
	-t ${BASE_IMAGE_TAG} \
	2>&1 | tee build_base.log \

.PHONY: build_top
build_top:
	docker build . \
	--build-arg base_tag=${BASE_IMAGE_TAG} \
	-f ${DOCKER_FILE} \
	-t ${IMAGE_TAG} \
	2>&1 | tee build_top.log \

.PHONY: build
build: build_base build_top

.PHONY: run
run: ${BUILD_TARGET}
	export TOP_VERSION=${TOP_VERSION} && \
	export BASE_VERSION=${BASE_VERSION} && \
	docker compose --profile ${PROFILE} up

.PHONY: stop
stop:
	docker compose --profile ${PROFILE} down
	
.PHONY: login
login:
	docker login code-registry.emsl.pnl.gov

.PHONY: push_base
push_base: login
	docker push ${BASE_IMAGE_TAG}

	@if [ ${TAG_LATEST} = 1 ]; then\
		docker tag ${BASE_IMAGE_TAG} "code-registry.emsl.pnl.gov/multiomics-analyses/pmart_standalone/base:latest";\
		docker push "code-registry.emsl.pnl.gov/multiomics-analyses/pmart_standalone/base:latest";\
    fi

.PHONY: push_top
push_top: login
	docker push ${IMAGE_TAG}

	@if [ ${TAG_LATEST} = 1 ]; then\
		docker tag ${IMAGE_TAG} "code-registry.emsl.pnl.gov/multiomics-analyses/pmart_standalone:latest";\
		docker push "code-registry.emsl.pnl.gov/multiomics-analyses/pmart_standalone:latest";\
	fi

.PHONY: push
push: push_base push_top

.PHONY: help
help:
	@echo "Makefile targets for pmart_standalone"
	@echo
	@echo "Usage: make <target> [VARIABLE=value]"
	@echo
	@echo "Targets:"
	@echo "  test         - Run package tests (uses Rscript)"
	@echo "  build_base   - Build base Docker image (uses BASE_DOCKER_FILE, BASE_VERSION)"
	@echo "  build_top    - Build top Docker image (uses DOCKER_FILE, BASE_VERSION, TOP_VERSION)"
	@echo "  build        - Run build_base and build_top"
	@echo "  run          - Run docker compose (uses PROFILE, TOP_VERSION, BASE_VERSION)"
	@echo "  stop         - Stop docker compose (uses PROFILE)"
	@echo "  login        - Login to registry code-registry.emsl.pnl.gov"
	@echo "  push_base    - Push base image (uses BASE_VERSION, TAG_LATEST)"
	@echo "  push_top     - Push top image (uses TOP_VERSION, TAG_LATEST)"
	@echo "  push         - push_base and push_top"
	@echo
	@echo "Environment variables / Makefile variables (with defaults):"
	@echo "  BASE_DOCKER_FILE = $(BASE_DOCKER_FILE)" 
	@echo "  DOCKER_FILE      = $(DOCKER_FILE)"
	@echo "  BASE_VERSION     = $(BASE_VERSION)"
	@echo "  TOP_VERSION      = $(TOP_VERSION)"
	@echo "  TAG_LATEST       = $(TAG_LATEST)"
	@echo "  MAP_SHINYTEST    = $(MAP_SHINYTEST)"
	@echo "  SECRET_PATH      = $(SECRET_PATH)"
	@echo "  APP_REGISTRY     = $(APP_REGISTRY)"
	@echo "  BASE_IMAGE_TAG   = $(BASE_IMAGE_TAG)"
	@echo "  IMAGE_TAG        = $(IMAGE_TAG)"
	@echo "  PROFILE          = $(PROFILE)"
	@echo "  DO_BUILD         = $(DO_BUILD)"
	@echo
	@echo "Examples:"
	@echo "  make build                            # build both images"
	@echo "  make build_base BASE_VERSION=1.2.3    # build base with different version"
	@echo "  make run PROFILE=dev                  # run compose with dev profile"
	@echo
	@echo "Secrets:"
	@echo "  build_base and build_top expect secrets mounted for credentialed remotes:"
	@echo "    --mount=type=secret,id=gitlab_pat"
	@echo "    --mount=type=secret,id=access_tokens"
	@echo
	@true
