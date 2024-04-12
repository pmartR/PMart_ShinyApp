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
PROFILE=local
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
