### repo variables
PROJECT_USER=kaybenleroll
PROJECT_NAME=talk_idsc_randport_202406
PROJECT_TAG=latest

IMAGE_TAG=${PROJECT_USER}/${PROJECT_NAME}:${PROJECT_TAG}

DOCKER_USER=rstudio
DOCKER_PASS=CHANGEME
DOCKER_UID=$(shell id -u)
DOCKER_GID=$(shell id -g)
DOCKER_BUILD_ARGS=

RSTUDIO_PORT=8787

PROJECT_FOLDER=randport


### Set GITHUB_USER with 'gh config set gh_user <<user>>'
GITHUB_USER=$(shell gh config get gh_user)

CONTAINER_NAME=idsc-2024-talk

### Project build targets
.SUFFIXES: .qmd .html .dot .png

QMD_FILES  := $(wildcard *.qmd)
HTML_FILES := $(patsubst %.qmd,%.html,$(QMD_FILES))

all-html: $(HTML_FILES)

.qmd.html:
	echo "TIMESTAMP:" `date` "- Rendering script $<"  >> output.log 2>&1
	quarto render $< --to revealjs     >> output.log 2>&1
	echo "TIMESTAMP:" `date` "- Finished $*.html"     >> output.log 2>&1


.dot.png:
	dot -Tpng -o$*.png $<

full_deps.dot:
	makefile2graph all-html > full_deps.dot

depgraph: full_deps.png



clean-html:
	rm -fv $(HTML_FILES)


### Docker targets
docker-build-image: Dockerfile
	docker build -t ${IMAGE_TAG} \
	  ${DOCKER_BUILD_ARGS} \
	  --build-arg BUILD_DATE=$(shell date -u +'%Y-%m-%dT%H:%M:%SZ') \
	  -f Dockerfile . 2>&1 | tee -a docker_build.log

docker-run:
	docker run --rm -d \
	  -p ${RSTUDIO_PORT}:8787 \
	  -e USER=${DOCKER_USER} \
	  -e PASSWORD=${DOCKER_PASS} \
	  -e USERID=${DOCKER_UID} \
	  -e GROUPID=${DOCKER_GID} \
	  -v "${PWD}":"/home/${DOCKER_USER}/${PROJECT_FOLDER}":rw \
	  --name ${CONTAINER_NAME} \
	  ${IMAGE_TAG}

docker-bash:
	docker exec -it -u ${DOCKER_USER} ${CONTAINER_NAME} bash

docker-stop:
	docker stop ${CONTAINER_NAME}

docker-rm:
	docker rm ${CONTAINER_NAME}

docker-start:
	docker start ${CONTAINER_NAME}

docker-clean: docker-stop-all
	docker rm $(shell docker ps -q -a)

docker-pull:
	docker pull ${IMAGE_TAG}
