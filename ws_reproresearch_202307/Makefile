### repo variables
PROJECT_USER=kaybenleroll
PROJECT_NAME=ws_reproresearch_202307
PROJECT_TAG=latest

TIDYVERSE_IMAGE_TAG=${PROJECT_USER}/${PROJECT_NAME}-tidyverse:${PROJECT_TAG}
ANACONDA_IMAGE_TAG=${PROJECT_USER}/${PROJECT_NAME}-anaconda:${PROJECT_TAG}

DOCKER_USER=rstudio
DOCKER_PASS=CHANGEME
DOCKER_UID=$(shell id -u)
DOCKER_GID=$(shell id -g)

RSTUDIO_PORT=8787

PROJECT_FOLDER=workshop


### Set GITHUB_USER with 'gh config set gh_user <<user>>'
GITHUB_USER=$(shell gh config get gh_user)

CONTAINER_NAME=repro-workshop

### Project build targets
.SUFFIXES: .qmd .html .dot .png

QMD_FILES  := $(wildcard *.qmd)
HTML_FILES := $(patsubst %.qmd,%.html,$(QMD_FILES))

all-html: $(HTML_FILES)

.qmd.html:
	echo "TIMESTAMP:" `date` "- Rendering script $<"  >> output.log 2>&1
	quarto render $< --to html >> output.log 2>&1
	echo "TIMESTAMP:" `date` "- Finished $*.html"         >> output.log 2>&1

.dot.png:
	dot -Tpng -o$*.png $<

full_deps.dot:
	makefile2graph all-html > full_deps.dot

depgraph: full_deps.png




clean-html:
	rm -fv *.html

clean-cache:
	rm -rfv *_cache
	rm -rfv *_files

clean-data:
	rm -rfv data/*.rds

clean-precompute:
	rm -rfv precompute/*.rds


mrproper: clean-html clean-cache clean-data clean-precompute
	rm data/*.xlsx


### Docker targets
docker-build-tidyverse-image: Dockerfile.tidyverse
	docker build -t ${TIDYVERSE_IMAGE_TAG} \
	  --build-arg BUILD_DATE=$(shell date -u +'%Y-%m-%dT%H:%M:%SZ') \
	  -f Dockerfile.tidyverse .


docker-build-anaconda-image: Dockerfile.anaconda
	docker build -t ${ANACONDA_IMAGE_TAG} \
	  --build-arg BUILD_DATE=$(shell date -u +'%Y-%m-%dT%H:%M:%SZ') \
	  -f Dockerfile.anaconda .



docker-run-tidyverse:
	docker run --rm -d \
	  -p ${RSTUDIO_PORT}:8787 \
	  -e USER=${DOCKER_USER} \
	  -e PASSWORD=${DOCKER_PASS} \
	  -e USERID=${DOCKER_UID} \
	  -e GROUPID=${DOCKER_GID} \
	  -v "${PWD}":"/home/${DOCKER_USER}/${PROJECT_FOLDER}":rw \
	  --name ${CONTAINER_NAME} \
	  ${TIDYVERSE_IMAGE_TAG}


docker-run-python:
	docker run --rm -i -t \
	  -v "${PWD}":"/opt/notebooks":rw \
	  -p 8888:8888 \
	  --name anaconda_workspace \
	  ${PYTHON_IMAGE_TAG} \
	  /bin/bash -c "\
	    jupyter notebook \
	    --notebook-dir=/opt/notebooks --ip='*' --port=8888 \
	    --no-browser --allow-root"




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
