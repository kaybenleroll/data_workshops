### repo variables
PROJECT_USER=kaybenleroll
PROJECT_NAME=talk_bm_btydbayes_202211
PROJECT_TAG=latest

IMAGE_TAG=${PROJECT_USER}/${PROJECT_NAME}:${PROJECT_TAG}

DOCKER_USER=rstudio
DOCKER_PASS=CHANGEME
DOCKER_UID=$(shell id -u)
DOCKER_GID=$(shell id -g)

RSTUDIO_PORT=8787

PROJECT_FOLDER=btydtalk


### Set GITHUB_USER with 'gh config set gh_user <<user>>'
GITHUB_USER=$(shell gh config get gh_user)

CONTAINER_NAME=clv-workshop

### Project build targets
.SUFFIXES: .Rmd .qmd .html .dot .png

RMD_FILES  := $(wildcard *.Rmd)
QMD_FILES  := $(wildcard *.qmd)
HTML_FILES := $(patsubst %.Rmd,%.html,$(RMD_FILES))

all-html: $(HTML_FILES)

.Rmd.html:
	echo "TIMESTAMP:" `date` "- Rendering script $<" >> output.log 2>&1
	Rscript -e 'rmarkdown::render("$<")'             >> output.log 2>&1
	echo "TIMESTAMP:" `date` "- Finished"            >> output.log 2>&1

.qmd.html:
	echo "TIMESTAMP:" `date` "- Rendering script $<"  >> output.log 2>&1
	Rscript -e 'quarto::quarto_render("$<")'          >> output.log 2>&1
	echo "TIMESTAMP:" `date` "- Finished $*.html"     >> output.log 2>&1


.dot.png:
	dot -Tpng -o$*.png $<

full_deps.dot:
	makefile2graph all-html > full_deps.dot

depgraph: full_deps.png




mrproper: clean-cache clean-data clean-html clean-models
	rm -fv data/*.xlsx
	rm -fv *.dot
	rm -fv output.log

clean-data:
	rm -fv data/*.rds
	rm -fv data/*.csv

clean-html:
	rm -fv *.html

clean-cache:
	rm -rfv *_cache
	rm -rfv *_files

clean-models:
	rm -fv stan_models/*


### Docker targets
docker-build-image: Dockerfile
	docker build -t ${IMAGE_TAG} -f Dockerfile .

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
