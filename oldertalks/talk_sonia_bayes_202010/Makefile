### repo variables
PROJECT_USER=kaybenleroll
PROJECT_NAME=sonia_talk
PROJECT_TAG=latest

IMAGE_TAG=${PROJECT_USER}/${PROJECT_NAME}:${PROJECT_TAG}

DOCKER_USER=rstudio
DOCKER_UID=$(shell id -u)
DOCKER_GID=$(shell id -g)

RSTUDIO_PORT=8787

PROJECT_FOLDER=sonia_talk


### Set GITHUB_USER with 'gh config set gh_user <<user>>'
GITHUB_USER=$(shell gh config get gh_user)
#GITHUB_PROJECT="Kompreno D&O App"
#GITHUB_LABEL=stat-modelling
#GITHUB_MILESTONE=v1.1

CONTAINER_NAME=soniatalk



render-html: exploring_lifebook_data.Rmd talk_sonia_bayes_202010.Rmd
	Rscript -e 'rmarkdown::render("exploring_lifebook_data.Rmd")'
	Rscript -e 'rmarkdown::render("talk_sonia_bayes_202010.Rmd")'


create-slides-pdf:
	docker run --rm -t \
	  -v `pwd`:/slides \
	  astefanutti/decktape \
	  --size 1920x1080 \
	  /slides/talk_sonia_bayes_202010.html \
	  talk_sonia_bayes_202010.pdf


### Docker targets
docker-build-image: Dockerfile
	docker build -t ${IMAGE_TAG} -f Dockerfile .

docker-run:
	docker run --rm -d \
	  -p ${RSTUDIO_PORT}:8787 \
	  -e USER=${DOCKER_USER} \
	  -e PASSWORD=quickpass \
	  -e USERID=${DOCKER_UID} \
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
